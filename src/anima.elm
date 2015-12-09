module Anima where

import Space exposing (..)
import GenAnim as GA
import Physics exposing (..)
import Filters exposing (Filter)
import Automaton as Auto exposing ((>>>))
import Focus
import Color
import Debug exposing (..)
import Time
import Window

type alias TimeStep = GA.TimeStep
type alias Space space = Space.Space space
type alias Force space = Physics.Force space
type alias PhasePos space = Physics.PhasePos space
type alias Particle space = Physics.Particle space
type alias Filter space = Filters.Filter space
type alias AnimUpdater state input output = GA.AnimUpdater state input output
type alias Animation input output = GA.Animation input output

samplingInterval = Filters.samplingInterval

-- Simple animations

timeKeeper : Float -> Auto.Automaton Float Float
timeKeeper t0 = Auto.state t0 (+)

animateWith = Auto.hiddenState

anim = GA.anim f1d
anim2D = GA.anim f2d
anim3D = GA.anim f3d
animColor = GA.anim fcolor

konst = GA.konst

handoff = GA.handoff f1d
handoff2D = GA.handoff f2d
handoff3D = GA.handoff f3d
handoffColor = GA.handoff fcolor

type alias Time = Float

-- Filters are expressed as automatons with some hidden state.

run = Auto.run

springy = Filters.springy f1d
springy2D = Filters.springy f2d
springyColor = Filters.springy fcolor

laggy : Float -> Float -> Filter1D
laggy = Filters.laggy f1d

laggy2D : Float -> (Float, Float) -> Filter2D
laggy2D = Filters.laggy f2d

laggyColor : Float -> Color.Color -> FilterColor
laggyColor = Filters.laggy fcolor

type alias Filter1D = Filter Float
type alias Filter2D = Filter (Float,Float)
type alias Filter3D = Filter (Float,Float,Float)
type alias FilterColor = Filter Color.Color

lerp = Filters.lerp
lerp1D = f1d.lerp
lerp2D = f2d.lerp
lerpColor = fcolor.lerp

linearFollower = Filters.linearFollower f1d
linearFollower2D = Filters.linearFollower f2d

biquad : Filters.BiQuadSpec -> Filters.SecondOrderFilter Float
biquad = Filters.biquad f1d
biquad2D = Filters.biquad f2d

lpf f0 q = Filters.lpf f1d f0 q
lpf2D f0 q = Filters.lpf f2d f0 q

particle = Physics.particle f1d
particle2D = Physics.particle f2d

-- Simple application framework

type alias App input model direction viewstate output =
    { modeller : Auto.Automaton input model
    , director : Auto.Automaton (input, model) direction
    , animator : Animation direction viewstate
    , viewer : Auto.Automaton (model, viewstate) output
    , timeStep : input -> Maybe TimeStep
    , initial :
        { model : model
        , view  : output
        , viewstate : viewstate
        }
    }

type CommonEvent = TimeStep Float | ResizeWindow Int Int
type Event a = EnvEvent CommonEvent | UserEvent a

type alias Env = {dt : TimeStep, time : Float, width: Int, height: Int}
type alias WithEnv base = {env: Env, data: base}

env0 : a -> WithEnv a
env0 a = {data = a, env = {dt = 0.0, time = 0.0, width = 640, height = 480}}

withWindowSize : Int -> Int -> WithEnv a -> WithEnv a
withWindowSize w h rec = let env = rec.env 
                         in {rec | env = {env | width = w, height = h}}

withTimeStep dt rec = let env = rec.env
                 in {rec | env = {env | dt = dt, time = env.time + dt}}
                    
timeStep : Event a -> Maybe TimeStep
timeStep e = case e of
    EnvEvent (TimeStep dt) -> Just dt
    _ -> Nothing
    
animationNeeded = Signal.constant True
frames = Time.fpsWhen 60 animationNeeded
windowResizeEvents = Signal.map (\(w,h) -> EnvEvent (ResizeWindow w h)) Window.dimensions
timeStepEvents = Signal.map (\dt -> EnvEvent (TimeStep (Time.inSeconds dt))) frames

env : Signal Env
env = Signal.map2 (\(w,h) dt -> {dt = Time.inSeconds dt, time = 0.0, width = w, height = h}) Window.dimensions frames
            
type alias OpinionatedApp input model direction viewstate output =
    { modeller : input -> model -> model
    , director : (input, model) -> (WithEnv direction) -> (WithEnv direction)
    , animator : Animation (WithEnv direction) (WithEnv viewstate)
    , viewer : (model, WithEnv viewstate) -> output
    , initial :
        { model : model
        , view : output
        , direction : direction
        , viewstate : viewstate
        }
    }

convertModeller : (input -> model -> model) -> (Event input -> model -> model)
convertModeller modeller =
    \ einput model ->
        case einput of
            UserEvent ue -> modeller ue model
            _ -> model

convertDirector : 
    ((input,model) -> WithEnv director -> WithEnv director) 
    -> ((Event input, model) -> WithEnv director -> (WithEnv director, WithEnv director))
convertDirector director =
    \(input,model) dir -> 
        let dir1 = case input of
                        EnvEvent (ResizeWindow w h) ->
                            withWindowSize w h dir
                        EnvEvent (TimeStep dt) ->
                            withTimeStep dt dir
                        _ ->
                            dir
            dir2 = case input of
                        UserEvent ue ->
                            director (ue,model) dir1
                        _ ->
                            dir1
        in
           (dir2,dir2)

mergeEnvironmentChanges : Signal input -> Signal (Event input)
mergeEnvironmentChanges sig = Signal.mergeMany [windowResizeEvents, timeStepEvents, Signal.map UserEvent sig]

appify : OpinionatedApp input model direction viewstate output 
            -> App (Event input) model (WithEnv direction) (WithEnv viewstate) output
appify app =
    { modeller = procWithUpdater (convertModeller app.modeller) app.initial.model
    , director = animateWith (env0 app.initial.direction) (convertDirector app.director)
    , animator = app.animator
    , viewer = Auto.pure app.viewer
    , timeStep = timeStep
    , initial = { model = app.initial.model
                , view = app.initial.view
                , viewstate = env0 app.initial.viewstate
                }
    }

runApp : App (Event input) model direction viewstate output -> Signal input -> Signal output
runApp app userInput =
    let
        input = mergeEnvironmentChanges userInput
        modelSig = Auto.run app.modeller app.initial.model input
        viewStateSig = Auto.run (controllerProc app.timeStep app.director (app.animator |> dropTimeStep) app.initial.viewstate) app.initial.viewstate (Signal.map2 (,) input modelSig)
        viewSig = Auto.run app.viewer app.initial.view (Signal.map2 (,) modelSig viewStateSig)
    in
       viewSig

runOpinionatedApp : OpinionatedApp input model direction viewstate output -> Signal input -> Signal output
runOpinionatedApp app userInput =
    runApp (appify app) userInput

procWithUpdater : (input -> model -> model) -> model -> Auto.Automaton input model
procWithUpdater updater initialModel =
    animateWith initialModel
        (\input model ->
          let
              m2 = updater input model
          in
              (m2,m2))

controllerProc : (input -> Maybe TimeStep)
                    -> Auto.Automaton (input, model) direction
                    -> Auto.Automaton (TimeStep, direction) viewstate
                    -> viewstate
                    -> Auto.Automaton (input, model) viewstate
controllerProc timeStep director animator vs0 =
    animateWith (director, animator, vs0)
        (\(input,model) (director, animator, vs) ->
            let (director2,dir) = Auto.step (input,model) director
            in
               case timeStep input of
                   Just dt ->
                       let (animator2,vs2) = Auto.step (dt,dir) animator
                       in (vs2,(director2,animator2,vs2))
                   Nothing ->
                       (vs,(director2,animator,vs)))



-- helpers

-- Make a pure animation record transformer
pureData : (recIn -> recOut) -> Animation (WithEnv recIn) (WithEnv recOut)
pureData f = Auto.pure (\(dt,a) -> (dt, {env = a.env, data = f a.data}))

pure : (recIn -> recOut) -> Animation recIn recOut
pure f = Auto.pure (\(dt,a) -> (dt, f a))

-- Defining a property animation like this permits us to chain
-- animations of different record properties in order to combine
-- their outputs.
type alias PropertyAnim recIn recOut = Animation (recIn,recOut) (recIn,recOut)

animProp : Focus.Focus recIn dir
                -> Focus.Focus recOut space
                -> Animation dir space
                -> PropertyAnim recIn recOut
animProp focIn focOut anim =
    animateWith anim
        (\(dt, (recIn,recOut)) anim ->
            let dir1 = Focus.get focIn recIn
                (anim2,(_,space1)) = Auto.step (dt,dir1) anim
            in ((dt,(recIn,Focus.set focOut space1 recOut)), anim2))

animPropInline : Focus.Focus rec dir
                    -> Animation dir dir
                    -> PropertyAnim rec rec
animPropInline foc anim =
    animateWith anim
        (\(dt, (rec, _)) anim ->
            let dir1 = Focus.get foc rec
                (anim2, (_, space1)) = Auto.step (dt, dir1) anim
                rec2 = Focus.set foc space1 rec
            in ((dt, (rec2, rec2)), anim2))

copyProp : Focus.Focus recIn x
                -> Focus.Focus recOut x
                -> PropertyAnim recIn recOut
copyProp focIn focOut =
    Auto.pure (\(dt,(recIn,recOut)) ->
        (dt,(recIn, Focus.set focOut (Focus.get focIn recIn) recOut)))

dropTimeStep anim =
    anim >>> Auto.pure (\(_,x) -> x)

data_ = Focus.create .data (\f rec -> {rec | data = f rec.data})
x_ = Focus.create .x (\f rec -> {rec | x = f rec.x})
y_ = Focus.create .y (\f rec -> {rec | y = f rec.y})

filterProp : Focus.Focus rec space -> Filter space -> Filter rec
filterProp focus anim =
    animateWith anim
        (\(dt, recIn) anim ->
            let dir1 = Focus.get focus recIn
                (anim2,(_,space1)) = Auto.step (dt,dir1) anim
            in ((dt,Focus.set focus space1 recIn), anim2))

animateRec : recOut -> List (PropertyAnim recIn recOut) -> Animation recIn recOut
animateRec recOut anims =
    let inFilter = Auto.pure (\(dt,i) -> (dt,(i,recOut)))
        outFilter = Auto.pure (\(dt,(_,o)) -> (dt,o))
    in
       inFilter >>> List.foldl (>>>) outFilter anims

