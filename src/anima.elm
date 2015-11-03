module Anima where

import Space exposing (..)
import GenAnim as GA
import Physics exposing (..)
import Filters exposing (Filter)
import Automaton as Auto exposing ((>>>))
import Focus
import Color
import Debug exposing (..)

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
    { modelProc : Auto.Automaton input model
    , director : Auto.Automaton (input, model) direction
    , animator : Animation direction viewstate
    , viewProc : Auto.Automaton (model, viewstate) output
    , timeStep : input -> Maybe TimeStep
    , initial :
        { model : model
        , view  : output
        , viewstate : viewstate
        }
    }

type alias OpinionatedApp input model direction viewstate output =
    { modelProc : input -> model -> model
    , director : (input, model) -> direction -> direction
    , animator : Animation direction viewstate
    , viewProc : (model, viewstate) -> output
    , timeStep : input -> Maybe TimeStep
    , initial :
        { model : model
        , view : output
        , direction : direction
        , viewstate : viewstate
        }
    }

appify : OpinionatedApp input model direction viewstate output 
            -> App input model direction viewstate output
appify app =
    { modelProc = procWithUpdater app.modelProc app.initial.model
    , director = animateWith app.initial.direction 
                    (\(input, model) dir -> 
                        let dir2 = app.director (input, model) dir
                        in (dir2,dir2))
    , animator = app.animator
    , viewProc = Auto.pure app.viewProc
    , timeStep = app.timeStep
    , initial = { model = app.initial.model
                , view = app.initial.view
                , viewstate = app.initial.viewstate
                }
    }

runApp : App input model direction viewstate output -> Signal input -> Signal output
runApp app input =
    let
        modelSig = Auto.run app.modelProc app.initial.model input
        viewStateSig = Auto.run (controllerProc app.timeStep app.director (app.animator |> dropTimeStep) app.initial.viewstate) app.initial.viewstate (Signal.map2 (,) input modelSig)
        viewSig = Auto.run app.viewProc app.initial.view (Signal.map2 (,) modelSig viewStateSig)
    in
       viewSig

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

-- Definiting a property animation like this permits us to chain
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

copyProp : Focus.Focus recIn x
                -> Focus.Focus recOut x
                -> PropertyAnim recIn recOut
copyProp focIn focOut =
    Auto.pure (\(dt,(recIn,recOut)) ->
        (dt,(recIn, Focus.set focOut (Focus.get focIn recIn) recOut)))

dropTimeStep anim =
    anim >>> Auto.pure (\(_,x) -> x)

x_ = Focus.create .x (\f rec -> {rec | x <- f rec.x})
y_ = Focus.create .y (\f rec -> {rec | y <- f rec.y})

filterProp : Focus.Focus rec space -> Filter space -> Filter rec
filterProp focus anim =
    animateWith anim
        (\(dt, recIn) anim ->
            let dir1 = Focus.get focus recIn
                (anim2,(_,space1)) = Auto.step (dt,dir1) anim
            in ((dt,Focus.set focus space1 recIn), anim2))

animateRec : recOut -> List (Animation (recIn,recOut) (recIn,recOut)) -> Animation recIn recOut
animateRec recOut anims =
    let inFilter = Auto.pure (\(dt,i) -> (dt,(i,recOut)))
        outFilter = Auto.pure (\(dt,(_,o)) -> (dt,o))
    in
       inFilter >>> List.foldl (>>>) outFilter anims
