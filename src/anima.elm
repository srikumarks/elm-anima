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
import Mouse
import Dict exposing (Dict)
import Task

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
    , director : Auto.Automaton (input, model) (WithEnv direction)
    , animator : Animation (WithEnv direction) (WithEnv viewstate)
    , viewer : Auto.Automaton (model, WithEnv viewstate) output
    , timeStep : input -> Maybe TimeStep
    , initial :
        { model : model
        , view  : output
        , viewstate : WithEnv viewstate
        }
    }

type CommonEvent = TimeStep Float | MousePos Int Int
type EventU a = EnvEvent CommonEvent | UserEvent a
type alias Event a = ((Int,Int), EventU a) -- Always pick the current window dimensions.
                                           -- The "resize window" event is no longer a part
                                           -- of CommonEvent. If you want to respond to window
                                           -- dimension changes, present it to the system as
                                           -- a user event.

type alias Env = { dt : TimeStep
                 , time : Float
                 , width: Int
                 , height: Int
                 , mousePos : Point2D
                 , tasks : List (Task.Task () ()) -- Not really the right place for this,
                                                  -- but will do for now.
                 }
type alias WithEnv base = { env : Env, data : base }

env0 : a -> WithEnv a
env0 a = {data = a, env = {dt = 0.0, time = 0.0, width = 640, height = 480, mousePos = (320,240), tasks = []}}

withWindowSize : Int -> Int -> WithEnv a -> WithEnv a
withWindowSize w h rec = let env = rec.env 
                         in {rec | env = {env | width = w, height = h}}

withTimeStep : Float -> WithEnv a -> WithEnv a
withTimeStep dt rec = let env = rec.env
                      in {rec | env = {env | dt = dt, time = env.time + dt}}

withMousePos : Point2D -> WithEnv a -> WithEnv a
withMousePos pos rec = let env = rec.env
                       in {rec | env = {env | mousePos = pos}}
                    
timeStep : Event a -> Maybe TimeStep
timeStep (_,e) = case e of
    EnvEvent (TimeStep dt) -> Just dt
    _ -> Nothing
    
animationNeeded = Signal.constant True
frames = Time.fpsWhen 60 animationNeeded
timeStepEvents = Signal.map (\dt -> EnvEvent (TimeStep (Time.inSeconds dt))) frames
mousePositionEvents = Signal.map (\(x,y) -> EnvEvent (MousePos x y)) Mouse.position

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
            (_,UserEvent ue) -> modeller ue model
            _ -> model

convertDirector : 
    ((input,model) -> WithEnv director -> WithEnv director) 
    -> ((Event input, model) -> WithEnv director -> (WithEnv director, WithEnv director))
convertDirector director =  \(((w,h),input),model) dir -> 
        let dir0 = withTimeStep 0.0 dir |> withWindowSize w h
            dir1 = case input of
                        EnvEvent (TimeStep dt) ->
                            withTimeStep dt dir0
                        EnvEvent (MousePos x y) ->
                            withMousePos (toFloat x, toFloat y) dir0
                        _ ->
                            dir0
            dir2 = case input of
                        UserEvent ue ->
                            director (ue,model) dir1
                        _ ->
                            clearTasks dir1
        in
           (dir2, dir2)

mergeEnvironmentChanges : Signal input -> Signal (Event input)
mergeEnvironmentChanges sig = Signal.map2 (,)
                                Window.dimensions
                                (Signal.mergeMany 
                                    [ timeStepEvents
                                    , mousePositionEvents
                                    , Signal.map UserEvent sig
                                    ])

appify : OpinionatedApp input model direction viewstate output 
            -> App (Event input) model direction viewstate output
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

runApp : App (Event input) model direction viewstate output 
            -> Signal input 
            -> (Signal output, Signal (Task.Task () ()))
runApp app userInput =
    let input = mergeEnvironmentChanges userInput
        controllerr = controllerProc app.timeStep app.director (app.animator |> dropTimeStep) app.initial.viewstate
        combined = Auto.hiddenState (app, controllerr)
                        (\input (app, controller) ->
                            let (modeller', model) = Auto.step input app.modeller
                                (controller', viewstate) = Auto.step (input, model) controller
                                (viewer', view) = Auto.step (model, viewstate) app.viewer
                            in
                               ((view, viewstate.env.tasks), ({ app | modeller = modeller', viewer = viewer' }, controller')))
        viewAndTasksSig = Auto.run combined (app.initial.view, []) input
    in
       ( viewAndTasksSig |> Signal.map (\(a,_) -> a)
       , viewAndTasksSig |> Signal.map (\(_,a) -> a)
                         |> Signal.filterMap (\tasks -> if List.length tasks > 0 then
                                                        Just (Task.sequence tasks `Task.andThen` (\x -> Task.succeed ()))
                                                     else
                                                        Nothing)
                                        (Task.succeed ())
       )

runOpinionatedApp : OpinionatedApp input model direction viewstate output 
                        -> Signal input 
                        -> (Signal output, Signal (Task.Task () ()))
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
                    -> Auto.Automaton (input, model) (WithEnv direction)
                    -> Auto.Automaton (TimeStep, (WithEnv direction)) (WithEnv viewstate)
                    -> (WithEnv viewstate)
                    -> Auto.Automaton (input, model) (WithEnv viewstate)
controllerProc timeStep director animator vs0 =
    animateWith (director, animator, vs0)
        (\(input,model) (director, animator, vs) ->
            let (director2, dir) = Auto.step (input, model) director
                tasks = dir.env.tasks
            in
               case timeStep input of
                   Just dt ->
                       let (animator2, vs2) = Auto.step (dt, dir) animator
                       in (copyTasks dir vs2, (director2, animator2, vs2))
                   Nothing ->
                       (copyTasks dir vs, (director2, animator, vs)))

clearTasks : WithEnv a -> WithEnv a
clearTasks dir = let env = dir.env
                 in { dir | env = { env | tasks = [] } }

copyTasks from to =
    let env = to.env
    in { to | env = { env | tasks = from.env.tasks } }
                    
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


{-| Drag-and-drop helper. The picker automaton tracks pickup/drop actions and 
outputs a force to be applied to a particle being dragged, if one is being
dragged. It doesn't make any assumptions about what to do upon these actions,
but only concerns itself with directing the object being dragged.

The picker automaton is a singleton. It needs to be wired up to appropriate 
input in order for it to function.
-}
type PickerAction = PickupItem String | MoveItem | DropItem
type alias Picker space = Auto.Automaton (space, Maybe PickerAction) (Maybe (String, space, Bool))
picker : Picker Point2D
picker = 
    let automaton = Auto.hiddenState Nothing updater
        noForce = f2d.zero
        updater (pos, action) state =
            case action of
                Just (PickupItem key) ->
                    case state of
                        Just _ -> 
                            -- Already moving something. Ignore this pickup.
                            updater (pos, Just MoveItem) state
                            
                        Nothing -> 
                            -- Start drag operation
                            (Just (key, noForce, True), Just (key, pos))

                Just MoveItem ->
                    case state of
                        Just (key, startPos) -> 
                            -- Valid drag state
                            (Just (key, f2d.sub pos startPos, True), state)

                        Nothing -> 
                            -- Invalid drag state. Ignore it.
                            (Nothing, state)

                Just DropItem ->
                    case state of
                        Just (key, startPos) ->
                            (Just (key, f2d.sub pos startPos, False), Nothing)

                        Nothing ->
                            (Nothing, state)

                Nothing ->
                    updater (pos, Just MoveItem) state
                    
    in automaton


logMaybe str maybe = case maybe of
    Just (PickupItem key) -> Just (Debug.log str (PickupItem key))
    Just DropItem -> Just (Debug.log str DropItem)
    Just MoveItem -> Just MoveItem
    Nothing -> Nothing
    
-- When objects are identified by string keys, you can direct forces
-- at them using this kind of dictionary.
type alias DirectedForces space = Dict String (List (Force space))

-- A particle collection is a system that takes directed forces
-- and applies them to a collection of labelled particles.
type alias ParticleColl space a = Animation (DirectedForces space) (Dict String a)

{-| Makes a particle collection. 

The "field" helps get and set the relevant space property.
The "items" are the things constituting the particle system.
The "particles" are the processes that capture the particle behaviour.

The result is a "particle collection" on which you can direct
forces to particles identified by string keys.

-}
particleColl : 
    Focus.Focus a space
    -> Dict String a
    -> Dict String (Particle space) 
    -> ParticleColl space a
particleColl field items particles = 
    Auto.hiddenState (items, particles)
        (\(dt, directedForces) (itemsIn, particlesIn) ->
            let (pos', particles') = 
                    Dict.foldl distributeForces (itemsIn, particlesIn) directedForces
                    
                distributeForces key forces (items, particles) =
                    case Dict.get key particles of
                        Just p ->
                            let (p', (_, (pos', _))) = 
                                    Auto.step (dt, forces) p

                                nextItems = 
                                    Dict.update key updatePos items

                                updatePos mPrevPos =
                                    case mPrevPos of
                                        Just prevPos ->
                                            Just (Focus.set field pos' prevPos)
                                        Nothing ->
                                            Nothing
                            in 
                               (nextItems, Dict.insert key p' particles)

                        Nothing ->
                            (items, particles)
            in 
               ((dt, pos'), (pos', particles')))


{-
{-| Makes an animation that animates a collection of particles
that are part of the application. The picker is used to pick
one particle of the given particle collection.

The "picker" is the global picker, or you can make your own.
"setItems" modifies the direction with the given information.
"tracker" gives the position along with a possible picker action to act on.
"forces" provides the forces on the system as computed from the direction.
"particleColl" is the collection of particles from which you want
the picker to be able to pick one.

Note: The "setItems" and "tracker" functions could fit in some kind
of a type class.

The result is an animation of the "direction" structure that tracks
the application of the picker, generating events along the way.
-}
applyPicker :
    Picker Point2D
    -> (Maybe PickerAction -> Dict String a -> dir -> dir)
    -> (dir -> (Point2D, Maybe PickerAction))
    -> Animation dir (DirectedForces Point2D)
    -> ParticleColl Point2D a
    -> Animation dir dir
applyPicker picker setItems tracker forces particleColl =
    let automaton = Auto.hiddenState (picker, forces, particleColl) update
        update (dt, dir) (picker, forces, particleColl) =
            let (picker', (_, (mDelta, pickerAction))) = Auto.step (dt, tracker dir) picker
                (forces', (_, dirForces)) = Auto.step (dt, dir) forces
                dirForces' = case mDelta of
                    Just (key, delta) ->
                        Dict.update key (\v -> case v of
                                                Just f -> Just (f ++ [Drag delta])
                                                Nothing -> Nothing)
                                dirForces
                    Nothing ->
                        dirForces
                (particleColl', (_, result)) = Auto.step (dt, dirForces') particleColl
            in
               ((dt, setItems pickerAction result dir), (picker', forces', particleColl'))
    in
       automaton
          -}  

dictItem =
    let getPos default k dict =
            Maybe.withDefault default (Dict.get k dict)
        setPos k updater dict =
            Dict.update k (Maybe.map updater) dict
        fn default k =
            Focus.create (getPos default k) (setPos k)
    in
       fn
