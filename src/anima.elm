module Anima
    (
    Time, TimeStep, Animation, AnimUpdater, Filter, Filter1D, Filter2D, Filter3D, FilterColor,
    f1d,f2d,f3d,fcolor,
    run, samplingInterval, timeKeeper, animateWith,
    anim, anim2D, anim3D, animColor,
    konst,
    handoff, handoff2D, handoff3D, handoffColor,
    repeat, warp,
    biquad, lpf,
    biquad2D, lpf2D,
    springy, laggy, linearFollower,
    springy2D, laggy2D, linearFollower2D,
    springyColor, laggyColor,
    Particle, PhasePos, ParticleState, Force, ForceFn,
    particle, particle2D,
    App, OpinionatedApp, appify, runApp, animProp, filterProp, animateRec, dropTimeStep, x_, y_, procWithUpdater
    )
    where

import Automaton as Auto exposing ((>>>))
import Focus
import Color
import Debug exposing (..)

type alias TimeStep = Float
type alias Space a =
    { zero : a
    , add : a -> a -> a
    , sub : a -> a -> a
    , scale : Float -> a -> a
    , madd : Float -> a -> a -> a
    , lsum : Float -> a -> Float -> a -> a
    , lerp : Float -> a -> a -> a
    , dot : a -> a -> Float
    , dir : a -> a
    , abs : a -> Float
    , norm : a -> Float
    , dist : a -> a -> Float
    }

type alias Space1D = Space Float
type alias Space2D = Space (Float, Float)
type alias Space3D = Space (Float, Float, Float)
type alias SpaceColor = Space Color.Color

f1d : Space1D
f1d =
    { zero = 0.0
    , add = (+)
    , sub = (-)
    , scale = (*)
    , madd f x1 x2 = f * x1 + x2
    , lsum f1 x1 f2 x2 = f1 * x1 + f2 * x2
    , lerp f x1 x2 = x1 + f * (x2 - x1)
    , dot = (*)
    , dir a = if a > 0.0 then 1.0 else if a < 0.0 then -1.0 else 0.0
    , abs = abs
    , norm x = x * x
    , dist p1 p2 = f1d.abs (f1d.sub p1 p2)
    }

f2d : Space2D
f2d =
    { zero = (0.0, 0.0)
    , add (x1,y1) (x2,y2) = (x1+x2,y1+y2)
    , sub (x1,y1) (x2,y2) = (x1-x2,y1-y2)
    , scale f (x,y) = (f * x, f * y)
    , madd f (x1,y1) (x2,y2) = (f * x1 + x2, f * y1 + y2)
    , lsum f1 (x1,y1) f2 (x2,y2) = (f1 * x1 + f2 * x2, f1 * y1 + f2 * y2)
    , lerp f p1 p2 = f2d.lsum (1.0-f) p1 f p2
    , dot (x1,y1) (x2,y2) = x1 * x2 + y1 * y2
    , dir p = f2d.scale (1.0 / f2d.abs p) p
    , abs p = sqrt (f2d.norm p)
    , norm (x,y) = x * x + y * y
    , dist p1 p2 = f2d.abs (f2d.sub p1 p2)
    }

f3d : Space3D
f3d =
    { zero = (0.0, 0.0, 0.0)
    , add (x1,y1,z1) (x2,y2,z2) = (x1+x2,y1+y2,z1+z2)
    , sub (x1,y1,z1) (x2,y2,z2) = (x1-x2,y1-y2,z1-z2)
    , scale f (x,y,z) = (f * x, f * y, f * z)
    , madd f (x1,y1,z1) (x2,y2,z2) = (f * x1 + x2, f * y1 + y2, f * z1 + z2)
    , lsum f1 (x1,y1,z1) f2 (x2,y2,z2) = (f1 * x1 + f2 * x2, f1 * y1 + f2 * y2, f1 * z1 + f2 * z2)
    , lerp f p1 p2 = f3d.lsum (1.0-f) p1 f p2
    , dot (x1,y1,z1) (x2,y2,z2) = x1 * x2 + y1 * y2 + z1 * z2
    , dir p = f3d.scale (1.0 / f3d.abs p) p
    , abs p = sqrt (f3d.norm p)
    , norm (x,y,z) = x * x + y * y + z * z
    , dist p1 p2 = f3d.abs (f3d.sub p1 p2)
    }

fcolor : SpaceColor
fcolor =
    { zero = Color.black
    , add c1 c2 = let (rgb1,rgb2) = (Color.toRgb c1, Color.toRgb c2)
                      alpha = rgb1.alpha + rgb2.alpha
                  in Color.rgba
                        (floor ((toFloat rgb1.red * rgb1.alpha + toFloat rgb2.red * rgb2.alpha) / alpha))
                        (floor ((toFloat rgb1.green * rgb1.alpha + toFloat rgb2.green * rgb2.alpha) / alpha))
                        (floor ((toFloat rgb1.blue * rgb1.alpha + toFloat rgb2.blue * rgb2.alpha) / alpha))
                        (alpha)
    , sub c1 c2 = let (rgb1,rgb2) = (Color.toRgb c1, Color.toRgb c2)
                      alpha = rgb1.alpha + rgb2.alpha
                  in Color.rgba
                        (floor ((toFloat rgb1.red * rgb1.alpha - toFloat rgb2.red * rgb2.alpha) / alpha))
                        (floor ((toFloat rgb1.green * rgb1.alpha - toFloat rgb2.green * rgb2.alpha) / alpha))
                        (floor ((toFloat rgb1.blue * rgb1.alpha - toFloat rgb2.blue * rgb2.alpha) / alpha))
                        (alpha)
    , scale f c = let rgba = Color.toRgb c
                  in Color.rgba rgba.red rgba.green rgba.blue (f * rgba.alpha)
    , madd f c1 c2 = fcolor.add c2 (fcolor.scale f c1)
    , lsum f1 c1 f2 c2 = fcolor.add (fcolor.scale f1 c1) (fcolor.scale f2 c2)
    , lerp f p1 p2 = fcolor.lsum (1.0-f) p1 f p2
    , dot c1 c2 = let (rgb1,rgb2) = (Color.toRgb c1, Color.toRgb c2)
                  in sqrt (toFloat (rgb1.red * rgb2.red + rgb1.green * rgb2.green + rgb1.blue * rgb2.blue))
    , dir p = fcolor.scale (1.0 / fcolor.abs p) p
    , abs p = sqrt (fcolor.norm p)
    , norm c = let rgb = Color.toRgb c
               in toFloat (rgb.red * rgb.red + rgb.green * rgb.green + rgb.blue * rgb.blue)
    , dist p1 p2 = fcolor.abs (fcolor.sub p1 p2)
    }


-- Simple animations

samplingInterval = 1.0 / 60.0
timeKeeper : Float -> Auto.Automaton Float Float
timeKeeper t0 = Auto.state t0 (+)

type alias Animation input output = Auto.Automaton (TimeStep, input) (TimeStep, output)
type alias AnimUpdater state input output = (TimeStep, input) -> state -> ((TimeStep, output), state)
type alias Filter space = Animation space space
type alias Filter1D = Filter Float
type alias Filter2D = Filter (Float,Float)
type alias Filter3D = Filter (Float,Float,Float)
type alias FilterColor = Filter Color.Color

animateWith = Auto.hiddenState

anim = anim_s f1d
anim2D = anim_s f2d
anim3D = anim_s f3d
animColor = anim_s fcolor

konst : s -> Animation i s
konst val = Auto.pure (\(dt,_) -> (dt,val))

anim_s : Space s -> Float -> s -> s -> (Float -> Float) -> Animation i s
anim_s s dur fromVal toVal ease =
    animateWith 0.0
        (\(dt,_) t ->
            let f = t / dur
            in ((dt, s.lerp (ease f) fromVal toVal), t + dt))


type Handoff i o = Before Float (Animation i o) (Animation i o) | After (Animation i o)

handoff = handoff_s f1d
handoff2D = handoff_s f2d
handoff3D = handoff_s f3d
handoffColor = handoff_s fcolor

-- A "handoff" runs the first animation over a period of "time",
-- and overlaps the second animation a bit before handing off
-- the motion to it. During the overlap, the input is cross
-- faded using the unit shape function (easing function).
handoff_s : Space s -> Time -> Animation i s -> Animation i s -> Animation i s
handoff_s space time animBefore animAfter =
    animateWith (Before 0.0 animBefore animAfter)
        (\(dt,x) s ->
            case s of
                Before t ab aa ->
                    let (ab2,xa2) = Auto.step (dt,x) (if t < time then ab else aa)
                    in (xa2, if t < time then Before (t+dt) ab2 aa else After aa)
                After aa ->
                    let (aa2,xa2) = Auto.step (dt,x) aa
                    in (xa2, After aa2))

type alias Time = Float
type RepeatState i space
    = Attack Time (Animation i space)
    | Sustain Time Int (Animation i space) (Animation i space)
    | Release (Animation i space)

repeat : Int -> (Time,Time) -> Animation i space -> Animation i space
repeat count (t1,t2) anim =
    animateWith (Attack 0.0 anim)
        (\(dt,x) s ->
            case s of
                Attack t a ->
                    let (a2,x2) = Auto.step (dt,x) a
                        tdt = t + dt
                    in
                       if | tdt < t1 -> (x2, Attack tdt a2)
                          | otherwise -> (x2, Sustain tdt 1 a a2)
                Sustain t n start a ->
                    let (a2,x2) = Auto.step (dt,x) a
                        tdt = t + dt
                    in
                       if | tdt < t2 -> (x2, Sustain tdt n start a2)
                          | otherwise ->
                              if n < count
                                 then (x2, Sustain tdt (n+1) start start)
                                 else (x2, Release a2)
                Release a ->
                    let (a2,x2) = Auto.step (dt,x) a
                    in (x2, Release a2))

warp : Animation Float TimeStep -> Animation i space -> Animation (Float,i) space
warp twarp anim =
    animateWith (twarp,anim)
        (\(dt,(rate,x)) (tw1,a1) ->
            let (tw2,(_,dt2)) = Auto.step (dt,rate) tw1
                (a2,(_,x2)) = Auto.step (dt2,x) a1
            in ((dt,x2),(tw2,a2)))

-- Filters are expressed as automatons with some hidden state.

run = Auto.run

springy = springy_s f1d
springy2D = springy_s f2d
springyColor = springy_s fcolor

springy_s : Space a -> Float -> Float -> a -> Filter a
springy_s s f0 springiness p0 = animateWith (steady p0) (lpf_s s f0 (springiness * criticalQ))

laggy : Float -> Float -> Filter1D
laggy = laggy_s f1d

laggy2D : Float -> (Float, Float) -> Filter2D
laggy2D = laggy_s f2d

laggyColor : Float -> Color.Color -> FilterColor
laggyColor = laggy_s fcolor

laggy_s : Space a -> Float -> a -> Filter a
laggy_s space smoothingFactor p = animateWith (0.0, p, p) (lpf1_s space smoothingFactor)

linearFollower = linearFollower_s f1d
linearFollower2D = linearFollower_s f2d

linearFollower_s : Space a -> Float -> a -> Filter a
linearFollower_s space speed currPos = animateWith currPos (stepLinearFollower_s space speed)

-- Interpolators

lerp space = space.lerp
lerp1D = f1d.lerp
lerp2D = f2d.lerp
lerpColor = fcolor.lerp

-- First order filters

lpf1_s : Space a -> Float -> AnimUpdater (TimeStep,a,a) a a
lpf1_s space factor (dtn, xn) (dt0, xn0, yn1) =
    let
        dt = min intervalLimit (dt0 + dtn)
        x = if dt >= samplingInterval then space.lerp (dtn/dt) xn0 xn else xn0
    in
       if dt >= samplingInterval
          then let yn = space.lerp factor yn1 x
               in lpf1_s space factor (dt, yn) (dt - samplingInterval, x, yn)
          else ((dt,yn1), (dt, x, yn1))


-- Second order filters

type alias SOFState a = (TimeStep, a, a, a, a, a)
type alias SecondOrderFilter a = AnimUpdater (SOFState a) a a
type alias BiQuadSpec = {b0 : Float, b1 : Float, b2 : Float, a0 : Float, a1 : Float, a2 : Float}


currentVal : SOFState a -> a
currentVal (_, v, _, _, _, _) = v

biquad : BiQuadSpec -> SecondOrderFilter Float
biquad = biquad_s f1d
biquad2D = biquad_s f2d

intervalLimit = 10 * samplingInterval

biquad_s : Space a -> BiQuadSpec -> SecondOrderFilter a
biquad_s space bq (dtn, xn) (dt0, xn0, yn1, yn2, xn1, xn2) =
    let
        dt = min intervalLimit (dt0 + dtn)
    in
       if dt >= samplingInterval
          then let
                    sadd = space.add
                    x = space.scale (1.0/dt) (space.lsum dt0 xn0 dtn xn)
                    nexty = space.lsum bq.b0 xn bq.b1 xn1
                                `sadd` space.lsum bq.b2 xn2 (negate bq.a1) yn1
                                `sadd` space.scale (negate bq.a2) yn2
                                |> space.scale (1.0/bq.a0)
               in
                  biquad_s space
                      bq
                      (0.0, x)
                      (dt - samplingInterval, x, nexty, yn1, x, xn1)
          else ((dt,yn1), (dt, xn0, yn1, yn2, xn1, xn2))


-- From http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt
lpf f0 q = lpf_s f1d f0 q
lpf2D f0 q = lpf_s f2d f0 q

lpf_s : Space a -> Float -> Float -> SecondOrderFilter a
lpf_s space f0 q =
    let w0 = 2.0 * pi * f0 * samplingInterval
        cw0 = cos w0
        sw0 = sin w0
        alpha = sw0 / (2.0 * q)
        b1 = 1.0 - cw0
        b0 = b1 * 0.5
        b2 = b0
        a0 = 1.0 + alpha
        a1 = -2.0 * cw0
        a2 = 1.0 - alpha
    in
       biquad_s space {b0=b0,b1=b1,b2=b2,a0=a0,a1=a1,a2=a2}


criticalQ = 1.0 / sqrt 2.0

steady val = (0.0, val, val, val, val, val)

butterworth f0 = lpf f0 criticalQ

-- Trackers

stepLinearFollower = stepLinearFollower_s f1d
stepLinearFollower2D = stepLinearFollower_s f2d

stepLinearFollower_s : Space a -> Float -> AnimUpdater a a a
stepLinearFollower_s space speed (dt,p) currp =
    let
        d = dt * speed
        dir = space.sub p currp
        dirn = space.abs dir
        pos = if dirn <= d
                 then p
                 else space.madd (d/dirn) dir currp
    in
       ((dt,pos),pos)

-- Physics 1D

type alias PhasePos space = (space, space)
type alias ParticleState space = {mass : Float, x : space, px : space, dpx : space}
type alias ForceFn space = Space space -> TimeStep -> ParticleState space -> ParticleState space

type Force space
    = Drag space
    | SomeForce space
    | Kick space
    | Wall space Float space
    | Buff space space
    | Friction Float Float
    | Spring space Float Float
    | Gravity space
    | Custom (ForceFn space)

type alias Particle space = Animation (List (Force space)) (PhasePos space)

particle = particle_s f1d
particle2D = particle_s f2d

particle_s : Space s -> Float -> s -> s -> Particle s
particle_s s mass x vx = animateWith (particleState_s s mass x vx) (applyForces_s s)

applyForces_s : Space s -> AnimUpdater (ParticleState s) (List (Force s)) (PhasePos s)
applyForces_s s (dt, forces) p =
    let
        p2 = List.foldl (applyForce_s s dt) p forces |> move_s s dt
    in
       ((dt,(p2.x, p2.px)), p2)

applyForce_s : Space s -> TimeStep -> Force s -> ParticleState s -> ParticleState s
applyForce_s s dt f p =
    case f of
        Drag x -> drag_s s x dt p
        SomeForce f -> impulse_s s f dt p
        Kick dpx -> {p | dpx <- s.add dpx p.dpx}
        Wall dir e x -> wall_s s dir e x dt p
        Buff dir x -> wall_s s dir 0.0 x dt p
        Friction rest munorm -> friction_s s rest munorm dt p
        Spring x k damp -> spring_s s x k damp dt p
        Gravity g -> gravity_s s g dt p
        Custom fn -> fn s dt p

particleState_s : Space s -> Float -> s -> s -> ParticleState s
particleState_s s mass x vx = {mass = mass, x = x, px = s.scale mass vx, dpx = s.zero}

move = move_s f1d
move2D = move_s f2d

move_s : Space s -> TimeStep -> ParticleState s -> ParticleState s
move_s s dt p =
    let
        p1 = { p | px <- s.add p.px p.dpx, dpx <- s.zero }
        p2 = { p1 | x <- s.madd (dt / p1.mass) p1.px p1.x }
    in
       p2

-- Forces

drag = drag_s f1d
drag2D = drag_s f2d

-- Back calculate the momentum required to meet the desired drag.
-- Another way to do this would be to attach a high tension spring to
-- the particle and set the spring's end point to the desired drag
-- position.
drag_s : Space s -> s -> TimeStep -> ParticleState s -> ParticleState s
drag_s s x dt p =
    { p | x <- x,
          px <- s.scale (p.mass/dt) (s.sub x p.x),
          dpx <- s.zero
    }

impulse = impulse_s f1d
impulse2D = impulse_s f2d

impulse_s : Space s -> s -> TimeStep -> ParticleState s -> ParticleState s
impulse_s s f dt p = { p | dpx <- s.madd dt f p.dpx }

wall = wall_s f1d
wall2D = wall_s f2d

wall_s : Space s -> s -> Float -> s -> TimeStep -> ParticleState s -> ParticleState s
wall_s s dir e x dt obj =
    if s.dot obj.px dir < 0.0 && (s.abs obj.px) * dt / obj.mass > s.abs (s.sub obj.x x)
       then { obj | dpx <- s.madd (negate (1.0 + e)) obj.px obj.dpx }
       else obj

friction = friction_s f1d
friction2D = friction_s f2d

friction_s : Space s -> Float -> Float -> TimeStep -> ParticleState s -> ParticleState s
friction_s s rest munorm dt obj =
    let
        speed = s.abs obj.px
    in
       if speed <= obj.mass * rest
          then { obj | dpx <- s.sub obj.dpx obj.px }
          else impulse_s s (s.scale (negate munorm / speed) obj.px) dt obj

spring = spring_s f1d
spring2D = spring_s f2d

spring_s : Space s -> s -> Float -> Float -> TimeStep -> ParticleState s -> ParticleState s
spring_s s x0 k damp dt obj =
    impulse_s s (s.lsum k (s.sub obj.x x0) (negate damp / obj.mass) obj.px) dt obj

gravity = gravity_s f1d
gravity2D = gravity_s f2d

gravity_s : Space s -> s -> TimeStep -> ParticleState s -> ParticleState s
gravity_s s g dt obj =
    impulse_s s (s.scale obj.mass g) dt obj



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

appify : OpinionatedApp input model direction viewstate output -> App input model direction viewstate output
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
