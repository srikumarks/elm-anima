module GenAnim 
    ( Animation, AnimUpdater, TimeStep
    , konst, anim, handoff, repeat, warp
    , animateWith
    ) where

import Automaton as Auto
import Space exposing (..)

type alias TimeStep = Float

-- An "animation" is a processor that, at each time step,
-- maintains an output that is dependent on the time evolution
-- of an input.
type alias Animation input output = Auto.Automaton (TimeStep, input) (TimeStep, output)

-- Updater function that can be used with Automaton to derive an animation.
type alias AnimUpdater state input output = (TimeStep, input) -> state -> ((TimeStep, output), state)

animateWith = Auto.hiddenState

-- An "animation" that produces a fixed value for any input.
konst : s -> Animation i s
konst val = Auto.pure (\(dt,_) -> (dt,val))

-- A fixed animation of a value from a starting value to an ending value
-- that happens over a given duration. The "ease" function can be used to
-- determine the shape of the animation near the starting and ending
-- values.
anim : Space s -> Float -> s -> s -> (Float -> Float) -> Animation i s
anim s dur fromVal toVal ease =
    animateWith 0.0
        (\(dt,_) t ->
            let f = t / dur
            in ((dt, s.lerp (ease f) fromVal toVal), t + dt))


-- A "handoff" combines two animations by transitioning from one to the other
-- over a determined interval. This structure holds the state of the transition.
type Handoff i o = Before Float (Animation i o) (Animation i o) | After (Animation i o)

-- A "handoff" runs the first animation over a period of "time",
-- and overlaps the second animation a bit before handing off
-- the motion to it. During the overlap, the input is cross
-- faded using the unit shape function (easing function).
handoff : Space s -> Time -> Animation i s -> Animation i s -> Animation i s
handoff space time animBefore animAfter =
    animateWith (Before 0.0 animBefore animAfter)
        (\(dt,x) s ->
            case s of
                Before t beforeAnim afterAnim ->
                    let (ab2,xa2) = Auto.step (dt,x) (if t < time then beforeAnim else afterAnim)
                    in (xa2, if t < time then Before (t+dt) ab2 afterAnim else After afterAnim)
                After afterAnim ->
                    let (aa2,xa2) = Auto.step (dt,x) afterAnim
                    in (xa2, After aa2))

type alias Time = Float

-- The state maintained by an animation repeater.
type RepeatState i space
    = Attack Time (Animation i space)
    | Sustain Time Int (Animation i space) (Animation i space)
    | Release (Animation i space)

-- Repeats the (t1,t2) portion of the given animation "count" times. 
repeat : Int -> (Time,Time) -> Animation i space -> Animation i space
repeat count (t1,t2) anim =
    animateWith (Attack 0.0 anim)
        (\(dt,x) s ->
            case s of
                Attack t a ->
                    let (a2,x2) = Auto.step (dt,x) a
                        tdt = t + dt
                    in
                       if tdt < t1 then
                          (x2, Attack tdt a2)
                       else
                          (x2, Sustain tdt 1 a a2)
                Sustain t n start a ->
                    let (a2,x2) = Auto.step (dt,x) a
                        tdt = t + dt
                    in
                       if tdt < t2 then 
                          (x2, Sustain tdt n start a2)
                       else
                          if n < count then
                             (x2, Sustain tdt (n+1) start start)
                          else 
                             (x2, Release a2)
                Release a ->
                    let (a2,x2) = Auto.step (dt,x) a
                    in (x2, Release a2))

-- Used to slow down or speed up animations using a time warper animation.
-- A time warper transforms a "rate" input into a series of time steps.
warp : Animation Float TimeStep -> Animation i space -> Animation (Float,i) space
warp twarp anim =
    animateWith (twarp,anim)
        (\(dt,(rate,x)) (tw1,a1) ->
            let (tw2,(_,dt2)) = Auto.step (dt,rate) tw1
                (a2,(_,x2)) = Auto.step (dt2,x) a1
            in ((dt,x2),(tw2,a2)))






