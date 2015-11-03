module GenAnim 
    ( Animation, AnimUpdater, TimeStep
    , konst, anim, handoff, repeat, warp
    , animateWith
    ) where

import Automaton as Auto
import Space exposing (..)

type alias Animation input output = Auto.Automaton (TimeStep, input) (TimeStep, output)
type alias AnimUpdater state input output = (TimeStep, input) -> state -> ((TimeStep, output), state)
type alias TimeStep = Float

animateWith = Auto.hiddenState

konst : s -> Animation i s
konst val = Auto.pure (\(dt,_) -> (dt,val))

anim : Space s -> Float -> s -> s -> (Float -> Float) -> Animation i s
anim s dur fromVal toVal ease =
    animateWith 0.0
        (\(dt,_) t ->
            let f = t / dur
            in ((dt, s.lerp (ease f) fromVal toVal), t + dt))


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






