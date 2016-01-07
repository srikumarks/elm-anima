module Physics (PhasePos, Particle, Force(..), particle, position, momentum, bind) where 


{-| A simple implementation of Newtonian physics. 

A "particle" is modeled as a process that takes a list of forces applied to it
and, over each time step, changes its position in a "phase space" as a
consequence. The "phase space" has two axes - a "position" axis and a "momentum"
axis. 

Note that position itself can be multi-dimensional - such as 1D, 2D, 3D - and
the momentum will have as many dimensions.

-}

import Space exposing (..)
import GenAnim exposing (Animation, TimeStep, AnimUpdater, animateWith)
import Focus
import Automaton as Auto
import Debug

-- Phase space position is a pair where the first part is "position" and the
-- second part is "momentum".                                        
type alias PhasePos space = (space, space)

position : PhasePos space -> space
position (x, _) = x

momentum : PhasePos space -> space
momentum (_, px) = px

samplingInterval = 1.0 / 60.0
maxTimeGap = 10.0 * samplingInterval

-- The internal state of a "particle" includes its phase space coordinates, its
-- mass and an as-yet-unprocessed momentum update "dpx". The intention is for the
-- physics calculations to first accumulate effects in "dpx" and then to one final
-- "move" to update the position and momentum.                                                       
type alias ParticleState space = {mass : Float, x : space, px : space, dpx : space}

-- A generic force function is a particle state modifier.
type alias ForceFn space = Space space -> TimeStep -> ParticleState space -> ParticleState space

-- A few prebuilt types of forces you can apply to a particle.
type Force space
    = Drag space                -- Drag positionVector
    | SomeForce space           -- SomeForce forceVector
    | Kick space                -- Kick impulseVector
    | Wall space Float space    -- Wall normalVector coeffOfRestitution pointOnWall
    | Buff space space          -- a wall where coeffOfRestitution is zero
    | Friction Float Float      -- Friction restSpeed fricCoeff
    | Spring space Float Float  -- Spring anchorPoint hookeConstant dampingFactor
    | Gravity space             -- Gravity gvector

-- Disabling the custom force function for API simplicity initially.
--    | Custom (ForceFn space)

-- We model a particle as a process that responds to a list of forces by updating
-- a position in a "phase space".
type alias Particle space = Animation (List (Force space)) (PhasePos space)

-- Create a "particle". This is the only exported function.
particle : Space s -> Float -> s -> s -> Particle s
particle s mass x vx = animateWith {mass = mass, x = x, px = s.scale mass vx, dpx = s.zero} (applyForces' 0 s)

-- bind helps with associating a display property stored in a record with a
-- the physics of a particle. The position of the particle is mapped
-- to the record field, to produce an animation of the record.
bind : Space s -> Particle s -> Focus.Focus rec s -> (rec -> List (Force s)) -> Animation rec rec
bind s p focus forces =
    animateWith p
        (\(dt,rec) p ->
            let (p2, (_,(px2,_))) = Auto.step (dt, forces rec) p
            in ((dt, Focus.set focus px2 rec), p2))

-- An updater function that applies a list of forces on a particle to produce
-- its final phase position.
applyForces : Space s -> AnimUpdater (ParticleState s) (List (Force s)) (PhasePos s)
applyForces s (dt, forces) p =
    let p2 = List.foldl (applyForce s dt) p forces |> move s dt
    in ((dt,(p2.x, p2.px)), p2)

-- A wrapper to take care of large time gaps.
-- We limit the time gaps so that when the user switches tabs
-- and comes back, some major computation loop doesn't run,
-- and yet physics appears to settle somewhat.
applyForces' i s (dt, forces) p =
    if dt > maxTimeGap then
       applyForces' i s (maxTimeGap - samplingInterval, forces) p
    else if dt > samplingInterval then
            applyForces s (samplingInterval, forces) p
                |> \((_,_),p) -> p
                |> applyForces' (i+1) s (dt - samplingInterval, if i > 0 then forces else removeOnceOnlyForces forces)
    else 
        applyForces s (dt, forces) p
        
-- Drag and Kick should not be applied repeatedly.
-- The rest are like constraints and can continue to apply.
removeOnceOnlyForces =
    List.filter (\f -> case f of
                            Drag _ -> False
                            Kick _ -> False
                            _ -> True)

-- applyForce accumulates the momentum change implied by the given force in the particle's
-- "dpx" field.
applyForce : Space s -> TimeStep -> Force s -> ParticleState s -> ParticleState s
applyForce s dt f p =
    case f of
        Drag dx                 -> drag s dx dt p
        SomeForce f             -> force s f dt p
        Kick dpx                -> {p | dpx = s.add dpx p.dpx}
        Wall dir e x            -> wall s dir e x dt p
        Buff dir x              -> wall s dir 0.0 x dt p
        Friction rest munorm    -> friction s rest munorm dt p
        Spring x k damp         -> spring s x k damp dt p
        Gravity g               -> gravity s g dt p
        -- Custom fn               -> fn s dt p

-- Applies the momentum changes accumulated in the "dpx" field to 
-- the phase space position.                                               
move : Space s -> TimeStep -> ParticleState s -> ParticleState s
move s dt p =
    let newpx = s.add p.px p.dpx
    in { p | px = newpx, 
             dpx = s.zero, 
             x = s.madd (dt / p.mass) newpx p.x }

-- Forces

-- Models the particle being dragged by contact.
-- 
-- Back calculate the momentum required to meet the desired drag.
-- Another way to do this would be to attach a high tension spring to
-- the particle and set the spring's end point to the desired drag
-- position.
--
-- Originally, Drag was specified as the point to which the particle
-- must be attached. This is not desirable in general since which part
-- of the particle the position refers to is unclear. If we instead
-- specify the change in the particle's position, then the dragging
-- action becomes independent of where the particle has been "grabbed".
drag : Space s -> s -> TimeStep -> ParticleState s -> ParticleState s
drag s dx dt p =
    { p | px = s.scale (p.mass / dt) dx,
          dpx = s.zero }

-- Models a steady force applied over the given time step.
force : Space s -> s -> TimeStep -> ParticleState s -> ParticleState s
force s f dt p = { p | dpx = s.madd dt f p.dpx }

-- "Bounces" the particle off the wall on impact.
wall : Space s -> s -> Float -> s -> TimeStep -> ParticleState s -> ParticleState s
wall s dir e x dt obj =
    let pimpact = s.dot obj.px dir
    in  if pimpact < 0.0 && abs pimpact * dt / obj.mass > abs (s.dot dir (s.sub obj.x x))
           then { obj | dpx = s.madd (negate (1.0 + e)) (s.scale pimpact dir) obj.dpx }
           else obj

-- Applies a steady frictional force on the particle. If the speed
-- of the particle is under the given "rest" threshold, the particle is
-- brought to a full stop.
friction : Space s -> Float -> Float -> TimeStep -> ParticleState s -> ParticleState s
friction s rest munorm dt obj =
    let
        speed = s.abs obj.px
    in
       if speed <= obj.mass * rest
          then { obj | dpx = s.sub obj.dpx obj.px }
          else force s (s.scale (negate munorm / speed) obj.px) dt obj

-- Models a classic Hooke spring with one end anchored to a point in space and the other
-- end attached to the particle.                                                       
spring : Space s -> s -> Float -> Float -> TimeStep -> ParticleState s -> ParticleState s
spring s x0 k damp dt obj =
    force s (s.lsum k (s.sub x0 obj.x) (negate damp / obj.mass) obj.px) dt obj

-- Models a uniform gravitational field.
gravity : Space s -> s -> TimeStep -> ParticleState s -> ParticleState s
gravity s g dt obj =
    force s (s.scale obj.mass g) dt obj



