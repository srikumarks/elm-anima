module Filters 
    ( Filter, samplingInterval, springy, laggy, linearFollower
    , lerp, lpf1, lpf, SecondOrderFilter, BiQuadSpec, currentVal, biquad
    , butterworth
    ) where

import GenAnim as GA
import Space exposing (..)
import Automaton as Auto

type alias Filter space = GA.Animation space space


samplingInterval = 1.0 / 60.0
animateWith = Auto.hiddenState

springy : Space a -> Float -> Float -> a -> Filter a
springy s f0 springiness p0 = 
    animateWith (steady p0) (lpf s f0 (springiness * criticalQ))


laggy : Space a -> Float -> a -> Filter a
laggy space smoothingFactor p = animateWith (0.0, p, p) (lpf1 space smoothingFactor)

linearFollower : Space a -> Float -> a -> Filter a
linearFollower space speed currPos = animateWith currPos (stepLinearFollower space speed)

-- Interpolators

lerp space = space.lerp

-- First order filters

lpf1 : Space a -> Float -> GA.AnimUpdater (GA.TimeStep,a,a) a a
lpf1 space factor (dtn, xn) (dt0, xn0, yn1) =
    let
        dt = min intervalLimit (dt0 + dtn)
        x = if dt >= samplingInterval then space.lerp (dtn/dt) xn0 xn else xn0
    in
       if dt >= samplingInterval
          then let yn = space.lerp factor yn1 x
               in lpf1 space factor (dt, yn) (dt - samplingInterval, x, yn)
          else ((dt,yn1), (dt, x, yn1))


-- Second order filters

type alias SOFState a = (GA.TimeStep, a, a, a, a, a)
type alias SecondOrderFilter a = GA.AnimUpdater (SOFState a) a a
type alias BiQuadSpec = {b0 : Float, b1 : Float, b2 : Float, a0 : Float, a1 : Float, a2 : Float}

currentVal : SOFState a -> a
currentVal (_, v, _, _, _, _) = v

intervalLimit = 10 * samplingInterval

biquad : Space a -> BiQuadSpec -> SecondOrderFilter a
biquad space bq (dtn, xn) (dt0, xn0, yn1, yn2, xn1, xn2) =
    let
        dtsum = dt0 + dtn
        dt = min intervalLimit dtsum
    in
       if dt >= samplingInterval
          then let
                    sadd = space.add
                    x = space.scale (1.0/dtsum) (space.lsum dt0 xn0 dtn xn)
                    nexty = space.lsum bq.b0 xn bq.b1 xn1
                                `sadd` space.lsum bq.b2 xn2 (negate bq.a1) yn1
                                `sadd` space.scale (negate bq.a2) yn2
                                |> space.scale (1.0/bq.a0)
               in
                  biquad space
                      bq
                      (0.0, x)
                      (dt - samplingInterval, x, nexty, yn1, x, xn1)
          else ((dt, yn1), (dt, xn0, yn1, yn2, xn1, xn2))


-- From http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt
lpf : Space a -> Float -> Float -> SecondOrderFilter a
lpf space f0 q =
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
       biquad space {b0=b0,b1=b1,b2=b2,a0=a0,a1=a1,a2=a2}


criticalQ = 1.0 / sqrt 2.0

steady val = (0.0, val, val, val, val, val)

butterworth s f0 = lpf s f0 criticalQ

-- Trackers

stepLinearFollower : Space a -> Float -> GA.AnimUpdater a a a
stepLinearFollower space speed (dt,p) currp =
    let
        d = dt * speed
        dir = space.sub p currp
        dirn = space.abs dir
        pos = if dirn <= d
                 then p
                 else space.madd (d/dirn) dir currp
    in
       ((dt,pos),pos)



