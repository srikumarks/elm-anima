module TestAnima where

import Anima exposing (..)
import Graphics.Element as El
import Graphics.Collage as Co
import Graphics.Input as In
import Touch
import Color
import Window
import Signal
import Debug exposing (..)
import Time
import Automaton as Auto exposing ((>>>))
import Focus

-- A simple application that shows a red box that can be in
-- one of two positions within the window. Clicking within the
-- window causes the box to jump to the other position using a
-- springy animation.

type Input = Quiet | Click | TimeStep Float | ResizeWindow (Int,Int)

type alias Env = {windowSize : (Int,Int)}
type alias DirectionE env = {env | color : Color.Color, x : Float}
type alias ViewStateE env = DirectionE env
type alias Direction = DirectionE Env
type alias ViewState = ViewStateE Env

animationNeeded = Signal.constant True
frames = Time.fpsWhen 60 animationNeeded
clicks = Signal.mailbox Quiet
windowResize = Signal.map ResizeWindow Window.dimensions

input = Signal.mergeMany
    [ windowResize
    , clicks.signal
    , Signal.map (\x -> TimeStep (Time.inSeconds x)) frames
    ]

type alias Model = {flipFlop : Bool}

initial = 
    { model = { flipFlop = False }
    , direction = { color = Color.red, x = 256.0, windowSize = (1024,768) }
    , viewstate = { color = Color.red, x = 256.0, windowSize = (1024,768) }
    , view = El.show "initializing..."
    }

app = {
    modelProc = 
        \input model ->
            case input of
                Click -> { model | flipFlop <- not model.flipFlop }
                _ -> model
    , director =
        \(input, model) dir ->
            let dir2 = case input of
                            ResizeWindow (w,h) ->
                                { dir | windowSize <- (w,h) }
                            _ ->
                                dir
                (w,h) = dir2.windowSize
                dx = 0.25 * toFloat w
            in { dir2 | x <- if model.flipFlop then -dx else dx }
    , animator = 
        filterProp x_ (springy 3.0 1.5 0.0)
    , timeStep = 
        \i -> case i of
                    TimeStep dt -> Just dt
                    _ -> Nothing
    , viewProc =
        \(model,vs) ->
            let (w,h) = vs.windowSize
            in
               Co.collage w h [Co.rect 100.0 100.0 |> Co.filled vs.color |> Co.moveX vs.x]
                   |> In.clickable (Signal.message clicks.address Click)
    , initial = initial
    }

main = Anima.runApp (appify app) input
