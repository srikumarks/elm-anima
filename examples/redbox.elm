module RedBox where

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
import Focus exposing ((=>))

-- A simple application that shows a red box that can be in
-- one of two positions within the window. Clicking within the
-- window causes the box to jump to the other position using a
-- springy animation.

type Input = Quiet | Click

type alias Direction = {color : Color.Color, x : Float}
type alias ViewState = Direction

clicks = Signal.mailbox Quiet

input = clicks.signal

type alias Model = {flipFlop : Bool}

initial = 
    { model     = { flipFlop = False }
    , direction = { color = Color.red, x = 256.0 }
    , viewstate = { color = Color.red, x = 256.0 }
    , view      = El.show "initializing..."
    }

app : OpinionatedApp Input Model Direction ViewState El.Element
app = {
    modeller = \input model ->
        case input of
            Click -> { model | flipFlop = not model.flipFlop }
            _ -> model

    , director = \(input, model) dir ->
        let dx = 0.25 * toFloat dir.env.width
            data = dir.data
        in
           {dir | data = {data | x = if model.flipFlop then -dx else dx}}

    , animator = 
        filterProp (data_ => x_) (springy 3.0 1.5 0.0)

    , viewer = \(model, vs) ->
        Co.collage vs.env.width vs.env.height [
                Co.rect 100.0 100.0 
                    |> Co.filled vs.data.color 
                    |> Co.moveX vs.data.x
            ]
            |> In.clickable (Signal.message clicks.address Click)

    , initial = 
        initial
    }

main = Anima.runApp (appify app) input
