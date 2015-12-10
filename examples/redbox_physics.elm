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
import Physics
import Space exposing (..)

{- A simple application that shows a red box that can be in one of two
positions within the window. Clicking within the window causes the
box to jump to the other position using a springy animation. -}

{- Our only input is a click within the box to switch its position. -}
type Input = Quiet | Click

{- Our only model state is a boolean - indicating whether the box
is on the left hand side or the right hand side. -}
type alias Model = {boxIsToTheLeft : Bool}

{- Given the position indication, the director decides a color for the box
and its actual stable position within the window. -}
type alias Direction = {color : Color.Color, x : Float}

{- The view state is structurally the same as the direction, except that the
meaning of the "x" field is "the position of the box right now" as opposed to
"the stable position of the box" as indicated by the direction. -}
type alias ViewState = Direction

clicks = Signal.mailbox Quiet

input = clicks.signal

initial = 
    { model     = { boxIsToTheLeft = False }
    , direction = { color = Color.red, x = 256.0 }
    , viewstate = { color = Color.red, x = 256.0 }
    , view      = El.show "initializing..."
    }

app : OpinionatedApp Input Model Direction ViewState El.Element
app = {

    {- The modeller responds to the input by flipping the position
    of the box from left to right or vice versa. -}
    modeller = \input model ->
        case input of
            Click -> { model | boxIsToTheLeft = not model.boxIsToTheLeft }
            _ -> model

    {- Based on the model, the director decides the stable position
    of the box within the window, taking into account the window
    environment. -}
    , director = \(input, model) dir ->
        let dx = 0.25 * toFloat dir.env.width
            data = dir.data
        in
           {dir | data = { data | x = if model.boxIsToTheLeft then -dx else dx } }

    {- The animator specifies how to achieve the position indicated
    by the director. In this case, the indicated stable position is
    to be reached using a springy animation. -}
    , animator = 
        let box = Physics.particle f1d 1.0 initial.direction.x f1d.zero
        in Physics.bind f1d box (data_ => x_) (\dir -> [Physics.Spring dir.data.x 200.0 10.0])

    {- The viewer takes the position indicated by the animator and renders
    the box at that position. -}
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

main = Anima.runOpinionatedApp app input
