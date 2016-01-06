module Example8 where

import Anima exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Window
import Signal
import Debug exposing (..)
import Time
import Automaton as Auto exposing ((>>>))
import Focus exposing ((=>))
import Physics
import Space exposing (..)
import Dict
import Debug

{- A rewrite of [example 8][e8] in the Elm architecture tutorial which involves
animation. It presents 4 buttons that can be twisted to the side by clicking
on them. The animations can be interrupted midway smoothly. 

[e8]: https://github.com/evancz/elm-architecture-tutorial/tree/master/examples/8

-}

{- Our only input is a click within the box to switch its tilt. -}
type Input = Quiet | Flip Int

{- Each button's state is captured by a boolean - whether it is
right side up (False) or tilted (True).
-}
type alias Model = {buttons : Dict.Dict Int Bool}

{- Given the button state, the director computes the intended final
stable angle of the button. The angles are in degrees.
-}
type alias Direction = {buttonAngles : Dict.Dict Int Float}

{- The view state is structurally the same as the direction, except that the
meaning of a button's angle is "the angle of the button right now" as opposed to
"the stable angle of the button" as indicated by the direction. 
-}
type alias ViewState = Direction

inputMailbox = Signal.mailbox Quiet

label i = case i of
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    _ -> "BIGNUM"

initial = 
    let dir = { buttonAngles = Dict.fromList [(1, 0.0), (2, 0.0), (3, 0.0), (4, 0.0)] }
    in { model     = { buttons = Dict.fromList [(1,False),(2,False),(3,False),(4,False)] }
       , direction = dir
       , viewstate = dir
       , view = text "Initializing..."
       }

app : OpinionatedApp Input Model Direction ViewState Html
app = {

    {- The modeller responds to the input by flipping the position
    of the button that's clicked on. -}
    modeller = \input model ->
        case input of
            Flip i ->
                { model | buttons = Dict.update i (Maybe.map not) model.buttons }
            Quiet -> 
                model

    {- Based on the model, the director decides the stable angle
    of the buttons.
    -}
    , director = \(input, model) dir ->
        let angle i flipped = if flipped then 90.0 else 0.0
            data = dir.data
        in { dir | data = { data | buttonAngles = Dict.map angle model.buttons } }

    {- The animator specifies how to achieve the angle indicated
    by the director. We use the physics module to make the right-tilt
    bounce when it hits the final value and leave the left tilt unhindered.

    animator        : Animation Direction Direction
    bindings        : Dict Int (Animation Direction Direction)
    bindParticle    : Int -> Particle Float -> Animation Direction Direction
    particles       : Dict Int (Particle Float)
    makeParticle    : Int -> Float -> Particle Float

    We use `makeParticle` to make a "particle" for each button and plonk them into
    the `particles` dictionary. Each of these particles, we turn them into an
    animation of the direction structure by "binding" the particle to a particular
    field in the direction structure. When binding, we specify the forces that the
    particle is to be subject to in order to get the behaviour we want. Now we have
    `bindings`, which is a collection of animations of direction structures. We then 
    chain all these to form the animator.
    -}
    , animator = 
        let bindings = 
                Dict.map bindParticle particles
            bindParticle i particle =
                Physics.bind f1d particle (data_ => buttonAngles_ => dictItem 0.0 i) 
                    (\dir -> 
                        [ Physics.Spring (degrees i dir.data.buttonAngles) 200.0 6.0
                        , Physics.Wall -1.0 0.7 90.0
                        ])
            particles = 
                Dict.map makeParticle initial.direction.buttonAngles
            makeParticle i angle =
                Physics.particle f1d 1.0 angle f1d.zero
            buttonAngles_ =
                Focus.create .buttonAngles (\fn rec -> { rec | buttonAngles = fn rec.buttonAngles })
            chain i binding result =
                binding >>> result
            passThrough = 
                Auto.pure (\x -> x)
        in 
           Dict.foldl chain passThrough bindings

    {- The viewer takes the position indicated by the animator and renders
    the box at that position. -}
    , viewer = \(model, vs) ->
        div [style [("width","640px"), ("height", "480px"), ("position", "absolute"), ("padding", "30pt")]]
            (List.map (button model vs) [1,2,3,4])

    , initial = 
        initial

    }

button model vs i =
    let tx = "rotate(" ++ toString (degrees i vs.data.buttonAngles) ++ "deg)"
    in div [style [ ("width", "80px"), ("height", "50px"), ("position", "relative"), ("text-align", "center")
                  , ("background", "lightblue"), ("margin", "50pt"), ("border-radius", "5pt")
                  , ("-webkit-transform", tx), ("-moz-transform", tx), ("transform", tx)
                  ]
           , onClick inputMailbox.address (Flip i)
           ]
           [ span [style [ ("position", "relative"), ("top", "10pt") ]]
               [text (buttonName model i)] ]

degrees i dict = 
    Maybe.withDefault 0.0 (Dict.get i dict)

buttonName model i =
    case Dict.get i model.buttons of
        Just bool -> (if bool then "side" else "up") ++ " " ++ toString i
        Nothing -> "oops!"
    
main = let (app', _) = Anima.runOpinionatedApp app inputMailbox.signal in app'
