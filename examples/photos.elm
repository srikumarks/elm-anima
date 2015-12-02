module Photos where

import Array exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
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

{-

A simple viewer for a list of photos. Clicking on the currently
displayed photo will switch to the next one using a 3D transition.
The animation is interruptible and continuous.

-}

type Input = Quiet | Click | Next | Prev

-- We display one photo at a time. currentPhoto indicates which
-- photo as well as how we got to it. The actual photo index is
-- obtained by taking modulo numberOfPhotos.
type alias Model = {photoURLs : Array String, currentPhoto : Int}

-- Each photo is shown for a certain range of angles. The mapping
-- from angles to the photo to be shown is given by angleToPhotoIndex.
-- The angle is the stable angle at which the currentPhoto as indicated
-- by the model should be shown.
type alias Direction = {angle : Float, angleToPhotoIndex : Float -> Int}

-- The view state is essentially the same structure as the direction,
-- but the meaning of angle is different. This angle is a dynamic value
-- and the photo associated with the angle can change with the angle.
type alias ViewState = Direction

clicks = Signal.mailbox Quiet

input = clicks.signal


photoURLs = 
    [ "http://vignette3.wikia.nocookie.net/clubpenguinpookie/images/d/d0/Extremely-cute-kitten_large.jpg"
    , "http://dailynewsdig.com/wp-content/uploads/2013/05/cutest-kittens-in-the-world-2.jpg"
    , "http://www.pets.ie/pet_talk/wp-content/uploads/2013/07/cute-kitty-3.jpg"
    , "http://hdwpics.com/images/27F4124C4648/Cute-White-Brown-Kittens-HD.jpg"
    , "http://comparethemadkat.com/wp-content/uploads/2013/09/cat-102.jpg"
    , "http://www.vrouwen.nl/files/0/0/0/1/00019490.jpg"
    , "http://my10online.com/wp-content/uploads/2011/09/cutest-kittens-500.jpg"
    , "https://missswedishkiwi.files.wordpress.com/2015/05/kitten14.jpg"
    ]

numPhotos = List.length photoURLs

angleToPhotoIndex angle = (floor ((angle + 90) / 180)) % numPhotos

initial = 
    { model     = { photoURLs = Array.fromList photoURLs, currentPhoto = 0 }
    , direction = { angle = 0.0, angleToPhotoIndex = angleToPhotoIndex }
    , viewstate = { angle = 0.0, angleToPhotoIndex = angleToPhotoIndex }
    , view      = hiddenImageLoaders
    }

app : OpinionatedApp Input Model Direction ViewState Html
app = {
    -- The modeller looks after changing the "current photo" in response
    -- to user input.
    modeller = \input model ->
        case input of
            Next ->  { model | currentPhoto = model.currentPhoto + 1 }
            Prev ->  { model | currentPhoto = model.currentPhoto - 1 }
            Click -> app.modeller Next model
            _ -> model

    -- The director gives the angle at which the current photo should be displayed
    -- in the stable state
    , director = \(input, model) dir ->
        let data = dir.data
        in {dir | data = {data | angle = (toFloat model.currentPhoto) * 180}}

    -- The animator moves the angle from the current value to the value
    -- desired by the director.
    , animator = 
        filterProp (data_ => angle_) (springy 1.0 1.2 initial.direction.angle)

    -- The view renders that indication of the animator. The image to display
    -- is based on the angle, as indicated by the director providing the
    -- conversion function.
    , viewer = \(model, vs) ->
            case (Array.get (vs.data.angleToPhotoIndex vs.data.angle) model.photoURLs) of
                Just url ->
                    box vs.env.width vs.env.height 
                        [ hiddenImageLoaders
                        , let dx = 0.25 * toFloat vs.env.width
                              dy = 0.15 * toFloat vs.env.height
                          in picture url 512 512 dx dy vs.data.angle
                                 [ onClick clicks.address Click ]
                        ]
                Nothing ->
                    text "No such image"

    , initial = 
        initial
    }

main = Anima.runApp (appify app) input

box width height contents = 
    let widthStr = toString width ++ "px"
        heightStr = toString height ++ "px"
    in 
       div [ style [ ("width", widthStr)
                   , ("height", heightStr)
                   , ("background", "black")
                   ] ]
           contents

picture url width height dx dy angle attrs =
    let transform = perspective 1024 ++ translate3d dx dy 0.0 ++ rotateY angle
    in  div ([ style [ ("width", toString width ++ "px")
                     , ("height", toString height ++ "px")
                     , ("position", "absolute")
                     , ("background-image", "url(\"" ++ url ++ "\")")
                     , ("background-size", "contain")
                     , ("background-position", "center")
                     , ("background-repeat", "no-repeat")
                     , ("-webkit-transform", transform)
                     , ("-moz-transform", transform)
                     , ("transform", transform)
                     ] ] ++ attrs)
            []

-- Useful to trigger loading of images which will otherwise be loaded only one by one.
hiddenImageLoaders = div [style [ ("visibility", "hidden") ]] (List.map (\p -> img [src p] []) photoURLs)

-- Utility functions for calculating the transform css string.
perspective n = "perspective(" ++ toString n ++ "px)"
translate3d dx dy dz = "translate3d(" ++ toString dx ++ "px," ++ toString dy ++ "px," ++ toString dz ++ "px)"
rotateY deg = "rotateY(" ++ toString deg ++ "deg)"

keyMsg i = case (log "key" i) of
    37 -> Prev
    39 -> Next
    _ -> Quiet
    
angle_ = Focus.create .angle (\f rec -> {rec | angle = f rec.angle})

