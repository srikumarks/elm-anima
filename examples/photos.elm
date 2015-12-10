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

{- We display one photo at a time. currentPhoto indicates which photo as well
as how we got to it. The actual photo index is obtained by taking modulo
numberOfPhotos. -}
type alias Model = { photoURLs : Array String, currentPhoto : Int }

{- Each photo is shown for a certain range of angles. The mapping from angles
to the photo to be shown is given by `angleToPhotoIndex`.  The `angle` is the
stable angle at which the `currentPhoto` as indicated by the model should be
shown. The photo index provided by the `angleToPhotoIndex` function is to be
interpreted modulo the number of photos, so this record holds this modulus as
well. -}
type alias Direction = { angle : Float, numPhotos : Int, angleToPhotoIndex : Float -> Int }

{- The view state can often be the same structure as the direction, but in this
case, it is a little different, as the viewer needs to know which photo to display
and how to display it. Which photo is given by `photoIndex` and the "how" part is
jointly indicated by `angle` and `flip`. While `angle` looks like the same value 
in the direction, this is the *instantaneous* angle, as opposed to the stable angle
indicated in the direction. `flip` tells the viewer whether the image is to be flipped
horizontally. -}
type alias ViewState = { angle : Float, photoIndex : Int, flip : Bool }

clicks = Signal.mailbox Quiet

input = clicks.signal

photoURLs = 
    [ "http://data.whicdn.com/images/12769998/large.jpg"
    , "http://amazinganimalphotos.com/wp-content/uploads/2013/04/World-s-Cutest-Kitten-cat-photo.jpg"
    , "http://www.pets.ie/pet_talk/wp-content/uploads/2013/07/cute-kitty-3.jpg"
    , "http://hdwpics.com/images/27F4124C4648/Cute-White-Brown-Kittens-HD.jpg"
    , "http://comparethemadkat.com/wp-content/uploads/2013/09/cat-102.jpg"
    , "http://www.vrouwen.nl/files/0/0/0/1/00019490.jpg"
    , "http://my10online.com/wp-content/uploads/2011/09/cutest-kittens-500.jpg"
    , "https://missswedishkiwi.files.wordpress.com/2015/05/kitten14.jpg"
    ]

initial = 
    { model     = { photoURLs = Array.fromList photoURLs, currentPhoto = 0 }
    , direction = { angle = 0.0
                  , numPhotos = List.length photoURLs
                  , angleToPhotoIndex = \angle -> floor ((angle + 90) / 180)
                  }
    , viewstate = { angle = 0.0, photoIndex = 0, flip = False }
    , view      = hiddenImageLoaders
    }

app : OpinionatedApp Input Model Direction ViewState Html
app = {
    {- The modeller looks after changing the "current photo" in response
    to user input. -}
    modeller = \input model ->
        case input of
            Next -> { model | currentPhoto = model.currentPhoto + 1 }
            Prev -> { model | currentPhoto = model.currentPhoto - 1 }
            Click -> app.modeller Next model
            _ -> model

    {- The director gives the angle at which the current photo should be displayed
    in the stable state. -}
    , director = \(input, model) dir ->
        let data = dir.data
        in {dir | data = {data | angle = 180.0 * toFloat model.currentPhoto}}

    {- The animator moves the angle from the current value to the value desired
    by the director. The animator also decides, based on the angle it computes,
    which image is to be shown at that angle.  Depending on the angle, the
    image may need to be flipped along the x axis. So the animator also
    computes that information. -}
    , animator = 
        filterProp angle_ (springy 1.0 1.2 initial.direction.angle)
            >>> Anima.pureData (\a -> 
                    let ix = a.angleToPhotoIndex a.angle
                    in { angle = a.angle
                       , photoIndex = ix % a.numPhotos
                       , flip = ix % 2 == 1
                       })

    {- The view renders the will of the animator. In this case, the animator
    has computed for us the photo to be shown, the angle at which it should be
    shown, and whether we should flip it along the x axis. -}
    , viewer = \(model, vs) ->
            case (Array.get vs.data.photoIndex model.photoURLs) of 
                Just url ->
                    box vs.env.width vs.env.height 
                        [ hiddenImageLoaders
                        , let dx = 0.25 * toFloat vs.env.width
                              dy = 0.15 * toFloat vs.env.height
                          in picture url 512 512 dx dy vs.data.angle vs.data.flip
                                [ onClick clicks.address Click ]
                        ]
                Nothing ->
                    text "No such image"

    , initial = 
        initial
    }

main = Anima.runOpinionatedApp app input

-- Puts some content within a black box of given dimensions.
box width height contents = 
    let widthStr = toString width ++ "px"
        heightStr = toString height ++ "px"
    in 
       div [ style [ ("width", widthStr)
                    , ("height", heightStr)
                    , ("background", "black")
                    ] ]
           contents

{- Shows a picture drawn from the given url, at the given dimensions fitting it
in such a way that the picture's aspect ratio is preserved. The whole
picture is translated and rotated according to the given dx,dy and
angle. -}
picture url width height dx dy angle flip attrs =
    let transform = perspective 1024 ++ translate3d dx dy 0.0 ++ rotateY angle ++ flipX flip
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
hiddenImageLoaders = div [hidden True] (List.map (\p -> img [src p] []) photoURLs)

-- Utility functions for calculating the transform css string.
perspective n = "perspective(" ++ toString n ++ "px)"
translate3d dx dy dz = "translate3d(" ++ toString dx ++ "px," ++ toString dy ++ "px," ++ toString dz ++ "px)"
rotateY deg = "rotateY(" ++ toString deg ++ "deg)"
flipX flip = if flip then "scale3d(-1.0,1.0,1.0)" else ""

angle_ = data_ => Focus.create .angle (\f rec -> {rec | angle = f rec.angle})

