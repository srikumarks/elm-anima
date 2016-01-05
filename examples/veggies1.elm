module Veggies1 where

{-| Simple demo of two lists of labels. You can move
the labels between the two lists by clicking on them. 
-}

import Anima exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Touch
import Color
import Window
import Signal
import Debug exposing (..)
import Time
import Automaton as Auto exposing ((>>>))
import Focus exposing ((=>))
import Space exposing (..)
import LabelItem exposing (..)

{- We have a list that contains mixed fruits and veggies.
The user's task is to sort them into two list - the left
one holding only fruits and the right one only veggies. -}

{- Our model is utterly simple. It consists of two lists
of string names of our fruits and veggies. The only additional
thing is that each item is identified by a "key". -}
type alias Key = String
type alias Model = 
    { fruits : List (Key, String)
    , veggies : List (Key, String)
    }

{- We identify an item on either list using an enumeration.
The item on each list is identified by a zero-based index. -}
type Item = Fruit Int | Veggie Int

{- The input we can supply to the system is to make a move
of one item to another. You can move a fruit from one position
to another fruit position, or move a veggie into a fruit position,
and so on. -}
type Input = Quiet | Move Item Item

input = Signal.mailbox Quiet

modeller input model =
    case input of
        Quiet ->
            model
            
        Move (Fruit f1) (Fruit f2) ->
            { model | fruits = moveItem model.fruits f1 f2 }

        Move (Veggie v1) (Veggie v2) ->
            { model | veggies = moveItem model.veggies v1 v2 }

        Move (Fruit f) (Veggie v) ->
            let (item, rem) = removeItem f model.fruits
            in { model 
               | fruits = rem
               , veggies = insertItem item v model.veggies
               }

        Move (Veggie v) (Fruit f) ->
            let (item, rem) = removeItem v model.veggies
            in { model
               | veggies = rem
               , fruits = insertItem item f model.fruits
               }

moveItem list fromPos toPos =
    if fromPos == toPos then
       list
    else
        let (item, target) = removeItem fromPos list
        in 
           insertItem item (if fromPos < toPos then toPos - 1 else toPos) target

removeItem fromPos list =
    let h = List.take fromPos list
        t = List.drop fromPos list
    in
       (List.take 1 t, h ++ List.drop 1 t)

insertItem item toPos list =
    List.append (List.take toPos list) (List.append item (List.drop toPos list))

type alias Direction = {}

type alias ViewState = Direction


pt2px pt = pt * 1.5

initial = 
    { model = 
        { fruits =
            [ ("one", "Apple")
            , ("two", "Orange")
            , ("three", "Banana")
            , ("four", "Carrot")
            , ("five", "Broccoli")
            ]
        , veggies =
            [ ("six", "Pear")
            , ("seven", "Pumpkin")
            ]
        }
    , direction = {}
    , viewstate = {}
    , view = text "Initializing ..."
    }


app : OpinionatedApp Input Model Direction ViewState Html
app = 
    { modeller = modeller
    , director = \ _ dir -> dir
    , animator = Auto.pure (\x -> x)
    , viewer = viewer
    , initial = initial
    }

-- The viewer simply shows what's given to it as the view state.
-- At this point, it has the instanteous positions of all the
-- items in both the fruits and veggies lists.
viewer (model, vs) =
    div []
        [   div [style [("position", "relative"), ("padding", "33pt"), ("font-size", "24pt")]]
                [   span [style [("position", "absolute"), ("left", "30pt")]] [text "Fruits"]
                ,   span [style [("position", "absolute"), ("left", "230pt")]] [text "Veggies"]
                ]
        ,   div [style [("position", "relative"), ("width", "100%"), ("height", "250pt")]]
                (let fruitsList =
                        List.indexedMap 
                            (\i (k,label) -> 
                                labelItem label (fruitPos i) (movement (Fruit i)) input.address k)
                            model.fruits
                     veggiesList =
                         List.indexedMap 
                             (\i (k,label) -> 
                                 labelItem label (veggiePos i) (movement (Veggie i)) input.address k)
                             model.veggies
                 in
                    fruitsList ++ veggiesList)
        ,   div [style [("width", "300pt"), ("padding", "33pt"), ("font-size", "18pt")]] 
                [ text (dispCount "fruit" model.fruits ++ " and " ++ dispCount "veggie" model.veggies) ]
        ]

movement item =
    Just (Move item (case item of 
                        Fruit i -> Veggie 0
                        Veggie i -> Fruit 0))
             
dispCount str list =
    dispNum (List.length list) str
    
dispNum n noun = case n of
    0 -> "no " ++ pluralize noun
    1 -> "a " ++ noun
    _ -> toString n ++ " " ++ pluralize noun

pluralize noun = noun ++ "s"

main = let (app', _) = Anima.runOpinionatedApp app input.signal in app'
