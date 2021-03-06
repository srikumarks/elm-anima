module Veggies4 where

{-| Simple demo of two lists of labels. You can move
the labels between the two lists by clicking on them. 
This one doesn't move the labels upon clicking, but
lets you drag the labels to various positions.
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
import Dict
import Task exposing (..)
import LabelItem exposing (..)
import Time exposing (..)
import Physics

fmin = Basics.min

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

findEntry key list =
    let find i list =
            case list of
                [] -> Nothing
                (key',_)::rest -> if key == key' then
                                      Just i
                                  else
                                      find (i+1) rest
    in find 0 list
       
{- We identify an item on either list using an enumeration.
The item on each list is identified by a zero-based index. -}
type alias Item = LabelItem.Item

findItem key model =
    let fruit = findEntry key model.fruits
        veggies = findEntry key model.veggies
    in
       case (fruit,veggies) of
           (Just f, Nothing) -> Fruit f
           (Nothing, Just v) -> Veggie v
           _ -> Fruit 0

{- The input we can supply to the system is to make a move
of one item to another. You can move a fruit from one position
to another fruit position, or move a veggie into a fruit position,
and so on. The director may indicate that a "suggestion" to
move an item from one position to another, giving the current
position of the item being dragged. DnD is a separate channel by
which the picker behaviour is sent to the director and animator. -}
type Input = Quiet | Move Item Item | DnD PickerAction

type alias LabelInfo = Point2D
type alias LabelColl = Dict.Dict String LabelInfo

{- Each item is directed to move to a particular point within the page. -}
type alias Direction = { picker : Picker Point2D
                       , suggestion : Maybe (String, Item, Item, Point2D)
                       , pickerOutput : Maybe (String, Point2D, Bool)
                       , pickerDelta : Maybe Point2D
                       , labels : LabelColl
                       }

{- For the moment, the view state is the same as the direction. 
We'll just be filtering the position to get a smooth movement. -}
type alias ViewState = Direction

inputMailbox = Signal.mailbox Quiet

{-| The modeller only cares about the definitive state of the
system that is relevant to the user - i.e. the state that the
user would wish to preserve. In this case, this is imply the
list of fruits and list of veggies. So the modeller only responds
to change requests and does not change the model in any other
circumstances.
-}
modeller : Input -> Model -> Model
modeller input model =
    case input of
        Move (Fruit f1) (Fruit f2) ->
            { model | fruits = moveItem model.fruits f1 f2 }

        Move (Veggie v1) (Veggie v2) ->
            { model | veggies = moveItem model.veggies v1 v2 }

        Move (Fruit f) (Veggie v) ->
            let (item, rem) = removeItem f model.fruits
            in { model | fruits = rem
                       , veggies = insertItem item v model.veggies
               }

        Move (Veggie v) (Fruit f) ->
            let (item, rem) = removeItem v model.veggies
            in { model | veggies = rem
                       , fruits = insertItem item f model.fruits
               }

        _ ->
            model

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

-- Sample data
initialModel =
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

-- targetter = laggy2D 0.15
targetter = springy2D 3.0 1.5

{-| The direction for the model is to make a dictionary that maps
item keys to item positions. These item positions are solely
a function of the list position for the moment.
-}
directionForModel : Model -> LabelColl -> LabelColl
directionForModel model dict =
    let (_, fruitsDict) = 
            List.foldl 
                (\(k,v) (i,d) -> 
                    (i+1, Dict.insert k (fruitPos i) d)) 
                (0, dict) 
                model.fruits
        (_, veggiesDict) =
            List.foldl 
                (\(k,v) (i,d) -> 
                    (i+1, Dict.insert k (veggiePos i) d)) 
                (0, fruitsDict) 
                model.veggies
    in
       veggiesDict

limit min max x =
    if x < min then
       min
    else if x > max then
       max
    else
        x

pickerAction input =
    case input of
        DnD x -> Just x
        _ -> Nothing
        
{-| The director provides indications based on the input and the
model computed by the modeller. These directions serve to specify
stable configurations of the system. The animator's responsibility is
to attain these stable configurations.
-}
director : (Input, Model) -> WithEnv Direction -> WithEnv Direction
director (input, model) dir =
    let data            = dir.data
        env             = dir.env
        (picker', pickerOutput) = Auto.step (dir.env.mousePos, pickerAction input) dir.data.picker

        (lenf, lenv)    = (List.length model.fruits, List.length model.veggies)
        fruitLimit      = limit 0 lenf
        veggieLimit     = limit 0 lenv

        -- If you're dragging an item i on the list, then it doesn't
        -- make sense to let you drop it at i + 1 on the same list.
        -- So exclude that possibility.
        ignoreNext ix item =
            case (ix,item) of
                (Just (Fruit ix'), Fruit i) ->
                    let i' = fruitLimit i
                    in if ix' + 1 == i' then ix' else i'
                (Just (Veggie ix'), Veggie i) ->
                    let i' = veggieLimit i
                    in if ix' + 1 == i' then ix' else i'
                (_, Fruit i) ->
                    fruitLimit i
                (_, Veggie i) ->
                    veggieLimit i
                  
        pickedIx = 
            case pickerOutput of
                Just (key,_,_) ->
                    Just (findItem key model)
                Nothing ->
                    Nothing

        suggestor pos =
            if isNearFruits pos then
               Fruit (ignoreNext pickedIx (Fruit (fruitIndex pos)))
            else
                Veggie (ignoreNext pickedIx (Veggie (veggieIndex pos)))

        suggestion = 
            pickerOutput 
                `Maybe.andThen` \(key, pos, active) ->
                    case (pickedIx, Dict.get key dir.data.labels) of
                        (Just source, Just pos0) ->
                            let pos' = f2d.add pos pos0
                            in Just (key, source, suggestor pos', pos')
                        _ ->
                            Nothing

        tasks = 
            case (suggestion, pickerOutput) of
                (Just (key', source, dest, _), Just (key, pos, False)) ->
                    [Signal.send inputMailbox.address (Move source dest)]
                _ ->
                    []
            
    in
       { dir | data = { data |
                        labels          = directionForModel model dir.data.labels
                      , picker          = picker'
                      , pickerOutput    = pickerOutput
                      , suggestion      = suggestion
                      }
             , env = { env | tasks = tasks }
       }

springK = 200.0
springDamping = 12.0

{-| The animator takes the direction dictionary and modifies it into a bank
of filters that will take the position of a particle to the final position
using a springy animation. In this case, we're applying the picker to
the particle collection, so only one of them will usually be moving, though,
if you're fast enough, you can catch multiple things moving in parallel.

The animator also computes any tasks that are determined from the final
animated positions of the item being picked and moved about.
-}
animator : Animation (WithEnv Direction) (WithEnv ViewState)
animator =
    let particles = 
            Dict.map animation initial.direction.labels
        animation key pos =
            Physics.bind f2d
                (Physics.particle f2d 1.0 pos f2d.zero)
                (data_ => labels_ => dictItem f2d.zero key)
                (force key)
        force key dir =
            case (dir.data.pickerOutput, dir.data.pickerDelta) of
                (Just (key', pos, active), Just delta) ->
                    if key == key' then
                       [Physics.Drag delta]
                    else
                        spring key dir
                _ ->
                    spring key dir
        spring key dir =
            case Dict.get key dir.data.labels of
                Just pos' ->
                    [Physics.Spring pos' springK springDamping]
                Nothing ->
                    []
        chain key anim result =
            result >>> anim
        passThrough = 
            Auto.pure (\x->x)
        forceAnimator = 
            Dict.foldl chain passThrough particles
        logEnabled = False
        logInput label = 
            Auto.pure (\(dt,dir) -> (dt, if logEnabled then Debug.log label dir else dir))
    in
        logInput "animInput" >>> pickerDiff >>> forceAnimator >>> logInput "animOutput"

pickerDiff = 
    let setPickerDiff pd dir =
            let data = dir.data
            in { dir | data = { data | pickerDelta = pd } }
        updateDiff (dt, dir) prev =
            case dir.data.pickerOutput of
                Just (key, pos, active) ->
                    case prev of
                        Just prevPos ->
                            ((dt, setPickerDiff (Just (f2d.sub pos prevPos)) dir), Just pos)
                        Nothing ->
                            ((dt, setPickerDiff (Just f2d.zero) dir), Just pos)
                Nothing ->
                    ((dt, setPickerDiff Nothing dir), Nothing)
    in
       Auto.hiddenState Nothing updateDiff

labels_ : Focus.Focus Direction (Dict.Dict String LabelInfo)
labels_ = Focus.create .labels (\fn rec -> { rec | labels = fn rec.labels })

pos_ : Focus.Focus LabelInfo Point2D
pos_ = Focus.create (\x->x) (\fn rec -> fn rec) 

{-| The viewer simply shows what's given to it as the view state.
At this point, it has the instanteous positions of all the
items in both the fruits and veggies lists.

The labels are positioned absolutely and their changes of location
are effected using the "transform" CSS property.
-}
viewer : (Model, WithEnv ViewState) -> Html
viewer (model, vs) =
    div [ onMouseMove inputMailbox.address (DnD MoveItem)
        , onMouseUp inputMailbox.address (DnD DropItem)
        ]
        [   div [ style [("position", "relative"), ("padding", "33pt"), ("font-size", "24pt")] ]
                [ span [style [("position", "absolute"), ("left", "30pt")]] [text "Fruits"]
                , span [style [("position", "absolute"), ("left", "230pt")]] [text "Veggies"]
                ]
        ,   div [ style [("position", "relative"), ("width", "100%"), ("height", "250pt")] ]
                (fruitsList (model,vs)
                    ++ veggiesList (model,vs)
                    ++ suggestorLines (model,vs))
        ,   div [ style [("width", "300pt"), ("padding", "33pt"), ("font-size", "18pt")] ] 
                [ text (dispCount "fruit" model.fruits ++ " and " ++ dispCount "veggie" model.veggies) ]
        ]

-- Shows the current fruits list
fruitsList (model, vs) =
    List.indexedMap 
        (\i (k,label) -> 
            labelItem' label (getPos k vs.data.labels) Nothing inputMailbox.address |> draggable k (lift k vs) [])
        model.fruits
        
-- Shows the current veggies list
veggiesList (model, vs) =
    List.indexedMap 
        (\i (k,label) -> 
            labelItem' label (getPos k vs.data.labels) Nothing inputMailbox.address |> draggable k (lift k vs) [])
        model.veggies
     
-- Make sure that the thing you're picking and moving stays on top.
baseZOrder = [("zIndex", "1")]
lift k vs =
    case vs.data.suggestion of
        Just (key, _, _, _) ->
            if k == key then [("zIndex", "100")] else baseZOrder
        Nothing ->
            baseZOrder
            
-- If the picker is active, this shows two lines -
-- one light gray one indicating the position of the
-- label that was picked up, and one black one indicating
-- the position where it will be dropped.
suggestorLines (model, vs) = 
    case vs.data.suggestion of
        Just (key, source, dest, _) ->
            [ line source "lightgrey", line dest "black" ]
        Nothing ->
            []

line item color =
    let (x,y) = case item of
        Fruit f -> fruitPos f
        Veggie v -> veggiePos v
    in
       div [style [ ("position", "absolute")
                  , ("height", "2px")
                  , ("width", "120pt")
                  , ("background", color)
                  , ("left", toString x ++ "px")
                  , ("top", toString (y - 5.0) ++ "px")
                  , ("zIndex", "1")
                  ]]
           []

dispCount str list =
    dispNum (List.length list) str
    
dispNum n noun = case n of
    0 -> "no " ++ pluralize noun
    1 -> "a " ++ noun
    _ -> toString n ++ " " ++ pluralize noun

pluralize noun = noun ++ "s"

-- Starting conditions
initial = 
    let dir = { suggestion = Nothing
              , labels = directionForModel initialModel Dict.empty
              , picker = picker
              , pickerOutput = Nothing
              , pickerDelta = Nothing
              }
    in { model = initialModel
       , direction = dir
       , viewstate = dir
       , view = text "Initializing ..."
       }

app : OpinionatedApp Input Model Direction ViewState Html
app = 
    { modeller = modeller
    , director = director
    , animator = animator
    , viewer = viewer
    , initial = initial
    }

draggable key styles attrs entity =
    entity key styles 
        ([ onMouseDown inputMailbox.address (DnD (PickupItem key))
         , onMouseUp inputMailbox.address (DnD DropItem)
         , onMouseMove inputMailbox.address (DnD MoveItem)
         ] ++ attrs)
           
getPos key dict =
    let info = Dict.get key dict
    in case info of
            Just pos -> pos
            Nothing -> defaultPos

setPos key fn dict =
    Dict.insert key (fn defaultPos) dict

defaultPos = (100.0, 100.0)

movement item =
    Just (Move item (case item of 
                          Fruit i -> Veggie 0
                          Veggie i -> Fruit 0))
        
sigFirst : Signal (a,b) -> Signal a
sigFirst sig = Signal.map (\(a,_) -> a) sig

sigSecond : Signal (a,b) -> Signal b
sigSecond sig = Signal.map (\(_,b) -> b) sig

appSignal = Anima.runOpinionatedApp app inputMailbox.signal

port tasks : Signal (Task.Task () ())
port tasks = let (_, tasks) = appSignal in tasks

main = let (app, _) = appSignal in app

