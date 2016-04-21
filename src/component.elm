module Component(HtmlE, AttributeE, Input(..), Component, Path, path, static, node) where

{-| This module explores how a Html generating "component" may be specified
and composed just like normal Html if it were expressed as an Automaton.
It mimicks the [new Html API proposal by Evan](https://gist.github.com/evancz/3cd2ac3ca58956441702) 
using a type based on the old Address API for clarity.
-}

import Automaton as Auto
import Html exposing (..)
import Html.Attributes exposing (..)
import Signal as S
import Task

type alias HtmlE msg = 
    { render    : S.Address msg -> Html
    , tasks     : List (Task.Task () ())
    }
type alias AttributeE outMsg = S.Address outMsg -> Html.Attribute
type Input msg = Init | Input msg
type alias Component inMsg outMsg = Auto.Automaton (Input inMsg) (HtmlE outMsg)
type alias Path i1 o1 i2 o2 = Component i1 o1 -> Component i2 o2

append : Path i1 o1 i2 o2 -> Path i2 o2 i3 o3 -> Path i1 o1 i3 o3
append p1 p2 = \c -> p2 (p1 c)

path : (Input i2 -> Maybe (Input i1)) -> (o1 -> o2) -> Path i1 o1 i2 o2
path imap omap comp =
    Auto.hiddenState Nothing
        (\inp state ->
            let comp' = case state of
                            Just (c',h') -> c'
                            Nothing -> comp
            in
                case imap inp of 
                    Just i -> 
                        pathStep comp' i omap
                    Nothing ->
                        case state of
                            Nothing ->
                                pathStep comp Init omap
                            Just (c',h') ->
                                (h', state))

pathStep : Component i1 o1 -> (Input i1) -> (o1 -> o2) -> (HtmlE o2, Maybe (Component i1 o1, HtmlE o2))
pathStep comp' i omap =
    let (c',h) = Auto.step i comp'
        h' = { render = \addr -> h.render (S.forwardTo addr omap), tasks = h.tasks }
    in (h', Just (c',h'))

static : Html -> Component a b
static html = Auto.pure (\_ -> { render = \_ -> html, tasks = [] })

node : String -> List (AttributeE outMsg) -> List (Component inMsg outMsg) -> Component inMsg outMsg
node name attrs body = 
    Auto.hiddenState Nothing
        (\i state ->
            let comps' = case state of
                            Nothing -> body
                            Just cs -> cs
                co' = List.map (Auto.step i) comps'
                (c', o') = (List.map fst co', List.map snd co')
            in
               ( { render = \addr ->
                                 Html.node name 
                                    (List.map (\a -> a addr) attrs) 
                                    (List.map (\fn -> fn.render addr) o')
                 , tasks = List.map .tasks o' |> List.concat
                 }
               , Just c'
               ))
               
fst (x,_) = x
snd (_,x) = x

