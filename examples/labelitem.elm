module LabelItem where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Space exposing (..)

labelItem label pos message address strKey = 
    labelItem' label pos message address strKey []

labelItem' label (x,y) message address strKey attrs =
    let tx = "translate3d(" ++ (toString x) ++ "px," ++ (toString y) ++ "px, 0px)"
        clickHandler = case message of
            Just msg -> [onClick address msg]
            Nothing -> []
    in
        div ([ style [ ("position", "absolute")
                     , ("left", "0px")
                     , ("top", "0px")
                     , ("-webkit-transform", tx)
                     , ("-moz-transform", tx)
                     , ("-ms-transform", tx)
                     , ("transform", tx)
                     , ("width", "120pt")
                     , ("height", "30pt")
                     , ("background", "dodgerblue")
                     , ("display", "table")
                     , ("text-align", "center")
                     , ("-webkit-user-select", "none")
                     , ("-moz-user-select", "none")
                     , ("-ms-user-select", "none")
                     ]
             , key strKey
             ] ++ clickHandler ++ attrs)
             [ span [style [ ("display", "table-cell")
                           , ("vertical-align", "middle")
                           , ("color", "white") 
                           ]] 
                    [text label] ]

pt2px pt = pt * 1.33

fruitsOffset = (pt2px 30.0, pt2px 20.0)
veggiesOffset = (pt2px 230.0, pt2px 20.0)
fruitsDelta = (pt2px 0.0, pt2px 36.0)
veggiesDelta = (pt2px 0.0, pt2px 36.0)
xMiddle = pt2px (0.5 * (30.0 + 230.0))

fruitPos i = f2d.madd (toFloat i) fruitsDelta fruitsOffset
veggiePos i = f2d.madd (toFloat i) veggiesDelta veggiesOffset
fruitIndex (x,y) = let (_,y0) = fruitsOffset 
                       (_,dy) = fruitsDelta
                   in floor ((y - y0) / dy + 0.5)
veggieIndex (x,y) = let (_,y0) = veggiesOffset 
                        (_,dy) = veggiesDelta
                    in floor ((y - y0) / dy + 0.5)
isNearFruits (x,y) = x < xMiddle
isNearVeggies (x,y) = x > xMiddle




