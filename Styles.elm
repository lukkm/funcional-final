module Styles where

import Html.Attributes exposing (style,type',checked,src)
import Html exposing (..)

center : Attribute
center = style [("margin","0 auto")]

marginTop : Int -> Attribute
marginTop size = style [("margin-top", toString size ++ "px")]

marginLeft : Int -> Attribute
marginLeft size = style [("margin-left", toString size ++ "px")]

inline : Attribute
inline = style [("display","inline-block")]

tableBorder : Attribute
tableBorder =
    style
        [
        ("border","1px solid gray"),
        ("background-color","gray")
        ]

redTile : Attribute
redTile =
    style
        [
        ("border","1px solid gray"),
        ("background-color","red"),
        ("width", "20px"),
        ("height", "20px")
        ]

yellowTile : Attribute
yellowTile =
    style
        [
        ("border","1px solid gray"),
        ("background-color","yellow"),
        ("width", "20px"),
        ("height", "20px")
        ]

greenTile : Attribute
greenTile =
    style
        [
        ("border","1px solid gray"),
        ("background-color","green"),
        ("width", "20px"),
        ("height", "20px")
        ]

blueTile : Attribute
blueTile = 
    style
        [
        ("border","1px solid gray"),
        ("background-color","blue"),
        ("width", "20px"),
        ("height", "20px")
        ]