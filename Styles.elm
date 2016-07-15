module Styles where

import Html.Attributes exposing (style,type',checked,src)
import Html exposing (..)

center : Attribute
center = style 
                [
                ("margin","0 auto"),
                ("text-align","center")
                ]

centerMarginTop : Int -> Attribute
centerMarginTop margin = style 
                [
                ("margin","0 auto"),
                ("text-align","center"),
                ("margin-top", toString margin ++ "px")
                ]

buttonsContainer : Attribute
buttonsContainer = style 
                        [
                        ("margin","0 auto"),
                        ("width","150px")
                        ]

movesContainer : Attribute
movesContainer = style 
                        [
                        ("margin","0 auto"),
                        ("width","400px"),
                        ("margin-top", "40px")
                        ]

showHintButton : Attribute
showHintButton = style 
                        [
                        ("margin","0 auto"),
                        ("width","200px"),
                        ("background", "#DDDDDD"),
                        ("padding", "10px"),
                        ("margin-top", "40px")
                        ]

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

orangeTile : Attribute
orangeTile = 
    style
        [
        ("border","1px solid gray"),
        ("background-color","orange"),
        ("width", "20px"),
        ("height", "20px")
        ]

purpleTile : Attribute
purpleTile = 
    style
        [
        ("border","1px solid gray"),
        ("background-color","purple"),
        ("width", "20px"),
        ("height", "20px")
        ]

redTileInline : Attribute
redTileInline =
    style
        [
        ("border","1px solid gray"),
        ("background-color","red"),
        ("margin-left","3px"),
        ("width", "20px"),
        ("height", "20px"),
        ("display", "inline-block")
        ]

yellowTileInline : Attribute
yellowTileInline =
    style
        [
        ("border","1px solid gray"),
        ("background-color","yellow"),
        ("margin-left","3px"),
        ("width", "20px"),
        ("height", "20px"),
        ("display", "inline-block")
        ]

greenTileInline : Attribute
greenTileInline =
    style
        [
        ("border","1px solid gray"),
        ("background-color","green"),
        ("margin-left","3px"),
        ("width", "20px"),
        ("height", "20px"),
        ("display", "inline-block")
        ]

blueTileInline : Attribute
blueTileInline = 
    style
        [
        ("border","1px solid gray"),
        ("background-color","blue"),
        ("margin-left","3px"),
        ("width", "20px"),
        ("height", "20px"),
        ("display", "inline-block")
        ]

orangeTileInline : Attribute
orangeTileInline = 
    style
        [
        ("border","1px solid gray"),
        ("background-color","orange"),
        ("margin-left","3px"),
        ("width", "20px"),
        ("height", "20px"),
        ("display", "inline-block")
        ]

purpleTileInline : Attribute
purpleTileInline = 
    style
        [
        ("border","1px solid gray"),
        ("background-color","purple"),
        ("margin-left","3px"),
        ("width", "20px"),
        ("height", "20px"),
        ("display", "inline-block")
        ]