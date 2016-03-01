import Html exposing (..)
import Html.Events exposing (onClick)
import StartApp.Simple as StartApp
import Html.Attributes exposing (style,type',checked,src)
import Html exposing (..)

--Files .elm
import Utils exposing (..)

{-

Model - Game logic.

-}

tableBorder : Attribute
tableBorder =
    style
        [
        ("border","1px solid gray"),
        ("background-color","gray")
        ]

type alias Model = 
    {
    board : Matrix
    }

init : Model
init = { board = getRandomBoard }

getStringFromTile : Tile -> String
getStringFromTile t = 
 if t == Yellow then "Y"
 else if t == Blue then "B"
 else if t == Red then "R"
 else "G"

--For the board, each List of the Matrix is one pilar of the board.
getRandomBoard : Matrix
getRandomBoard = randomMatrix 4 4

view address model = getViewBoard address model
update action model = model

getViewBoard : Signal.Address a -> Model -> Html
getViewBoard address model = 
    table [style [("margin","0 auto")]] [
    tr [tableBorder] [
        td [tableBorder] [text (getStringFromTile (getL (getL model.board 1) 1))],
        td [tableBorder] [text (getStringFromTile (getL (getL model.board 1) 2))],
        td [tableBorder] [text (getStringFromTile (getL (getL model.board 1) 3))],
        td [tableBorder] [text (getStringFromTile (getL (getL model.board 1) 4))]],
    tr [tableBorder] [
        td [tableBorder] [text "a"],
        td [tableBorder] [text "a"],
        td [tableBorder] [text "a"],
        td [tableBorder] [text "a"]],
    tr [tableBorder] [
        td [tableBorder] [text "a"],
        td [tableBorder] [text "a"],
        td [tableBorder] [text "a"],
        td [tableBorder] [text "a"]],
    tr [tableBorder] [
        td [tableBorder] [text "a"],
        td [tableBorder] [text "a"],
        td [tableBorder] [text "a"],
        td [tableBorder] [text "a"]]]

main = StartApp.start { model = init, view = view, update = update }