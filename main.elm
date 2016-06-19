import Html exposing (..)
import Html.Events exposing (onClick)
import StartApp.Simple as StartApp

--Files .elm
import Utils exposing (..)
import Styles exposing (..)
import Debug exposing (..)

{-

Model - Game logic.

-}

type alias Model = 
    {
    board : Matrix
    }

init : Model
init = { board = getRandomBoard }

getColorForTile : Tile -> Attribute
getColorForTile t = 
 if t == Yellow then yellowTile
 else if t == Blue then blueTile
 else if t == Red then redTile
 else greenTile

--For the board, each List of the Matrix is one pilar of the board.
getRandomBoard : Matrix
getRandomBoard = randomMatrix 4 4

view address model = getGameView address model

getGameView : Signal.Address Action -> Model -> Html
getGameView address model = 
    div [center] [
        getViewBoard model,
        getButtons address
    ]

getViewBoard : Model -> Html
getViewBoard model = 
    table [center] [
    tr [tableBorder] [
        td [getColorForTile (getM model.board 1 1)] [text ""],
        td [getColorForTile (getM model.board 1 2)] [text ""],
        td [getColorForTile (getM model.board 1 3)] [text ""],
        td [getColorForTile (getM model.board 1 4)] [text ""]],
    tr [tableBorder] [
        td [getColorForTile (getM model.board 2 1)] [text ""],
        td [getColorForTile (getM model.board 2 2)] [text ""],
        td [getColorForTile (getM model.board 2 3)] [text ""],
        td [getColorForTile (getM model.board 2 4)] [text ""]],
    tr [tableBorder] [
        td [getColorForTile (getM model.board 3 1)] [text ""],
        td [getColorForTile (getM model.board 3 2)] [text ""],
        td [getColorForTile (getM model.board 3 3)] [text ""],
        td [getColorForTile (getM model.board 3 4)] [text ""]],
    tr [tableBorder] [
        td [getColorForTile (getM model.board 4 1)] [text ""],
        td [getColorForTile (getM model.board 4 2)] [text ""],
        td [getColorForTile (getM model.board 4 3)] [text ""],
        td [getColorForTile (getM model.board 4 4)] [text ""]]]

getButtons : Signal.Address Action -> Html
getButtons address = 
    div [center, marginTop 40] [
        div [inline,yellowTile,onClick address (ChangeColor Yellow)] [],
        div [inline,blueTile,onClick address (ChangeColor Blue)] [],
        div [inline,redTile,onClick address (ChangeColor Red)] [],
        div [inline,greenTile,onClick address (ChangeColor Green)] []
    ]

{-
Update - Represents all interactions (actions) with the model .
--------------------------------------------------------------
-}
type Action = ChangeColor Tile

update : Action -> Model -> Model
update action model = 
    case action of
        ChangeColor tile -> changeColor model tile

changeColor : Model -> Tile -> Model
changeColor model tile = 
    {
        board = updateBoard model.board 1 1 (getL (getL model.board 1) 1) tile
    }

updateBoard : Matrix -> Int -> Int -> Tile -> Tile -> Matrix 
updateBoard board x y originalColor nextColor = 
    if (x < 1 || y < 1 || x > (sizeOf board) || y > (sizeOf board) || (getM board x y) /= originalColor)
        then
            fst (board, log "Holis" (x, y))
        else
            updateBoard (updateBoard (updateBoard (updateBoard (putM board x y nextColor) x (y-1) originalColor nextColor) (x-1) y originalColor nextColor) x (y+1) originalColor nextColor) (x+1) y originalColor nextColor

main = StartApp.start { model = init, view = view, update = update }