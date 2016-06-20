import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style,type',checked,src)
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
    board : Matrix,
    status : Status
    }

init : Model
init = { board = getRandomBoard, status = Starting }

getColorForTile : Tile -> Attribute
getColorForTile t = 
 if t == Yellow then yellowTile
 else if t == Blue then blueTile
 else if t == Red then redTile
 else greenTile

--Generate a random board
getRandomBoard : Matrix
getRandomBoard = randomMatrix 6 6

view address model = 
    case model.status of
        Starting -> getStartingView address
        InGame -> getGameView address model
        Won -> getWonView address

getGameView : Signal.Address Action -> Model -> Html
getGameView address model = 
    div [center] [
        getViewBoard model,
        getButtons address
    ]

getStartingView : Signal.Address Action -> Html
getStartingView address = 
    div [center] 
        [   
            getPlayButton address
        ]

getWonView : Signal.Address Action -> Html
getWonView address = 
    div[center] 
       [
            getWonText,
            getPlayButton address
       ]

getViewBoard : Model -> Html
getViewBoard model = 
    table [center] [
    tr [tableBorder] [
        td [getColorForTile (getM model.board 1 1)] [text ""],
        td [getColorForTile (getM model.board 1 2)] [text ""],
        td [getColorForTile (getM model.board 1 3)] [text ""],
        td [getColorForTile (getM model.board 1 4)] [text ""],
        td [getColorForTile (getM model.board 1 5)] [text ""],
        td [getColorForTile (getM model.board 1 6)] [text ""]],
    tr [tableBorder] [
        td [getColorForTile (getM model.board 2 1)] [text ""],
        td [getColorForTile (getM model.board 2 2)] [text ""],
        td [getColorForTile (getM model.board 2 3)] [text ""],
        td [getColorForTile (getM model.board 2 4)] [text ""],
        td [getColorForTile (getM model.board 2 5)] [text ""],
        td [getColorForTile (getM model.board 2 6)] [text ""]],
    tr [tableBorder] [
        td [getColorForTile (getM model.board 3 1)] [text ""],
        td [getColorForTile (getM model.board 3 2)] [text ""],
        td [getColorForTile (getM model.board 3 3)] [text ""],
        td [getColorForTile (getM model.board 3 4)] [text ""],
        td [getColorForTile (getM model.board 3 5)] [text ""],
        td [getColorForTile (getM model.board 3 6)] [text ""]],
    tr [tableBorder] [
        td [getColorForTile (getM model.board 4 1)] [text ""],
        td [getColorForTile (getM model.board 4 2)] [text ""],
        td [getColorForTile (getM model.board 4 3)] [text ""],
        td [getColorForTile (getM model.board 4 4)] [text ""],
        td [getColorForTile (getM model.board 4 5)] [text ""],
        td [getColorForTile (getM model.board 4 6)] [text ""]],
    tr [tableBorder] [
        td [getColorForTile (getM model.board 5 1)] [text ""],
        td [getColorForTile (getM model.board 5 2)] [text ""],
        td [getColorForTile (getM model.board 5 3)] [text ""],
        td [getColorForTile (getM model.board 5 4)] [text ""],
        td [getColorForTile (getM model.board 5 5)] [text ""],
        td [getColorForTile (getM model.board 5 6)] [text ""]],
    tr [tableBorder] [
        td [getColorForTile (getM model.board 6 1)] [text ""],
        td [getColorForTile (getM model.board 6 2)] [text ""],
        td [getColorForTile (getM model.board 6 3)] [text ""],
        td [getColorForTile (getM model.board 6 4)] [text ""],
        td [getColorForTile (getM model.board 6 5)] [text ""],
        td [getColorForTile (getM model.board 6 6)] [text ""]]]

getButtons : Signal.Address Action -> Html
getButtons address = 
    div [buttonsContainer] [
        div [yellowTileInline,onClick address (ChangeColor Yellow)] [],
        div [blueTileInline,onClick address (ChangeColor Blue)] [],
        div [redTileInline,onClick address (ChangeColor Red)] [],
        div [greenTileInline,onClick address (ChangeColor Green)] []
    ]

getPlayButton : Signal.Address Action -> Html
getPlayButton address =
    div [style [("margin", "0 auto"),("width","250px")]] 
    [
        img [src "img/playbutton.jpeg",Html.Attributes.width 250,onClick address Start,style [("margin-top","20px")]] []
    ]

getWonText : Html
getWonText = h1 [center] [text "You win"]

{-
Update - Represents all interactions (actions) with the model .
--------------------------------------------------------------
-}
type Action = ChangeColor Tile | Start

update : Action -> Model -> Model
update action model = 
    case action of
        ChangeColor tile -> changeColor model tile
        Start -> {
                    model | board = getRandomBoard,
                            status = InGame
                }

changeColor : Model -> Tile -> Model
changeColor model tile =
    let 
        updatedBoard = updateBoard model.board 1 1 (getL (getL model.board 1) 1) tile
    in
        {
            model | board = updatedBoard,
                    status = getBoardStatus updatedBoard
        }

updateBoard : Matrix -> Int -> Int -> Tile -> Tile -> Matrix 
updateBoard board x y originalColor nextColor = 
    if (x < 1 || y < 1 || x > (sizeOf board) || y > (sizeOf board) || (getM board x y) /= originalColor || originalColor == nextColor)
        then
            board
        else
            updateBoard (updateBoard (updateBoard (updateBoard (putM board x y nextColor) x (y-1) originalColor nextColor) (x-1) y originalColor nextColor) x (y+1) originalColor nextColor) (x+1) y originalColor nextColor

getBoardStatus : Matrix -> Status
getBoardStatus board = getBoardStatusRec board (getM board 1 1) 1 1

getBoardStatusRec : Matrix -> Tile -> Int -> Int -> Status
getBoardStatusRec board color x y = 
    if (x > (sizeOf board) || y > (sizeOf board))
        then
            Won
        else
            if (getM board x y /= color)
                then
                    InGame
                else
                    if (getBoardStatusRec board color (x+1) y == Won && getBoardStatusRec board color x (y+1) == Won)
                        then
                            Won
                        else
                            InGame

main = StartApp.start { model = init, view = view, update = update }