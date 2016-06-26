import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style,type',checked,src)
import StartApp.Simple as StartApp

--Files .elm
import Utils exposing (..)
import Styles exposing (..)
import Debug exposing (..)
import List exposing (foldr)
import String exposing (append)

{-

Model - Game logic.

-}

type alias Model = 
    {
    board : Matrix,
    status : Status,
    moves : Int
    }

init : Model
init = { board = getRandomBoard, status = Starting, moves = 10 }

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
        Won -> getFinishedView "You win" address
        Lost -> getFinishedView "You lose" address

getGameView : Signal.Address Action -> Model -> Html
getGameView address model = 
    div [center] [
        getViewBoard model,
        getButtons address,
        getRemainingMoves model
    ]

getStartingView : Signal.Address Action -> Html
getStartingView address = 
    div [center] 
        [   
            getPlayButton address
        ]

getFinishedView : String -> Signal.Address Action -> Html
getFinishedView str address = 
    div[center] 
       [
            getFinishedText str,
            getPlayButton address
       ]

getViewBoard : Model -> Html
getViewBoard model =
    div [centerMarginTop 40] [
        h1 [center] [text "FillZone"],
        table [centerMarginTop 20] (foldr (\x c -> [tr [tableBorder] (
            foldr (\y d -> [td [getColorForTile (getM model.board x y)] [text ""]] ++ d) [] [1..6])] ++ c) [] [1..6])
    ] 

getButtons : Signal.Address Action -> Html
getButtons address = 
    div [buttonsContainer] [
        div [yellowTileInline,onClick address (ChangeColor Yellow)] [],
        div [blueTileInline,onClick address (ChangeColor Blue)] [],
        div [redTileInline,onClick address (ChangeColor Red)] [],
        div [greenTileInline,onClick address (ChangeColor Green)] []
    ]

getRemainingMoves : Model -> Html
getRemainingMoves model = 
    div [movesContainer] [text (append "Remaining moves: " (toString model.moves))]

getPlayButton : Signal.Address Action -> Html
getPlayButton address =
    div [style [("margin", "0 auto"),("width","250px")]] 
    [
        img [src "img/playbutton.jpeg",Html.Attributes.width 250,onClick address Start,style [("margin-top","20px")]] []
    ]

getFinishedText : String -> Html
getFinishedText str = h1 [center] [text str]

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
                            status = InGame,
                            moves = 10
                }

changeColor : Model -> Tile -> Model
changeColor model tile =
    let 
        updatedBoard = updateBoard model.board 1 1 (getL (getL model.board 1) 1) tile
        remainingMoves = model.moves - 1
    in
        if (remainingMoves > 0)
            then
                {
                    model | board = updatedBoard,
                            status = getBoardStatus updatedBoard,
                            moves = model.moves - 1
                }
            else
                {
                    model | board = updatedBoard,
                            status = Lost,
                            moves = 0
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