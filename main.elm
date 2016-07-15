import Html exposing (..)
import Html.Events exposing (onClick,targetChecked,on)
import Html.Attributes exposing (style,type',checked,src)
import StartApp.Simple as StartApp
import Random exposing (Seed)

--Files .elm
import Utils exposing (..)
import GraphUtils exposing (..)
import Styles exposing (..)
import Debug exposing (..)
import Graph exposing (Graph, toString')
import List exposing (foldr)
import String exposing (append)

{-

Model - Game logic.

-}

port startTime : Float

startTimeSeed : Seed
startTimeSeed = Random.initialSeed <| round startTime

type alias Model = 
    {
    boardSize: Int,
    board : Maybe Matrix,
    seed : Seed,
    status : Status,
    showHint : Bool,
    moves : Int,
    startingMoves : Int
    }

init : Model
init = { boardSize = 6, board = Nothing, seed = startTimeSeed, status = Starting, showHint = False, moves = 20, startingMoves = 20 }

getColorForTile : Tile -> Attribute
getColorForTile t = 
 if t == Yellow then yellowTile
 else if t == Blue then blueTile
 else if t == Red then redTile
 else if t == Orange then orangeTile
 else if t == Purple then purpleTile
 else greenTile

--Generate a random board
getRandomBoard : Seed -> Int -> (Seed, Matrix)
getRandomBoard seed x = randomMatrix seed x x

view address model = 
    case model.status of
        Starting -> getStartingView address model
        InGame -> getGameView address model
        Won -> getFinishedView "You win" address model
        Lost -> getFinishedView "You lose" address model

getGameView : Signal.Address Action -> Model -> Html
getGameView address model = 
    div [center] [
        getViewBoard model,
        getButtons address,
        getRemainingMoves model,
        getSuggestedMoveView address model
    ]

getStartingView : Signal.Address Action -> Model -> Html
getStartingView address model = 
    div [center] 
        [   
            getPlayOptions address model,
            getPlayButton address
        ]

getFinishedView : String -> Signal.Address Action -> Model -> Html
getFinishedView str address model = 
    div[center] 
       [
            getFinishedText str,
            getPlayOptions address model,
            getPlayButton address
       ]

getViewBoard : Model -> Html
getViewBoard model =
    case model.board of
        Nothing -> div [centerMarginTop 40] [text "Error"]
        Just board -> 
            div [centerMarginTop 40] [
                h1 [center] [text "FillZone"],
                table [centerMarginTop 20] (foldr (\x c -> [tr [tableBorder] (
                    foldr (\y d -> [td [getColorForTile (getM board x y)] [text ""]] ++ d) [] [1..model.boardSize])] ++ c) [] [1..model.boardSize])
            ] 

getButtons : Signal.Address Action -> Html
getButtons address = 
    div [] [
        h4 [] [text "Click on a color to change"],
        div [buttonsContainer] [
            div [yellowTileInline,onClick address (ChangeColor Yellow)] [],
            div [blueTileInline,onClick address (ChangeColor Blue)] [],
            div [redTileInline,onClick address (ChangeColor Red)] [],
            div [orangeTileInline,onClick address (ChangeColor Orange)] [],
            div [purpleTileInline,onClick address (ChangeColor Purple)] [],
            div [greenTileInline,onClick address (ChangeColor Green)] []
    ]
  ]

getRemainingMoves : Model -> Html
getRemainingMoves model = 
    div [movesContainer] [text (append "Remaining moves: " (toString model.moves))]

getSuggestedMoveView : Signal.Address Action -> Model -> Html
getSuggestedMoveView address model =
    case model.board of
        Nothing -> div [] []
        Just board ->
            let
                suggestion = getSuggestedMoveStyle board
            in
                case suggestion of
                    Nothing -> div [] []
                    Just s -> 
                        if (model.showHint) then
                            div [movesContainer] [
                                div [] [text "Suggested move: ", div [fst s] []]
                            ]
                        else
                            div [showHintButton, onClick address ShowHint] [text "View Hint"]
    

getPlayOptions : Signal.Address Action -> Model -> Html
getPlayOptions address model= 
     div [] [
            h3 [] [text "Select Board Size: "],
            input [ type' "radio", checked (model.boardSize == 6), on "change" targetChecked (\_ -> Signal.message address (BoardSize 6))] [], 
            text "6x6" , br [] [] ,
            input [ type' "radio", checked (model.boardSize == 7), on "change" targetChecked (\_ -> Signal.message address (BoardSize 7))] []  , 
            text "7x7" , br [] [] ,
            input [ type' "radio", checked (model.boardSize == 8), on "change" targetChecked (\_ -> Signal.message address (BoardSize 8))] []  , 
            text "8x8" , br [] [],
            h3 [] [text "Select Game Mode: "],
            input [ type' "radio", checked (model.startingMoves == 20), on "change" targetChecked (\_ -> Signal.message address (Moves 20))] [], 
            text "Easy" , br [] [] ,
            input [ type' "radio", checked (model.startingMoves == 15), on "change" targetChecked (\_ -> Signal.message address (Moves 15))] []  , 
            text "Medium" , br [] [] ,
            input [ type' "radio", checked (model.startingMoves == 10), on "change" targetChecked (\_ -> Signal.message address (Moves 10))] []  , 
            text "Hard" , br [] []]

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
type Action = ChangeColor Tile | Start | BoardSize Int | Moves Int | ShowHint

update : Action -> Model -> Model
update action model = 
    case action of
        ChangeColor tile -> changeColor model tile
        Start -> 
            let
                newBoard = getRandomBoard model.seed model.boardSize
            in
                {
                    model | board = Just (snd newBoard),
                            seed = fst newBoard,
                            status = InGame,
                            moves = model.startingMoves
                }
        BoardSize x -> { model | boardSize = x }
        Moves x -> { model | startingMoves = x }
        ShowHint -> { model | showHint = True }

changeColor : Model -> Tile -> Model
changeColor model tile =
    case model.board of
        Nothing -> model
        Just board ->
            let 
                updatedBoard = updateBoard board 1 1 (getL (getL board 1) 1) tile
                boardStatus = getBoardStatus updatedBoard
                remainingMoves = model.moves - 1
            in
                if (boardStatus == Won || remainingMoves > 0)
                    then
                        {
                            model | board = Just updatedBoard,
                                    status = boardStatus,
                                    moves = model.moves - 1,
                                    showHint = False
                        }
                    else
                        {
                            model | board = Just updatedBoard,
                                    status = Lost,
                                    moves = 0,
                                    showHint = False
                        }

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