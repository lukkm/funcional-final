module GraphUtils where

import Graph exposing (fromNodesAndEdges, member, get, update, Graph, Node, Edge, NodeContext)
import Html exposing (..)
import Styles exposing (..)
import Utils exposing (..)
import IntDict exposing (..)
import List exposing (..)

generateGraph : Matrix -> Graph Int String
generateGraph board = createGraph Graph.empty board 1 1 0 (getM board 1 1)

createGraph : Graph Int String -> Matrix -> Int -> Int -> Int -> Tile -> Graph Int String
createGraph graph board x y incomingWeight incomingColor = 
    let
        pos = ((x - 1) * sizeOf(board)) + y
    in
        if (x < 1 || y < 1 || x > (sizeOf board) || y > (sizeOf board)) then
            graph
        else
            if ((getM board x y) == incomingColor) then
                createGraphStep graph board x y incomingWeight incomingColor
            else
                createGraphStep graph board x y (incomingWeight + 1) (getM board x y)

createGraphStep : Graph Int String -> Matrix -> Int -> Int -> Int -> Tile -> Graph Int String
createGraphStep graph board x y incomingWeight incomingColor =
    let
        pos = ((x - 1) * sizeOf(board)) + y
    in
        if (Graph.member pos graph == True) then
            case Graph.get pos graph of
                Nothing -> bottom {-- Shouldn't happen --}
                Just nodeContext -> 
                    if (nodeContext.node.label <= incomingWeight) then
                        graph
                    else
                        let
                            newGraph = Graph.update pos (\x -> Just (NodeContext (Node pos incomingWeight) IntDict.empty IntDict.empty)) graph
                        in
                           createGraphRec newGraph board x y incomingWeight incomingColor
        else
            let 
                newGraph = Graph.insert (NodeContext (Node pos incomingWeight) IntDict.empty IntDict.empty) graph
            in
                createGraphRec newGraph board x y incomingWeight incomingColor

createGraphRec : Graph Int String -> Matrix -> Int -> Int -> Int -> Tile -> Graph Int String
createGraphRec graph board x y incomingWeight incomingColor = createGraph (createGraph (createGraph (createGraph graph board x (y-1) incomingWeight incomingColor) board (x-1) y incomingWeight incomingColor) board x (y+1) incomingWeight incomingColor) board (x+1) y incomingWeight incomingColor

getGraphSum : Graph Int String -> Int
getGraphSum graph = List.foldr (\x c -> c + x.label) 0 (Graph.nodes graph)

getSuggestedMoveStyle : Matrix -> (Attribute, Int)
getSuggestedMoveStyle board = List.foldr (doGraphFold board) (yellowTileInline, 1000000) [(Yellow, yellowTileInline), (Blue, blueTileInline), (Red, redTileInline), (Green, greenTileInline), (Orange, orangeTileInline), (Purple, purpleTileInline)]

doGraphFold : Matrix -> (Tile, Attribute) -> (Attribute, Int) -> (Attribute, Int)
doGraphFold board currentTile lastAttr = getGraphComparison (updateBoard board 1 1 (getL (getL board 1) 1) (fst currentTile)) (snd currentTile) lastAttr

getGraphComparison : Matrix -> Attribute -> (Attribute, Int) -> (Attribute, Int)
getGraphComparison board newAttr lastPair = 
    let
        newValue = getGraphSum (generateGraph board)
    in
        if (newValue < snd lastPair) then
            (newAttr, newValue)
        else
            lastPair