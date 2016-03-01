module Utils where

import Random
import Random.Extra
import Random.Float
import Time

{-

Utils - This class contains utility functions

-}

type Tile = Yellow | Blue | Red | Green

type alias Matrix = List (List Tile)

bottom = bottom

getTileFromNumber : number -> Tile
getTileFromNumber x = 
 if x == 0 then Yellow
 else if x == 1 then Blue
 else if x == 2 then Red
 else Green

startTimeSeed : Random.Seed
startTimeSeed = Random.initialSeed <| round Time.timestamp

randomMatrix : Int -> Int -> Matrix
randomMatrix row col = List.repeat row (List.repeat col (fst (Random.generate (Random.map getTileFromNumber (Random.int 0 3)))))

getL : List a -> Int -> a
getL l n = 
    case l of
        [] -> bottom
        x::xs -> if (n == 1) then x else getL xs (n-1) 