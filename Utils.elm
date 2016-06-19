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

{-
startTimeSeed : Random.Seed
startTimeSeed = Random.initialSeed <| round Time.timestamp
-}

randomMatrix : Int -> Int -> Matrix
{- randomMatrix row col = List.repeat row (List.repeat col (fst (Random.generate (Random.map getTileFromNumber (Random.int 0 3))))) -}
randomMatrix row col = [[getTileFromNumber 0, getTileFromNumber 1, getTileFromNumber 2, getTileFromNumber 3],[getTileFromNumber 2, getTileFromNumber 2, getTileFromNumber 1, getTileFromNumber 3],[getTileFromNumber 0, getTileFromNumber 2, getTileFromNumber 3, getTileFromNumber 0],[getTileFromNumber 1, getTileFromNumber 1, getTileFromNumber 3, getTileFromNumber 2]]

getL : List a -> Int -> a
getL l n = 
    case l of
        [] -> bottom
        x::xs -> if (n == 1) then x else getL xs (n-1)

putL : List a -> a -> Int -> List a
putL l a n = 
    case l of
        [] -> []
        x::xs -> if (n == 1) then (a :: xs) else x :: putL xs a (n-1)

sizeOf : List a -> Int
sizeOf l = List.foldr (\x c -> c + 1) 0 l

getM : Matrix -> Int -> Int -> Tile
getM m r c = getL (getL m r) c

putM : Matrix -> Int -> Int -> Tile -> Matrix
putM m r c elem = 
    case m of
        [] -> bottom
        x::xs -> if (r == 1) then (putL x elem c) :: xs else x :: putM xs (r-1) c elem 