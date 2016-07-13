module Utils where

import Random
import Random exposing (Seed)
import Random exposing (generate, int)
import Random.Extra
import Random.Float
import List exposing (foldr)
import Time

{-

Utils - This class contains utility functions

-}

type Tile = Yellow | Blue | Red | Green | Orange | Purple
type Status = Starting | InGame | Won | Lost

type alias Matrix = List (List Tile)

bottom = bottom

getTileFromNumber : Int -> Tile
getTileFromNumber x = 
 if x == 0 then Yellow
 else if x == 1 then Blue
 else if x == 2 then Red
 else if x == 3 then Orange
 else if x == 4 then Purple
 else Green

randomInt : Seed -> (Int,Seed)
randomInt seed = Random.generate (Random.int 0 5) seed

randomMatrix : Random.Seed -> Int -> Int -> (Seed, Matrix)
randomMatrix seed row col = 
    let 
        range = randomRange seed (row * col)
    in 
        (snd (getL range 1), foldr (\x c -> 
            (foldr (\y d -> (getTileFromNumber (fst (getL range (x * col + y)))) :: d) [] [1..col]) :: c) [] [0..(row-1)])

randomRange : Seed -> Int -> List (Int, Seed)
randomRange initialSeed size = foldr doFold [(0, initialSeed)] [1..size]

updateBoard : Matrix -> Int -> Int -> Tile -> Tile -> Matrix 
updateBoard board x y originalColor nextColor = 
    if (x < 1 || y < 1 || x > (sizeOf board) || y > (sizeOf board) || (getM board x y) /= originalColor || originalColor == nextColor)
        then
            board
        else
            updateBoard (updateBoard (updateBoard (updateBoard (putM board x y nextColor) x (y-1) originalColor nextColor) (x-1) y originalColor nextColor) x (y+1) originalColor nextColor) (x+1) y originalColor nextColor

doFold : Int -> List (Int, Seed) -> List (Int, Seed)
doFold x l = 
    case l of 
        [] -> bottom 
        x::xs -> randomInt (snd x) :: (x::xs)

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