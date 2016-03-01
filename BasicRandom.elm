module BasicRandom where

import Random

import Time
import Signal

seed : Signal Random.Seed
seed = (\ (t, _) -> Random.initialSeed <| round t) <= Time.timestamp (Signal.constant ())

randomList : Random.Seed -> List Int
randomList elems max = 
    let (ls, _) = Random.generate (Random.list elems (Random.int 0 max)) seed
    in ls