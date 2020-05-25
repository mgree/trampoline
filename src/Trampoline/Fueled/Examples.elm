module Trampoline.Fueled.Examples exposing 
    ( countDown
    , factorial
    , diverge
    , badDiverge
    , betterDiverge
    , bestDiverge
    )

import Debug
import Trampoline.Fueled exposing (..)

countDown : Int -> Fueled Int
countDown n = 
    if n <= 0
    then return 0
    else call countDown (n-1)

factorial : Int -> Fueled Int
factorial n = 
    if n <= 0
    then return 1
    else call factorial (n-1) |> andThen (\res -> 
         return (res * n))

diverge : Int -> Fueled Int
diverge n = burn <|
    (return (n+1) |> andThen diverge)

-- This function doesn't behave the way you'd hope. Strict evaluation seems to want badDiverge to be a VALUE. 
badDiverge : Int -> Fueled Int
badDiverge n = burn (badDiverge (n+1))

-- The (admittedly gross) solution is to thunk the computation.
betterDiverge : Int -> Fueled Int
betterDiverge n = burn <| lazy (\() -> betterDiverge (n+1))

bestDiverge : Int -> Fueled Int
bestDiverge n = call bestDiverge (n+1)

