module Trampoline.Examples exposing 
    ( countDown
    , factorial
    , diverge
    , badDiverge
    , betterDiverge)

{-| Some simple examples of the `Fueled` monad from
`Trampoline.Fueled`.

The [repository on GitHub](https://github.com/mgree/trampoline) has
more examples, as well.

# Converging programs

@docs countDown, factorial

# Diverging programs

@docs diverge, badDiverge, betterDiverge

-}

import Trampoline.Fueled exposing (..)

countDown : Int -> Fueled Int
countDown n = burn <|
    if n <= 0
    then return 0
    else countDown (n-1)

factorial : Int -> Fueled Int
factorial n = burn <|
    if n <= 0
    then return 1
    else factorial (n-1) |> andThen (\res -> 
         return (res * n))

diverge : Int -> Fueled Int
diverge n = burn <| 
    (return (n+1) |> andThen diverge)

-- This function doesn't behave the way you'd hope. Strict evaluation seems to want badDiverge to be a VALUE. 
badDiverge : Int -> Fueled Int
badDiverge n = burn (badDiverge (n+1))

-- The (admittedly gross) solution is to thunk the computation.
betterDiverge : Int -> Fueled Int
betterDiverge n = lazy (\() -> betterDiverge (n+1))


