module Fuel exposing (..)

type alias Gas = Int

{-
-- the following is a bit closer to what we want...
type Fueled a = Thunk (Gas -> Either (Gas,a) (Fueled a)) | Computed a
-}

type RunResult a = OutOfGas (Fueled a) | Complete a

isOutOfGas : RunResult a -> Bool
isOutOfGas res =
    case res of
        OutOfGas _ -> True
        Complete _ -> False
    
type alias Fueled a = Gas -> (Gas, RunResult a)

run : Fueled a -> Gas -> (Gas, RunResult a)
run engine tank0 =
    if tank0 <= 0
    then (0, OutOfGas engine)
    else engine tank0

runToCompletion : Fueled a -> Gas -> a
runToCompletion engine tankSize =
    case run engine tankSize of
        (tankLeft, OutOfGas nextEngine) -> runToCompletion nextEngine tankSize
        (tankLeft, Complete result) -> result

map : (a -> b) -> Fueled a -> Fueled b
map f engine = \tank0 ->
    let (tank1, result) = run engine tank0 in
    case result of
        OutOfGas k -> (tank1, OutOfGas (map f k))
        Complete v -> (tank1, Complete (f v))

return : a -> Fueled a
return a = \tank0 -> (tank0, Complete a)

ap : Fueled (a -> b) -> Fueled a -> Fueled b
ap engineF engineA = \tank0 ->
    let (tank1, result) = run engineF tank0 in
    case result of
        OutOfGas k -> (tank1, OutOfGas (ap k engineA))
        Complete f -> run (map f engineA) tank1

andThen : (a -> Fueled b) -> Fueled a -> Fueled b
andThen cont engineA = \tank0 ->
    let (tank1, result) = run engineA tank0 in
    case result of
        OutOfGas k -> (tank1, OutOfGas (andThen cont k))
        Complete v -> run (cont v) tank1

burn : Fueled a -> Fueled a
burn engine = \tank0 -> run engine (tank0 - 1)

mkEngine : (() -> Fueled a) -> Fueled a
mkEngine thunk = burn (\tank0 -> thunk () tank0)

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
betterDiverge n = mkEngine (\() -> betterDiverge (n+1))

