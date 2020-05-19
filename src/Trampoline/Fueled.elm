module Trampoline.Fueled exposing
    ( RunResult(..)
    , isOutOfGas
    , Fueled
    , stepper
    , run
    , runToCompletion
    , map
    , return
    , ap
    , andThen
    , burn
    , mkEngine
    )

import Trampoline exposing (Gas, StepResult(..), Stepper)

type RunResult a = OutOfGas (Fueled a) | Complete a

type alias Fueled a = Gas -> (Gas, RunResult a)

stepper : Gas -> Stepper (Fueled a) a
stepper tankSize engine =
    let (_, result) = run engine tankSize in
    case result of
        OutOfGas nextEngine -> Stepping nextEngine
        Complete a -> Done a
    
isOutOfGas : RunResult a -> Bool
isOutOfGas res =
    case res of
        OutOfGas _ -> True
        Complete _ -> False

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
