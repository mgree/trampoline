module Trampoline.Fueled.Internal exposing (..)

import Trampoline as T
import Trampoline.Internal exposing (..)

type RunResult a = OutOfGas (Fueled a) | Complete a

type alias Fueled a = Gas -> (Gas, RunResult a)

defaultTankSize : Gas
defaultTankSize = defaultSteps

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

stepper : Gas -> T.Stepper (Fueled a) a
stepper tankSize engine =
    let (_, result) = run engine tankSize in
    case result of
        OutOfGas nextEngine -> T.Stepping nextEngine
        Complete a -> T.Done a
