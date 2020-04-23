module Trampoline.Rewrite exposing (..)

import Trampoline.Fueled exposing (RunResult(..), Gas, Fueled)

type Output i o = Running i | Done o

type alias Rewriter i o = i -> Output i o

type alias Steps = Int
    
run : Rewriter i o -> i -> Steps -> Output i o
run step i steps =
    if steps <= 0
    then Running i
    else case step i of
             Running iNext -> run step iNext (steps - 1)
             Done o        -> Done o

runToCompletion : Rewriter i o -> i -> o
runToCompletion step i =
    case step i of
        Running iNext -> runToCompletion step iNext
        Done o        -> o

liftMaybe : (a -> Maybe a) -> Rewriter a a
liftMaybe step =
    \a ->
        case step a of
            Nothing    -> Done a
            Just aNext -> Running aNext

liftFueled : Gas -> Rewriter (Fueled a) a
liftFueled tankSize = \engine ->
    case Trampoline.Fueled.run engine tankSize of
        (tankLeft, OutOfGas nextEngine) -> Running nextEngine
        (tankLeft, Complete a)          -> Done a
        
