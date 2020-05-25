module FuelTests exposing (..)

import Trampoline exposing (..)
import Trampoline.Fueled exposing (runToCompletion)
import Trampoline.Fueled.Internal exposing (..)
import Trampoline.Internal exposing (..)
import Trampoline.Examples exposing (..)

import Test exposing (..)
import Expect
import Fuzz
import Random

upTo1024 : Fuzz.Fuzzer Int
upTo1024 = Fuzz.intRange 1 1024

runsTo : Fueled a -> a -> Expect.Expectation
runsTo engine res = Expect.equal res (runToCompletion engine)
           
diverges : Fueled a -> Gas -> Expect.Expectation
diverges engine tank = run engine tank |> Tuple.second |> isOutOfGas
                     |> Expect.true "expected divergence"
                        
suite : Test
suite =
    describe "Fuel"
        [
         describe "factorial"
             [ test "runToCompletion fact 0" <|
                   \_ -> runsTo (factorial 0) 1
             , test "runToCompletion fact 1" <|
                   \_ -> runsTo (factorial 1) 1 
             , test "runToCompletion fact 5" <|
                   \_ -> runsTo (factorial 5) 120
             , test "run fact 5 out of gas (1 step)" <|
                   \_ -> run (factorial 5) 1
                      |> Tuple.second
                      |> isOutOfGas
                      |> Expect.true "factorial should not complete running on 1 unit of gas"
             , test "run fact 5 out of gas (3 step)" <|
                   \_ -> run (factorial 5) 3
                      |> Tuple.second
                      |> isOutOfGas
                      |> Expect.true "factorial should not complete running on 3 units of gas"
             ]
        , describe "diverge"
            [ test "run diverge 10 out of gas (10 steps)" <|
                  \_ -> diverges (diverge 10) 10
            , test "run diverge 0 out of gas (50 steps)" <|
                  \_ -> diverges (diverge 0) 50
            , fuzz upTo1024 "run diverge 0 out of gas (<=1024 steps)" <|
                  \gas -> diverges (diverge 0) gas
            ]
        ]
