module FuelTests exposing (..)

import Fuel exposing (..)

import Test exposing (..)
import Expect
import Fuzz
import Random

upTo2048 : Fuzz.Fuzzer Int
upTo2048 = Fuzz.intRange 1 2048

runsTo : Fueled a -> a -> Gas -> Expect.Expectation
runsTo engine res tank = Expect.equal res (runToCompletion engine tank)
           
diverges : Fueled a -> Gas -> Expect.Expectation
diverges engine tank = run engine tank |> Tuple.second |> isOutOfGas
                     |> Expect.true "expected divergence"
                        
suite : Test
suite =
    describe "Fuel"
        [
         describe "factorial"
             [ fuzz upTo2048 "runToCompletion fact 0" <|
                   \tank -> runsTo (factorial 0) 1 tank
             , fuzz upTo2048 "runToCompletion fact 1" <|
                   \tank -> runsTo (factorial 1) 1 tank
             , fuzz upTo2048 "runToCompletion fact 5" <|
                   \tank -> runsTo (factorial 5) 120 tank
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
            , fuzz upTo2048 "run diverge 0 out of gas (<=2048 steps)" <|
                  \gas -> diverges (betterDiverge 0) gas
            ]
        ]
