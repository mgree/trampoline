module Fuel exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Time

type alias Gas = Int

{-
-- the following is a bit closer to what we want...
type Fueled a = Thunk (Gas -> Either (Gas,a) (Fueled a)) | Computed a
-}

type RunResult a = OutOfGas (Fueled a) | Complete a

type alias Fueled a = Gas -> (Gas, RunResult a)

run : Fueled a -> Gas -> (Gas, RunResult a)
run engine tank0 =
    if tank0 <= 0
    then (0, OutOfGas engine)
    else engine tank0

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

type Msg = Go | Tick Time.Posix
type Model = Inert | Running Gas (Fueled Int) | Finished Int

initialTank : Gas
initialTank = 5

doTick : Model -> Model
doTick model =
    case model of
        Inert -> Inert
        Finished n -> Finished n
        Running used engine -> 
            let (tank, res) = run engine initialTank in
            case res of 
                OutOfGas newEngine -> Running (used + initialTank) newEngine
                Complete v -> Finished v

main = Browser.element 
    { init = \() -> (Inert, Cmd.none)
    , view = \model ->
        case model of
            Inert -> button [ onClick Go ] [ text "go" ]
            Running used f -> div [] [ text "running, used gas: ", String.fromInt used |> text ]
            Finished n -> div [] [ text "done @ ", String.fromInt n |> text ]
    , update = \msg model -> 
        case msg of
            Go -> (Running 0 (factorial 20), Cmd.none)
            Tick _ -> (doTick model, Cmd.none)
    , subscriptions = \_ -> Time.every 50 Tick
    }