module Fuel exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Process
import Task
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

type Msg = Go | Refuel Gas | Stop | Tick Time.Posix
type State = Inert | Running Gas (Fueled Int) | Stopped Gas (Fueled Int) | Finished Int
type alias Model =
  { state : State
  , time : Time.Posix
  }

-- There's a careful interplay between pauseTime and refuelAmount.
--
-- Make pauseTime too low and no other events get to run... so you can't stop your computation. Bad news.
-- Make pauseTime too high and your fueled computation runs too slowly.
--
-- Make refuelAmount too low and your fueled computation runs too slowly.
-- Make refuelAmount too high and your event loop might get unresponsive.
refuelCmd : Gas -> Float -> Cmd Msg
refuelCmd refuelAmount pauseTime = Task.perform (\() -> Refuel refuelAmount) (Process.sleep pauseTime)

defaultRefuel : Cmd Msg
defaultRefuel = refuelCmd 5 20.0

doGo : Model -> (Model, Cmd Msg)
doGo model =
    case model.state of
        Inert -> ({ model | state = Running 0 (betterDiverge 20) }, defaultRefuel)
        Finished n -> (model, Cmd.none)
        Running used engine -> (model, Cmd.none)
        Stopped used engine -> ({ model | state = Running used engine }, defaultRefuel)

doRefuel : Model -> Gas -> (Model, Cmd Msg)
doRefuel model gas =
    case model.state of
        Inert -> (model, Cmd.none)
        Finished n -> (model, Cmd.none)
        Running used engine -> 
            let (tank, res) = run engine gas in
            case res of 
                OutOfGas newEngine -> ({ model | state = Running (used + gas) newEngine }, defaultRefuel)
                Complete v -> ({ model | state = Finished v }, Cmd.none)
        Stopped used engine -> (model, Cmd.none)

doStop : Model -> Model
doStop model =
    case model.state of
        Inert -> model
        Finished n -> model
        Running used engine -> { model | state = Stopped used engine }
        Stopped used engine -> model

main = Browser.element 
    { init = \() -> ({ state = Inert, time = Time.millisToPosix 0 }, Cmd.none)
    , view = \model ->
        div []
            [ case model.state of
                Inert -> button [ onClick Go ] [ text "go" ]
                Running used f -> div [] [ text "running, used gas: ", String.fromInt used |> text 
                                         , button [ onClick Stop ] [ text "stop" ]
                                         ]
                Stopped used f -> div [] [ text "stopped, used gas: ", String.fromInt used |> text 
                                         , button [ onClick Go ] [ text "resume" ]
                                         ]
                Finished n -> div [] [ text "done @ ", String.fromInt n |> text ]
            , h1 [] [ model.time |> Time.posixToMillis |> String.fromInt |> text ]
            ]
    , update = \msg model -> 
        case msg of
            Go -> doGo model
            Refuel gas -> doRefuel model gas
            Stop -> (doStop model, Cmd.none)
            Tick newTime -> ({ model | time = newTime }, Cmd.none)
    , subscriptions = \model -> Time.every 100 Tick
    }