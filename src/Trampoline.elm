module Trampoline exposing (..)

import Process
import Task

import Trampoline.Fueled exposing (RunResult(..), Gas, Fueled)

type Msg a msg = SetEngine (Fueled a) | Go | Refuel Gas | Stop | Inner msg

type State a = Engineless
             | Idling      (Fueled a)
             | Running Gas (Fueled a)
             | Stopped Gas (Fueled a)
             | Finished a

type alias Model a model =
    { state : State a
    , model : model
    }

init : (flags -> (model, Cmd (Msg a msg))) -> flags -> (Model a model, Cmd (Msg a msg))
init initInner flags =
    let (inner, cmds) = initInner flags in
    ({ state = Engineless, model = inner }, cmds)

-- There's a careful interplay between pauseTime and refuelAmount.
--
-- Make pauseTime too low and no other events get to run... so you can't stop your computation. Bad news.
-- Make pauseTime too high and your fueled computation runs too slowly.
--
-- Make refuelAmount too low and your fueled computation runs too slowly.
-- Make refuelAmount too high and your event loop might get unresponsive.
refuelCmd : Gas -> Float -> Cmd (Msg a msg)
refuelCmd refuelAmount pauseTime =
    Task.perform (\() -> Refuel refuelAmount) (Process.sleep pauseTime)

-- TODO a way to configure this... messages?
defaultRefuel : Cmd (Msg a msg)
defaultRefuel = refuelCmd 5 20.0

doGo : Model a model -> (Model a model, Cmd (Msg a msg))
doGo model =
    case model.state of
        Engineless -> (model, Cmd.none)
        Idling engine -> ({ model | state = Running 0 engine }, defaultRefuel)
        Finished n -> (model, Cmd.none)
        Running used engine -> (model, Cmd.none)
        Stopped used engine -> ({ model | state = Running used engine }, defaultRefuel)

doRefuel : Model a model -> Gas -> (Model a model, Cmd (Msg a msg))
doRefuel model gas =
    case model.state of
        Engineless -> (model, Cmd.none)
        Idling engine -> (model, Cmd.none)
        Finished n -> (model, Cmd.none)
        Running used engine -> 
            let (tank, res) = Trampoline.Fueled.run engine gas in
            case res of 
                OutOfGas newEngine -> ({ model | state = Running (used + gas) newEngine }, defaultRefuel)
                Complete v -> ({ model | state = Finished v }, Cmd.none)
        Stopped used engine -> (model, Cmd.none)

doStop : Model a model -> Model a model
doStop model =
    case model.state of
        Engineless -> model
        Idling engine -> model
        Finished n -> model
        Running used engine -> { model | state = Stopped used engine }
        Stopped used engine -> model
                
update : (msg -> model -> (model, Cmd (Msg a msg))) -> Msg a msg -> Model a model -> (Model a model, Cmd (Msg a msg))
update updateInner msg model =
    case msg of
        SetEngine engine -> ({ model | state = Idling engine }, Cmd.none)
        Go -> doGo model
        Refuel gas -> doRefuel model gas
        Stop -> (doStop model, Cmd.none)
        Inner msgInner ->
            let (modelInnerNew, cmds) = updateInner msgInner model.model in
            ({ model | model = modelInnerNew }, cmds)
