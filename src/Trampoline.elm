module Trampoline exposing
    ( Model
    , Gas
    , StepResult(..)
    , Stepper
    , Msg(..)
    , AndGo(..)
    , State(..)
    , init
    , update
    , subscriptions
    , refuelCmd
    , doGo
    , keepStepping
    , doStop
    )

import Platform.Sub
import Process
import Task

type StepResult a o = Stepping a | Done o

type alias Stepper a o = a -> StepResult a o


type alias Gas = Int
    
type Msg a o msg = SetInput a AndGo | Go | Refuel Gas | Stop | Inner msg

type AndGo = AndGo | AndWait
    
type State a o = NoInput
               | HasInput a
               | Running  a
               | Stopped  a
               | Finished o

type alias Stats =
    { numSteps : Gas
    }

emptyStats : Stats
emptyStats =
    { numSteps = 0
    }
    
type alias Model a o model =
    { state : State a o
    , stats : Stats
    , stepper : Stepper a o
    , model : model
    }

init : (flags -> (model, Cmd (Msg a o msg))) -> Stepper a o -> flags -> (Model a o model, Cmd (Msg a o msg))
init initInner stepper flags =
    let (inner, cmds) = initInner flags in
    ({ state = NoInput
     , stats = emptyStats
     , stepper = stepper
     , model = inner
     }
    , cmds)

-- There's a careful interplay between pauseTime and refuelAmount.
--
-- Make pauseTime too low and no other events get to run... so you can't stop your computation. Bad news.
-- Make pauseTime too high and your fueled computation runs too slowly.
--
-- Make refuelAmount too low and your fueled computation runs too slowly.
-- Make refuelAmount too high and your event loop might get unresponsive.
refuelCmd : Gas -> Float -> Cmd (Msg a o msg)
refuelCmd refuelAmount pauseTime =
    Task.perform (\() -> Refuel refuelAmount) (Process.sleep pauseTime)

defaultSteps : Gas
defaultSteps = 5

defaultPauseTime : Float
defaultPauseTime = 20.0

-- TODO a way to configure this... messages?
defaultRefuel : Cmd (Msg a o msg)
defaultRefuel = refuelCmd 5 20.0

doGo : Model a o model -> (Model a o model, Cmd (Msg a o msg))
doGo model =
    case model.state of
        NoInput    -> (model, Cmd.none)
        HasInput a -> ({ model | state = Running a }, defaultRefuel)
        Finished o -> (model, Cmd.none)
        Running  a -> (model, Cmd.none)
        Stopped  a -> ({ model | state = Running a }, defaultRefuel)

countSteps : Gas -> Model a o model -> Model a o model
countSteps steps model =
    let stats = model.stats in
    { model | stats = { stats | numSteps = stats.numSteps + steps } }
        
keepStepping : Model a o model -> Gas -> (Model a o model, Cmd (Msg a o msg))
keepStepping model gas =
    case model.state of
        NoInput -> (model, Cmd.none)
        HasInput a -> (model, Cmd.none)
        Finished o -> (model, Cmd.none)
        Running a ->
            let loop n arg =
                    if n >= gas
                    then ({ model | state = Running arg } |> countSteps gas, defaultRefuel)
                    else case model.stepper arg of
                             Stepping newArg -> loop (n+1) newArg
                             Done o -> ({ model | state = Finished o } |> countSteps (n+1), Cmd.none)
            in
                loop 0 a
        Stopped a -> (model, Cmd.none)

doStop : Model a o model -> Model a o model
doStop model =
    case model.state of
        NoInput -> model
        HasInput a -> model
        Finished o -> model
        Running a -> { model | state = Stopped a }
        Stopped a -> model
                
update : (msg -> model -> (model, Cmd (Msg a o msg))) ->
         Msg a o msg -> Model a o model -> (Model a o model, Cmd (Msg a o msg))
update updateInner msg model =
    case msg of
        SetInput a AndGo -> ({ model | state = Running a }, Cmd.none)
        SetInput a AndWait -> ({ model | state = HasInput a }, Cmd.none)
        Go -> doGo model
        Refuel gas -> keepStepping model gas
        Stop -> (doStop model, Cmd.none)
        Inner msgInner ->
            let (modelInnerNew, cmds) = updateInner msgInner model.model in
            ({ model | model = modelInnerNew }, cmds)

subscriptions : (model -> Platform.Sub.Sub msg) -> Model a o model -> Platform.Sub.Sub (Msg a o msg)
subscriptions subscriptionsInner model = subscriptionsInner model.model |> Platform.Sub.map Inner
