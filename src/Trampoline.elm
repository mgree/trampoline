module Trampoline exposing
    ( Model
    , Gas
    , StepResult(..)
    , Stepper
    , Msg(..)
    , AndGo(..)
    , State(..)
    , Stats
    , init
    , update
    , subscriptions
    )

{-| The Trampoline library is for writing suspendable computations,
i.e., long-running computations that don't freeze the UI. The library
supports two interfaces: step functions and fueled computations. The
implementation is entirely based around step functions.

- TODO what is a step function?

# Key definitions for writing suspendable computations

Long running computations have two parts: a `Stepper` and an
input. You'll set the stepper when configuring the trampoline state
(see `init`).

@docs Stepper, StepResult

# Messages 

Your program will control the steppers' inputs and when it runs by
sending messages.  Internally, long running computations use private
messages to continue running while allowing other messages to be
processed.

@docs Msg, AndGo

# Models
@docs State, Stats, Model

# Helpers using the stepper

A variety of wrappers will help you manage state and handle only the
messages your program needs to know about.

@docs init, update, subscriptions

# Internals

@docs Gas

-}

import Platform.Sub
import Process
import Task

{-| A `Stepper a o` is a function that takes a value of type `a`
and either:

- isn't yet done, so it returns another value of type `a` (which
  should be stepped more)
- is finished, so it returns a value of type `o`.

For example, a stepper for a programming language might look like:

   type alias ExprStepper = Stepper Expr (Result RuntimeError Value)

The types `a` and `o` need not be different. A long running
computation which always succeeds and produces an integer might use a
stepper like:

   type alias IntStepper = Stepper Int Int
-}
type alias Stepper a o = a -> StepResult a o

{-| The result of a single step: either another `a`, or we're done
with an `o`.
-}
type StepResult a o = Stepping a | Done o

{-| We count steps in terms of `Gas`.
-}
type alias Gas = Int

-- TODO hide the Refuel constructor? can't hide exports, so we need to export functions

{-| Internal messages used by Trampoline's stepper. These actually
need to be hidden, with just `SetInput`, `Go`, and `Stop`, being
exposed.
-}
type Msg a o msg = SetInput a AndGo | Go | Refuel Gas | Stop | Inner msg

{-| Determines whether to immediately start running after `SetInput`
or to wait for `Go` message.
-}
type AndGo = AndGo | AndWait

{-| The current state of the stepper. This information is revealed
completely so you can reflect it best in your program; you should
probably not change it yourself. -}
type State a o = NoInput | HasInput a | Running a | Stopped a | Finished o

{-| The stepper keeps some rudimentary statistics. Eventually, this
information (along with some timing information) will allow for the
stepper to adaptively determine how many steps to take at a time. As
such, while it's currently harmless to change this information, it's
inadvisable. -}
type alias Stats =
    { numSteps : Gas
    , totalSteps : Gas
    }

emptyStats : Stats
emptyStats =
    { numSteps   = 0
    , totalSteps = 0
    }

{-| A `Model a o model` wraps your program's `model` type to
accommodate an `Stepper a o`.
-}
type alias Model a o model =
    { state : State a o
    , stats : Stats
    , stepper : Stepper a o
    , model : model
    }

{-| The `init` wrapper  initializes the outer trampoline state given:

- your program's inner initializer, of type `flags -> (model, Cmd (Msg
  a o msg))`
- a `Stepper a o` for the long running computation

This is meant to be used with Browser.element, e.g.,

    Browser.element { init = Trampoline.init myInit myStepper, ... }

-}
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

resetSteps : Model a o model -> Model a o model
resetSteps model =
    let stats = model.stats in
    { model | stats = { stats | numSteps = 0 } }
                      
countSteps : Gas -> Model a o model -> Model a o model
countSteps steps model =
    let stats = model.stats in
    { model | stats = { stats | numSteps = stats.numSteps + steps, totalSteps = stats.totalSteps + steps } }
        
keepStepping : Model a o model -> Gas -> (msg -> model -> (model, Cmd (Msg a o msg))) -> (o -> msg) -> (Model a o model, Cmd (Msg a o msg))
keepStepping model gas updateInner notify =
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
                             Done o ->
                                 let finishedModel = { model | state = Finished o } |> countSteps (n+1) in
                                 let (modelInnerNew, cmds) = updateInner (notify o) finishedModel.model in
                                 ({ finishedModel | model = modelInnerNew }, cmds)
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

{-| A wrapper for your program's `update` function. You must provide:

- your inner update function, of type `(msg -> model -> (model, Cmd (Msg a o msg)))`
- a function that generates message when stepping is complete, of type `o -> msg`

This is meant to be used with Browser.element, e.g.,

    Browser.element { update = Trampoline.init myUpdate NotifyMsg, ... }

where `NotifyMsg` is some message defined in your application of type `o -> msg`.
-}                
update : (msg -> model -> (model, Cmd (Msg a o msg))) -> (o -> msg) ->
         Msg a o msg -> Model a o model -> (Model a o model, Cmd (Msg a o msg))
update updateInner notify msg model =
    case msg of
        SetInput a andGo ->
            let inputModel = { model | state = HasInput a } in
            case andGo of
                AndGo -> update updateInner notify Go inputModel
                AndWait -> (inputModel, defaultRefuel)
        Go -> doGo model
        Refuel gas -> keepStepping model gas updateInner notify
        Stop -> (doStop model, Cmd.none)
        Inner msgInner ->
            let (modelInnerNew, cmds) = updateInner msgInner model.model in
            ({ model | model = modelInnerNew }, cmds)

{-| A wrapper for your program's `subscriptions` function, which amounts to running `Platform.Sub.map`.
-}
subscriptions : (model -> Platform.Sub.Sub msg) -> Model a o model -> Platform.Sub.Sub (Msg a o msg)
subscriptions subscriptionsInner model = subscriptionsInner model.model |> Platform.Sub.map Inner
