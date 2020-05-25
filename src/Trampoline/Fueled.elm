module Trampoline.Fueled exposing
    ( Fueled
    , stepper
    , runToCompletion
    , map
    , return
    , ap
    , andThen
    , burn
    , call
    , lazy
    )

{-| In addition to supporting `Stepper a o` functions, it's possible
to write "fueled" computations. A `Fueled a` runs some number of steps
and either `isOutOfGas` or yields a value of type `a`.

For those familiar with the functor/applicative/monad family, `Fueled`
is a monad, and that is the best way to interact with it.

There are two ways to run programs: all the way, via
`runToCompletion`, or by passing the `stepper` to `Trampoline.init`.

# Examples of `Fueled` computations

The [repository on GitHub](https://github.com/mgree/trampoline) has
more examples. The basic idiom of Fueled computations can be seen in
the following function:

    countDown : Int -> Fueled Int
    countDown n = 
        if n <= 0
        then return 0
        else call countDown (n-1)

A few notes:

  1. Your `Fueled` functions should never take `Fueled` arguments. Use `andThen`.
  2. Rather than simply returning a value, use `return`.
  3. Rather than making a recursive call, use `call`.

The `countDown` example is pretty silly---it always returns 0. Boring. Here's a marginally less boring example:

    factorial : Int -> Fueled Int
    factorial n = 
        if n <= 0
        then return 1
        else call factorial (n-1) |> andThen (\res -> 
             return (res * n))

We can add a note:

  4. Use `andThen` for sequencing.

Here are three diverging functions, in reverse order of goodness:

    bestDiverge : Int -> Fueled Int
    bestDiverge n = call bestDiverge (n+1)

Simple, delightful. Like [Charlie on the MTA](https://www.youtube.com/watch?v=S7Jw_v3F_Q0), it never returns---but it doesn't blow up the stack.
    
    betterDiverge : Int -> Fueled Int
    betterDiverge n = burn <| lazy (\() -> betterDiverge (n+1))
    
The `call` function amounts to exactly this idiom.

    recoveringMonadAddictDiverge : Int -> Fueled Int
    recoveringMonadAddictDiverge n = burn <|
        (return (n+1) |> andThen diverge)

Using `return` into `andThen` is a pretty weird thing to do, but doing
it here has the same effect as using `lazy`.

    badDiverge : Int -> Fueled Int
    badDiverge n = burn (badDiverge (n+1))

This function will blow up your stack---it won't actually suspend properly! Elm uses eager evaluation, so it has to compute `badDiverge (n+1)` before calling `burn`, which means computing `badDiverge (n+2)`, and so on.

# Key definitions for writing fueled computations

@docs Fueled, stepper, runToCompletion

# Generic monadic operations

@docs map, return, ap, andThen

# Fueled-specific monadic operations

@docs call, burn, lazy

-}

import Trampoline as T
import Trampoline.Internal exposing (..)
import Trampoline.Fueled.Internal exposing (..)


{-| A `Fueled a` is a suspendable computation that will either diverge
or eventually yield a value of type `a`. The `Fueled` type is abstract
to avoid shenanigans.
-}
type alias Fueled a = Trampoline.Fueled.Internal.Fueled a


{-| Fueled computations have a stepper that can be used with
`Trampoline.init`. To run a fueled computation, you might write:

   main = Browser.element { init = Trampoline.init myInit Trampoline.Fueled.stepper, ... }

somewhere else, you can then run `Trampoline.setInput
myFueledComputation AndGo` to set the fueled computation to run and
start stepping.
-}
stepper : T.Stepper (Fueled a) a
stepper = Trampoline.Fueled.Internal.stepper defaultTankSize

{-| Fueled computations can also be simply run... at the risk of
freezing the UI thread or blocking messages. Use this function carefully.
-}
runToCompletion : Fueled a -> a
runToCompletion engine =
    case run engine defaultTankSize of
        (tankLeft, OutOfGas nextEngine) -> runToCompletion nextEngine
        (tankLeft, Complete result) -> result

{-| Post-processing the results of a `Fueled` computation.
-}
map : (a -> b) -> Fueled a -> Fueled b
map f engine = \tank0 ->
    let (tank1, result) = run engine tank0 in
    case result of
        OutOfGas k -> (tank1, OutOfGas (map f k))
        Complete v -> (tank1, Complete (f v))

{-| A fueled computation that returns immediately.

   runToCompletion (return x) == x
-}
return : a -> Fueled a
return a = \tank0 -> (tank0, Complete a)

{-| Combining two `Fueled` computations applicatively.
-}
ap : Fueled (a -> b) -> Fueled a -> Fueled b
ap engineF engineA = \tank0 ->
    let (tank1, result) = run engineF tank0 in
    case result of
        OutOfGas k -> (tank1, OutOfGas (ap k engineA))
        Complete f -> run (map f engineA) tank1

{-| Meant to be used with `(|>)`, as in:

    f |> Trampoline.Fueled.andThen (\x -> ...)

To bind the result of `f` as `x`.
-}
andThen : (a -> Fueled b) -> Fueled a -> Fueled b
andThen cont engineA = \tank0 ->
    let (tank1, result) = run engineA tank0 in
    case result of
        OutOfGas k -> (tank1, OutOfGas (andThen cont k))
        Complete v -> run (cont v) tank1

{-| Makes a suspendable call to a `Fueled` computation. All recursive
calls should be written:

    f n = ... call f (n-1) ...
-}
call : (a -> Fueled b) -> a -> Fueled b
call f a = burn <| lazy (\_ -> f a)

{-| Delays a Fueled computation.
-}
lazy : (() -> Fueled a) -> Fueled a
lazy thunk = (\tank0 -> thunk () tank0)

{-| Uses up a bit of fuel.
-}
burn : Fueled a -> Fueled a
burn engine = \tank0 -> run engine (tank0 - 1)
