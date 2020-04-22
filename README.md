[![Build Status](https://travis-ci.com/mgree/trampoline.svg?branch=master)](https://travis-ci.com/mgree/trampoline)

Fueled computations in Elm.

The `Fuel` monad allows one to structure computations that use up 'gas', which allows one to write potentially non-terminating code in a relatively direct style. It turns out that these gas-based computations form a monad, so we can define usual functions like `map` and `andThen`.

# Example

See https://mgree.github.io/trampoline for an example of how to use
the trampoline pattern, a way of structuring a program to use `Fuel`:
timers or other events trigger fueled computations; the source code in
this repository, at
[example/src](https://github.com/mgree/trampoline/tree/master/example/src).

