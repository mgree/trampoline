[![Build Status](https://travis-ci.com/mgree/trampoline.svg?branch=master)](https://travis-ci.com/mgree/trampoline)

Fueled computations in Elm.

The `Fuel` monad allows one to structure computations that use up 'gas', which allows one to write potentially non-terminating code in a relatively direct style. It turns out that these gas-based computations form a monad, so we can define usual functions like `map` and `andThen`.

The `Trampoline` idiom is a way of structuring a program to use `Fuel`: timers or other events trigger fueled computations.

# TODO

- We could add timing information to fueled computations to adaptively set the fuel.
  + When would we want more or less?
- We could have fueled computations have a notion of intermediate result.
  + More management in the monad, but would allow for nicer UIs.
