Fueled computations in Elm.

The `Fuel` monad allows one to structure computations that use up 'gas', which allows one to write potentially non-terminating code in a relatively direct style.

The `Trampoline` idiom is a way of structuring a program to use `Fuel`: timers or other events trigger fueled computations. (This part isn't written yet, but a version of the idiom is in `src/fuel.elm`.)
