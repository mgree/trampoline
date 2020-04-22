- [ ] Rewrite system version

  type alias Rewriter a o = a -> Either a o

  `run` function just repeatedly calls "gas" times (or until an o comes out)
  
  `fromStepper :: (a -> Maybe a) -> Rewriter a a`
  
- [ ] More serious demos

  + [ ] Lambda calculus/Scheme
  
  + [ ] Probability language

- [ ] Higher-order update

- [ ] Add timing information to fueled computations to, e.g.,
  adaptively set the fuel.

  + When would we want more or less?
