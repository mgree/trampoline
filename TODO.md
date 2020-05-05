- [ ] Convert to rewrite system version
  
  related:
  
  `type Fueled a = Thunk (Gas -> Either (Gas,a) (Fueled a)) | Computed a`
  
- [ ] More serious demos

  + [ ] Lambda calculus/Scheme/STLC
  
    * [x] eval reference function

    * [x] CEK stepper function
    
    * [ ] hook up to driver
    
    * [ ] tests
    
    * [ ] compare with timeout-oriented programming?

  + [ ] Probability language

- [ ] Way to configure refueling amount and pause time

- [ ] Add timing information to fueled computations.
