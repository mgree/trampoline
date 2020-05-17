- [ ] Convert to rewrite system version
  
  related:
  
  `type Fueled a = Thunk (Gas -> Either (Gas,a) (Fueled a)) | Computed a`
  
- [ ] More serious demos

  + [ ] Lambda calculus/Scheme/STLC
  
    * [x] eval reference function

    * [x] CEK stepper function

    * [ ] tests
    
      random generator for exprs, test parser
      
      random generator of well typed exprs
      
      typechecker, evaluator 

    * [ ] source location information (see https://package.elm-lang.org/packages/elm/parser/latest/Parser#getPosition)

    * [ ] hook up to driver:
          
          option to disable typechecking
          
          stepping
          
          interrupt button

    * [ ] add operations on ints
    
    * [ ] compare with timeout-oriented programming?
    
  + [ ] Probability language

- [ ] Way to configure refueling amount and pause time

- [ ] Add timing information to fueled computations.
