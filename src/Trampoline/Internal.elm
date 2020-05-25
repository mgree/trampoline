module Trampoline.Internal exposing (..)

{-| We count steps in terms of `Gas`.
-}
type alias Gas = Int

defaultSteps : Gas
defaultSteps = 5

defaultPauseTime : Float
defaultPauseTime = 20.0

