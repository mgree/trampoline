module STLCTests exposing (..)

import STLC exposing (..)

import Test exposing (..)
import Expect
import Fuzz
import Random

import Parser exposing (Parser)
import Parser

parsesAs : Parser a -> String -> a -> Test
parsesAs p s expected =
    test s (\() -> case (Parser.run p s) of
                       Err err -> Expect.fail ("expected successful parse of " ++ s)
                       Ok v -> Expect.equal v expected)

suite : Test
suite =
    describe "STLC"
        [ describe "parseType"
              [ parsesAs parseType "int" TInt
              , parsesAs parseType "(int)" TInt
              , parsesAs parseType "int->int" (TFun TInt TInt)
              , parsesAs parseType " int    ->\n\rint" (TFun TInt TInt)
              , parsesAs parseType "(int->int)" (TFun TInt TInt)
              , parsesAs parseType "(int->int)->int" (TFun (TFun TInt TInt) TInt)
              , parsesAs parseType "int->int->int" (TFun TInt (TFun TInt TInt))
              , parsesAs parseType "int->(int->int)" (TFun TInt (TFun TInt TInt))
              , parsesAs parseType "((int->(int->int)))" (TFun TInt (TFun TInt TInt))
              , parsesAs parseType "int->(int->int)->int" (TFun TInt (TFun (TFun TInt TInt) TInt))
              ]
{-        , describe "parseExpr" []
        , describe "typeOf" []
        , describe "eval" []
        , describe "evalCEK" [] -}
        ]
