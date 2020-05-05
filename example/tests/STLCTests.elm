module STLCTests exposing (..)

import STLC exposing (..)

import Test exposing (..)
import Expect
import Fuzz
import Random

import Parser exposing (Parser)
import Parser

expectParse : Parser a -> String -> a -> Expect.Expectation
expectParse p s expected =
    case (Parser.run p s) of
        Err err -> Expect.fail ("expected successful parse of " ++ s)
        Ok v -> Expect.equal v expected

    
parsesAs : Parser a -> String -> a -> Test
parsesAs p s expected =
    test s (\() -> expectParse p s expected)

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
            , fuzz fuzzType "random type round-trip" (\ty -> expectParse parseType (stringOfType ty) ty)
            ]
        , describe "parseExpr"
            [ parsesAs parseExpr "x" (EVar "x")
            , parsesAs parseExpr "xYZ_Q12" (EVar "xYZ_Q12")
            , parsesAs parseExpr "  xYZ_Q13" (EVar "xYZ_Q13")
            , fuzz (Fuzz.intRange 0 1024) "all numbers parse"
                (\n -> expectParse parseExpr (String.fromInt n) (ENum n))
            , parsesAs parseExpr "1 2 3 4 5" (EApp (EApp (EApp (EApp (ENum 1) (ENum 2)) (ENum 3)) (ENum 4)) (ENum 5))
            , parsesAs parseExpr "\\x:int. x" (ELam "x" TInt (EVar "x"))
            ]
{-        , describe "typeOf" []
        , describe "eval" []
        , describe "evalCEK" [] -}
        ]

fuzzType : Fuzz.Fuzzer Type
fuzzType =
    let build i =
            if i <= 0
            then Fuzz.constant TInt
            else Fuzz.frequency [ (1, Fuzz.constant TInt)
                                , (2, Fuzz.map2 TFun (build (i - 1)) (build (i - 1)))
                                ]
    in
        build 5
