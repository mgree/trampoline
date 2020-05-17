module STLCTests exposing (..)

import STLC exposing (..)

import Test exposing (..)
import Dict
import Expect
import Fuzz exposing (Fuzzer)
import Fuzz
import Shrink exposing (Shrinker)
import Shrink
import Random exposing (Generator)
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
            , fuzz fuzzType "random type round-trip" (\ty -> expectParse parseType (typeToString ty) ty)
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
        , describe "typeOf"
            [ fuzz (normalFormOfType emptyCtx TInt) "closed normal forms of type int"
                  (\e -> Expect.equal (typeOf emptyCtx e) (Ok TInt))
            , fuzz (normalFormOfType emptyCtx (TFun TInt TInt)) "closed normal forms of type int->int"
                  (\e -> Expect.equal (typeOf emptyCtx e) (Ok (TFun TInt TInt)))
            , fuzz (normalFormAndType emptyCtx) "closed normal forms of arbitrary type"
                (\(e, ty) ->  Expect.equal (typeOf emptyCtx e) (Ok ty))
            ]
{-      , describe "eval" []
        , describe "evalCEK" [] -}
        ]

genSizedType : Int -> Generator Type
genSizedType i =
    if i <= 0
    then Random.constant TInt
    else
        genBool |>
        Random.andThen
            (\genFun ->
                 if genFun
                 then Random.map2 TFun (Random.lazy (\_ -> genSizedType (i - 1))) (Random.lazy (\_ -> genSizedType (i - 1)))
                 else Random.constant TInt)

shrinkType : Shrinker Type
shrinkType ty =
    case ty of
        TInt -> Shrink.noShrink TInt
        TFun ty1 ty2 -> Shrink.map (\(ty1S, ty2S) -> TFun ty1S ty2S) (Shrink.tuple (shrinkType, shrinkType) (ty1, ty2))

sizedType : Int -> Fuzzer Type
sizedType i = Fuzz.custom (genSizedType i) shrinkType

fuzzType : Fuzzer Type
fuzzType = sizedType 5

elements : List a -> Fuzzer a
elements l = Fuzz.oneOf (List.map Fuzz.constant l)

genAlphaChar : Generator Char
genAlphaChar = Random.uniform 'a' (String.toList "bcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

genVarChar : Generator Char
genVarChar = Random.uniform 'a' (String.toList "bcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")

genVarName : Generator VarName
genVarName =
    Random.int 0 15 |>
    Random.andThen
        (\len ->
             Random.map2 (\c cs -> String.fromList (c::cs)) genAlphaChar (Random.list len genVarChar))

varName : Fuzzer VarName
varName = Fuzz.custom genVarName Shrink.string

sizedExpr : Int -> Fuzzer Expr
sizedExpr i =
    if i <= 0
    then Fuzz.oneOf [ Fuzz.map ENum Fuzz.int
                    , Fuzz.map EVar varName
                    ]
    else Fuzz.oneOf [ Fuzz.map ENum Fuzz.int
                    , Fuzz.map EVar varName
                    , Fuzz.map3 ELam varName fuzzType (sizedExpr (i - 1))
                    , Fuzz.map2 EApp (sizedExpr (i - 1)) (sizedExpr (i - 1))
                    ]

varsOfType : Ctx -> Type -> List VarName
varsOfType ctx ty =
    ctx |>
    Dict.filter (\x tyX -> ty == tyX) |>
    Dict.keys

freshVar : Ctx -> Generator VarName
freshVar ctx =
    genVarName |>
    Random.andThen
        (\x ->
             if Dict.member x ctx
             then Random.lazy (\_ -> freshVar ctx)
             else Random.constant x)


genBool : Generator Bool
genBool = Random.uniform True [False]

orVarsOfType : Generator Expr -> Ctx -> Type -> Generator Expr
orVarsOfType fuzz ctx ty =
    genBool |>
    Random.andThen
        (\useFuzzer ->
             case (List.map EVar (varsOfType ctx ty), useFuzzer) of
                 ([], _) -> fuzz
                 (x::xs, False) -> Random.uniform x xs
                 (_, True) -> fuzz)

genNormalFormOfType : Ctx -> Type -> Generator Expr
genNormalFormOfType ctx ty =
    let nf = case ty of
                 TInt -> Random.map ENum (Random.int Random.minInt Random.maxInt)
                 TFun ty1 ty2 -> 
                     freshVar ctx |>
                     Random.andThen
                         (\x -> Random.map (ELam x ty1) (genNormalFormOfType (extendType ctx x ty1) ty2))
    in
        orVarsOfType nf ctx ty

normalFormOfType : Ctx -> Type -> Fuzzer Expr
normalFormOfType ctx ty = Fuzz.custom (genNormalFormOfType ctx ty) Shrink.noShrink

genNormalFormAndType : Ctx -> Generator (Expr, Type)
genNormalFormAndType ctx =
    genSizedType 5 |>
    Random.andThen
        (\ty ->
             genNormalFormOfType ctx ty
             |> Random.map (\e -> (e, ty)))

normalFormAndType : Ctx -> Fuzzer (Expr, Type)
normalFormAndType ctx = Fuzz.custom (genNormalFormAndType ctx) Shrink.noShrink
