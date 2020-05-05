module STLC exposing (..)

import Dict exposing (Dict)
import Dict as Dict

import Result as Result

type alias VarName = String

type Type = TInt | TFun Type Type

type alias Ctx = Dict VarName Type

emptyCtx : Ctx
emptyCtx = Dict.empty
    
lookupType : Ctx -> VarName -> Maybe Type
lookupType g x = Dict.get x g         

extendType : Ctx -> VarName -> Type -> Ctx             
extendType g x ty = Dict.insert x ty g
             
type Expr = EVar VarName
          | ENum Int
          | ELam VarName Type Expr
          | EApp Expr Expr

type Value = VNum Int
           | VClo VarName Type Expr Env

type alias Env = Dict VarName Value

emptyEnv : Env
emptyEnv = Dict.empty

lookupVal : Env -> VarName -> Maybe Value
lookupVal env x = Dict.get x env
            
extendVal : Env -> VarName -> Value -> Env
extendVal env x v = Dict.insert x v env
    
type TypeError = NoSuchVariable VarName
               | ExpectedFunction Type Expr
               | ApplicationMismatch Type Expr Type Expr
    
typeOf : Ctx -> Expr -> Result TypeError Type
typeOf g e =
    case e of
        EVar x -> lookupType g x |>
                  Result.fromMaybe (NoSuchVariable x)
        ENum _ ->
            Ok TInt
        ELam x ty1 eBody ->
            case typeOf (extendType g x ty1) eBody of
                Err err -> Err err
                Ok ty2 -> Ok (TFun ty1 ty2)
        EApp e1 e2 ->
            case (typeOf g e1, typeOf g e2) of
                (Err err1, _) -> Err err1
                (_, Err err2) -> Err err2
                (Ok (TFun ty11 ty12), Ok ty2) ->
                    if ty11 == ty2
                    then Ok ty12
                    else Err (ApplicationMismatch ty11 e1 ty2 e2)
                (Ok ty1, _) -> Err (ExpectedFunction ty1 e1)

type RunError = UndefinedVariable VarName
              | AppliedNonFunction Value Expr

eval : Env -> Expr -> Result RunError Value
eval env e =
    case e of
        EVar x -> lookupVal env x
               |> Result.fromMaybe (UndefinedVariable x)
        ENum n -> Ok (VNum n)
        ELam x ty1 eBody -> Ok (VClo x ty1 eBody env)
        EApp e1 e2 ->                                       
            case eval env e1 of
                Err err1 -> Err err1
                Ok (VClo x ty1 eBody envClo) ->
                    case eval env e2 of
                        Err err2 -> Err err2
                        Ok v2 -> eval (extendVal envClo x v2) eBody
                Ok v1 -> Err (AppliedNonFunction v1 e2)

eID : Type -> Expr
eID ty = ELam "x" ty (EVar "x")

type Kont = KEmpty
          | KAppL Expr Kont {- evaluating function, holds unevaluated argument -}
          | KAppR VarName Type Expr Env Kont
            {- evaluating argument, holds evaluated closure -}

type Config = Running Expr Env Kont
            | StepError RunError
            | Done Value

mkConfig : Env -> Expr -> Config
mkConfig env e = Running e env KEmpty

stepRunning : Expr -> Env -> Kont -> Config
stepRunning e env kOuter =              
    case (e, kOuter) of
        (EVar x, k) ->
            case lookupVal env x of
                Nothing -> StepError (UndefinedVariable x)
                Just (VNum n) -> Running (ENum n) env k
                Just (VClo y ty1 eBody envClo) -> Running (ELam y ty1 eBody) envClo k

        (EApp e1 e2, k) ->
            Running e1 env (KAppL e2 k)

        (ELam x ty1 eBody, KAppL eArg k) ->
            Running eArg env (KAppR x ty1 eBody env k)

        (ENum n, KAppL eArg k) ->
            StepError (AppliedNonFunction (VNum n) eArg)

        (ELam argX argTy argBody, KAppR x ty1 eBody envClo k) ->
            Running eBody (extendVal envClo x (VClo argX argTy argBody env)) k

        (ENum n, KAppR x ty1 eBody envClo k) ->
            Running eBody (extendVal envClo x (VNum n)) k

        (ELam x ty body, KEmpty) ->
            Done (VClo x ty body env)
       
        (ENum n, KEmpty) ->
            Done (VNum n)
                 
step : Config -> Maybe Config
step config =
    case config of
        StepError err -> Nothing

        Done val -> Nothing
                    
        Running e env k -> Just (stepRunning e env k)

evalCEK : Env -> Expr -> Result RunError Value
evalCEK initialEnv intialE =
    let loop cfg =
            case cfg of
                Done v -> Ok v
                StepError err -> Err err
                Running e env k -> loop (stepRunning e env k)
    in
        loop (mkConfig initialEnv intialE)

                        
