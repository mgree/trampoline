module STLC exposing (..)

import Char
import Dict exposing (Dict)
import Dict
import Result
import Set exposing (Set)
import Set

import Parser exposing (Parser, symbol, (|.), (|=), succeed, lazy, spaces, keyword)
import Parser

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

import Trampoline as T

------------------------------------------------------------------------
-- DRIVER
------------------------------------------------------------------------

type Msg = SetProgramText String

type ProgramState = PSUnparsed (List Parser.DeadEnd)
                  | PSIllTyped Expr TypeError
                  | PSTypeChecked Expr Type

type RunState = RSUnstarted
              | RSRunning Expr (Maybe Type)
              | RSCompleted Expr (Maybe Type) Value

                    
type alias Model =
    { programText : String
    , programState : ProgramState
    , runState : RunState
    }

withProgramText : String -> Model -> Model
withProgramText programText model =
    let programState =
            case Parser.run parseExpr programText of
                Err err -> PSUnparsed err
                Ok expr ->
                    case typeOf emptyCtx expr of
                        Err err -> PSIllTyped expr err
                        Ok ty -> PSTypeChecked expr ty
    in
        { model
            | programText = programText
            , programState = programState
        }
    
main = Browser.element { init = init
                       , view = view
                       , update = update
                       , subscriptions = subscriptions
                       }

init : () -> (Model, Cmd Msg)       
init () = (initialModel, Cmd.none)

initialModel : Model
initialModel = { programText = ""
               , programState = PSUnparsed []
               , runState = RSUnstarted
               }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

update : Msg -> Model -> (Model, Cmd Msg)               
update msg model =
    case msg of
        SetProgramText programText -> (model |> withProgramText programText, Cmd.none)

view : Model -> Html Msg
view model =
    div []
    [ h1 [] [text "STLC interpreter"]
    , div []
        [ textarea [ id "program"
                   , rows 15
                   , style "width" "100%"
                   , onInput SetProgramText
                   ]
              [ text model.programText ]
        , div [ id "programstate" ]
              [ viewProgramState model.programState ]
        ]
    ]

viewProgramState : ProgramState -> Html Msg
viewProgramState programState =
    case programState of
        PSUnparsed err ->
            div [ class "error" ]
                [ span [] [ text "Parse error:" ]
                , span [] [ text <| deadEndsToString err ]
                ]
        PSIllTyped expr err ->
            div [ class "error" ]
                [ span [] [ text "Type error: " ]
                , span [] [ text <| typeErrorToString err ]
                ]
        PSTypeChecked expr ty ->
            div [ class "success" ]
                [ span [] [ text ("Well typed: " ++ typeToString ty) ]
                ]

deadEndsToString : List Parser.DeadEnd -> String
deadEndsToString deadEnds =
    let
        deadEndToString : Parser.DeadEnd -> String
        deadEndToString deadEnd =
            let
                position : String
                position =
                    "row:" ++ String.fromInt deadEnd.row ++ " col:" ++ String.fromInt deadEnd.col ++ "\n"
            in
            case deadEnd.problem of
                Parser.Expecting str ->
                    "Expecting " ++ str ++ "at " ++ position

                Parser.ExpectingInt ->
                    "Expecting integer at " ++ position

                Parser.ExpectingHex ->
                    "Expecting hexadecimal number at " ++ position

                Parser.ExpectingOctal ->
                    "Expecting octal number at " ++ position

                Parser.ExpectingBinary ->
                    "Expecting binary at " ++ position

                Parser.ExpectingFloat ->
                    "Expecting float at " ++ position

                Parser.ExpectingNumber ->
                    "Expecting number at " ++ position

                Parser.ExpectingVariable ->
                    "Expecting variable at " ++ position

                Parser.ExpectingSymbol str ->
                    "Expecting symbol " ++ str ++ " at " ++ position

                Parser.ExpectingKeyword str ->
                    "Expecting keyword " ++ str ++ "at " ++ position

                Parser.ExpectingEnd ->
                    "Expecting end of input at " ++ position

                Parser.UnexpectedChar ->
                    "Unexpected character at " ++ position

                Parser.Problem str ->
                    "Error: " ++ str ++ " at " ++ position

                Parser.BadRepeat ->
                    "Internal error (BadRepeat) at " ++ position
    in
    List.foldl (++) "" (List.map deadEndToString deadEnds)

------------------------------------------------------------------------
-- EXPRESSIONS AND TYPES
------------------------------------------------------------------------

type alias VarName = String

type Type = TInt | TFun Type Type

type Expr = EVar VarName
          | ENum Int
          | ELam VarName Type Expr
          | EApp Expr Expr

eID : Type -> Expr
eID ty = ELam "x" ty (EVar "x")

typeToString : Type -> String
typeToString ty =
    let
        funToString tyOuter =
            case tyOuter of
                TFun lhs rhs -> baseToString lhs ++ " -> " ++ funToString rhs
                _ -> baseToString tyOuter

        baseToString tyOuter =
            case tyOuter of
                TInt -> "int"
                _ -> "(" ++ funToString tyOuter ++ ")"
    in
        funToString ty

exprToString : Expr -> String
exprToString expr =
    let
        lambdaToString eOuter =
            case eOuter of
                ELam x ty1 eBody ->
                    "\\" ++ x ++ ":" ++ typeToString ty1 ++ ". " ++ exprToString eBody

                _ -> appToString eOuter

        appToString eOuter =
            case eOuter of
                EApp e1 e2 -> appToString e1 ++ " " ++ atomToString e2
                _ -> atomToString eOuter

        atomToString eOuter = 
            case eOuter of
                EVar x -> x

                ENum n -> String.fromInt n

                _ -> "(" ++ lambdaToString eOuter ++ ")"
    in
        lambdaToString expr                            

------------------------------------------------------------------------
-- TYPE CHECKING
------------------------------------------------------------------------
            
type alias Ctx = Dict VarName Type

emptyCtx : Ctx
emptyCtx = Dict.empty
    
lookupType : Ctx -> VarName -> Maybe Type
lookupType g x = Dict.get x g         

extendType : Ctx -> VarName -> Type -> Ctx             
extendType g x ty = Dict.insert x ty g

type TypeError = NoSuchVariable VarName
               | ExpectedFunction Type Expr
               | ApplicationMismatch Type Expr Type Expr

typeErrorToString : TypeError -> String
typeErrorToString err =
    case err of
        NoSuchVariable x -> "No such variable " ++ x
        ExpectedFunction ty e ->
            "Expected " ++ exprToString e ++
                " to be a function, but it has type " ++ typeToString ty
        ApplicationMismatch ty11 e1 ty2 e2 ->
            exprToString e1 ++ " expects an argument of type " ++ typeToString ty11 ++
                " but " ++ exprToString e2 ++ " has type " ++ typeToString ty2

                 
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

------------------------------------------------------------------------
-- CONVENTIONAL, RECURSIVE INTERPRETER
------------------------------------------------------------------------
                 
type Value = VNum Int
           | VClo VarName Type Expr Env

type alias Env = Dict VarName Value

emptyEnv : Env
emptyEnv = Dict.empty

lookupVal : Env -> VarName -> Maybe Value
lookupVal env x = Dict.get x env
            
extendVal : Env -> VarName -> Value -> Env
extendVal env x v = Dict.insert x v env
        
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

------------------------------------------------------------------------
-- CEK MACHINE/STEPPER
------------------------------------------------------------------------
         
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

                        
------------------------------------------------------------------------
-- PARSER
------------------------------------------------------------------------

keywords : Set String
keywords = Set.singleton "int"

parseType : Parser Type
parseType =
    succeed (\(lhs, rhs) -> List.foldr TFun rhs lhs)
        |= sepByR1 parseAtomicType (spaces |. symbol "->" |. spaces)
               
parseAtomicType : Parser Type
parseAtomicType =
    succeed identity
        |. spaces
        |= Parser.oneOf
           [ succeed TInt |. keyword "int"
           , parens (lazy (\_ -> parseType))
           ]

parseExpr : Parser Expr
parseExpr =
    succeed (\(lhs, rhs) -> List.foldl (\r l -> EApp l r) lhs rhs)
        |= sepByL1 parseAtomicExpr spaces

parseAtomicExpr : Parser Expr
parseAtomicExpr =
    succeed identity
        |. spaces
        |= Parser.oneOf
           [ succeed ENum |= Parser.int
           , succeed EVar |= parseVar
           , succeed ELam |. symbol "\\" |. spaces |= parseVar |. spaces |. symbol ":" |. spaces |= parseType |. spaces |. symbol "." |. spaces |= lazy (\_ -> parseExpr)
           , parens (lazy (\_ -> parseExpr))
           ]
                 
parseVar : Parser String
parseVar = Parser.variable { start = Char.isAlpha
                           , inner = \c -> Char.isAlphaNum c || c == '_'
                           , reserved = keywords
                           }

parens : Parser a -> Parser a
parens p = succeed identity |. spaces |. symbol "(" |. spaces |= p |. spaces |. symbol ")"


sepByR1 : Parser a -> Parser sep -> Parser (List a, a)
sepByR1 parseItem parseSep =
    let helper items =
            parseItem |>
            Parser.andThen
                (\item ->
                     Parser.oneOf
                     [ succeed (Parser.Loop (item::items)) |. parseSep
                     , succeed (Parser.Done (items, item))
                     ])
    in
        Parser.loop [] helper |>
        Parser.map (\(items, item) -> (List.reverse items, item))

sepEndBy : Parser a -> Parser sep -> Parser (List a)
sepEndBy parseItem parseSep =
    let helper items =
            Parser.oneOf
                [ succeed (\item -> Parser.Loop (item::items))
                      |= parseItem
                      |. parseSep
                , succeed (Parser.Done items)
                ]
    in
        Parser.loop [] helper |>
        Parser.map List.reverse
            
sepByL1 : Parser a -> Parser sep -> Parser (a, List a)
sepByL1 parseItem parseSep =
    succeed (\item items -> (item, items))
            |= parseItem 
            |= sepEndBy parseItem parseSep           
