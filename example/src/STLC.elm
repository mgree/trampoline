module STLC exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import Parser

import Trampoline as T

import Trampoline.Fueled as TF

import STLC.Internal exposing (..)

------------------------------------------------------------------------
-- DRIVER
------------------------------------------------------------------------

type AppMsg = SetProgramText String
            | ProgramFinished (Result RunError Value)

type ProgramState = PSUnparsed (List Parser.DeadEnd)
                  | PSIllTyped Expr TypeError
                  | PSTypeChecked Expr Type

type alias AppModel =
    { programText : String
    , programState : ProgramState
    , result : Maybe (Result RunError Value)
    }

withProgramText : String -> AppModel -> AppModel
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

withResult : Result RunError Value -> AppModel -> AppModel
withResult res model = { model | result = Just res }

type alias Msg = T.Msg (TF.Fueled (Result RunError Value)) AppMsg

type alias Model = T.Model (TF.Fueled (Result RunError Value)) (Result RunError Value) AppModel    
        
main =
    Browser.element
        { init = T.init init TF.stepper
        , view = view
        , update = T.update update ProgramFinished
        , subscriptions = T.subscriptions subscriptions
        }

init : () -> (AppModel, Cmd Msg)       
init () = (initialModel, Cmd.none)

initialModel : AppModel
initialModel = { programText  = ""
               , programState = PSUnparsed []
               , result       = Nothing
               }
               
subscriptions : AppModel -> Sub AppMsg
subscriptions model = Sub.none

update : AppMsg -> AppModel -> (AppModel, Cmd Msg)               
update msg model =
    case msg of
        SetProgramText programText -> (model |> withProgramText programText, Cmd.none)
        ProgramFinished res -> (model |> withResult res, Cmd.none)

view : Model -> Html Msg
view model =
    div []
    [ h1 [] [text "STLC interpreter"]
    , div []
        [ textarea [ id "program"
                   , rows 15
                   , style "width" "100%"
                   , onInput (\text -> T.msg <| SetProgramText text)
                   ]
              [ text model.model.programText ]
        , div [ id "programstate" ]
              [ viewProgramState model.model.programState ]
        , div [ id "runstate" ]
              [ div [ id "totalsteps" ]
                  [ text "Total steps: "
                  , text <| String.fromInt model.stats.totalSteps
                  ]
              , viewRunState model
              ]
        , div [ id "result" ]
              ( case model.model.result of
                    Nothing -> []
                    Just res -> [ viewResult res ])
        ]
    , viewGrammar
    ]

viewProgramState : ProgramState -> Html Msg
viewProgramState programState =
    case programState of
        PSUnparsed err ->
            div [ class "error" ]
                [ span [] [ text "Parse error: " ]
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
                , input [ type_ "button", onClick (T.setInput (evalF emptyEnv expr) T.AndGo)
                        , value "Run" ] [ ]
                ]

viewRunState : Model -> Html Msg
viewRunState model =
    case model.state of
        T.NoInput      -> div [] []
        T.HasInput _   -> div [] []
        T.Running  _   -> div [] [ text "Running ("
                                 , text <| String.fromInt model.stats.numSteps
                                 , text " steps)"
                                 , input [ type_ "button", onClick T.stop, value "Stop" ] [ ]
                                 ]
        T.Stopped  cfg -> div [] [ text "Stopped"
                                 , input [ type_ "button", onClick T.go, value "Resume" ] [ ]
                                 ]
        T.Finished _ -> div [] [ text "Done" ]

viewResult : Result RunError Value -> Html Msg
viewResult res =
    case res of
        Err err -> div [] [ text "Error: "
                          , text <|
                              case err of
                                  UndefinedVariable x -> "no such variable " ++ x
                                  AppliedNonFunction v e ->
                                      "tried to apply " ++ valueToString v ++
                                          " to " ++ exprToString e
                          ]
        Ok v -> div [] [ text "Done: "
                       , text <| valueToString v
                       ]
                             
viewGrammar : Html Msg
viewGrammar =
    div [ id "grammar" ]
        [ h3 [] [text "STLC Grammar"]
        , div [] [ text "variable names: [a-zA-Z][a-zA-Z0-9_]*"
                 , p [] [text "an alphabetical character, followed by zero or more alphanumeric characters and underscores, e.g. x, y var7 lambdaTheUltimate_"]]
        , div [] [ text "types: ty ::= int | ty1->ty2"
                 , p [] [text "e.g., (int->int)->int"]
                 ]
        , div [] [text "expressions: e ::= x | n | e1 e2 | \\x:ty. e"]
        ]

