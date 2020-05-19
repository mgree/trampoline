module Simple exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Platform.Sub
import Process
import Task
import Time

import Trampoline as T
import Trampoline.Fueled exposing (Fueled, RunResult(..), run, andThen, return, burn, mkEngine)
import Trampoline.Examples exposing (betterDiverge)

type AppMsg = Tick Time.Posix
type alias AppModel =
    { time : Time.Posix
    }   

type alias Msg = T.Msg (Fueled Int) Int AppMsg
    
initializeEngineCmd : Cmd Msg
initializeEngineCmd =
    Task.perform (\() -> T.SetInput (betterDiverge 0) T.AndWait) (Process.sleep 1.0)

-- sample main tying it together
main = Browser.element 
    { init = T.init (\() -> ({ time = Time.millisToPosix 0 }, initializeEngineCmd)) (Trampoline.Fueled.stepper 1)
    , view = \model ->
        div []
            [ h1 [] [ text "Trampoline example app" ]
            , p [] [ text "This very simple application demonstrates how the mgree/trampoline library works. When you click the 'go' button, a nonterminating computation will start. You'll see a 'gas' counter indicating how much gas has been used so far. (Each tick of 'gas' represents some amount of computation.) When you click 'stop', the computation will be paused. You can click 'resume' to continue it." ]
            , p [] [ text "Note that the 'milliseconds since epoch' readout below continues to tick whether or not the computation is running. That is, the long-running computation doesn't 'pause' everything else." ]
            , div [] (case model.state of
                          T.NoInput -> [ div [] [ text "loading" ] ]
                          T.HasInput _ -> [ div [] [ text "ready" ]
                                          , button [ onClick T.Go ] [ text "go" ] ]
                          T.Running f -> [ div [] [ text "running, used gas: ",
                                                        text <| String.fromInt model.stats.numSteps ]
                                         , button [ onClick T.Stop ] [ text "stop" ]
                                         ]
                          T.Stopped f -> [ div [] [ text "stopped, used gas: ",
                                                        text <| String.fromInt model.stats.numSteps ]
                                         , button [ onClick T.Go ] [ text "resume" ]
                                         ]
                          T.Finished n -> [ text "done (?!) @ ", String.fromInt n |> text ])
            , div []
                [ text "Milliseconds since epoch (to show we're not paused): "
                , em [] [ model.model.time |> Time.posixToMillis |> String.fromInt |> text ]
                ]
            ]
    , update = T.update (\msg model ->
                             case msg of
                                 Tick newTime -> ({ model | time = newTime }, Cmd.none))
    , subscriptions = \model -> Time.every 100 Tick |> Platform.Sub.map T.Inner
    }

