module GameOfSim exposing (..)

import Html exposing (program)
import Model exposing (Model, defaultModel)
import View exposing (view)
import Msg exposing (Msg)
import Update exposing (update)


init : ( Model, Cmd Msg )
init =
    ( defaultModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none


main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
