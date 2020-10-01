module Page.Timer exposing (..)

import Element exposing (..)
import Time exposing (Posix)


type alias Timer =
    { length : Int
    , timePassed : Int
    }


defaultTimer : Timer
defaultTimer =
    { length = 10
    , timePassed = 0
    }


type Model
    = Stopped (List Timer) Timer
    | Paused (List Timer) Timer (List Timer)
    | Running (List Timer) Timer (List Timer)
    | Ended Timer (List Timer)


init : Model
init =
    Stopped (List.repeat 2 defaultTimer) defaultTimer


type Msg
    = Tick Posix
    | StartPauseClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( model, Cmd.none )

        StartPauseClicked ->
            ( model, Cmd.none )


playPause : Model -> Model
playPause model =
    case model of
        Stopped que active ->
            Running que active []

        Paused que active done ->
            Running que active done

        Running que active done ->
            Paused que active done

        Ended active done ->
            Ended active done


view : Model -> Element Msg
view model =
    text "hello"
