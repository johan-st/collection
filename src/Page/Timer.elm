module Page.Timer exposing (..)

import Element exposing (..)
import Element.Font as Font
import Page.PrimeFactorization exposing (modelEncoder)
import Time exposing (Posix)
import Utils.Color as C


type alias Timer =
    { name : String
    , length : Int
    , timePassed : Int
    }


defaultTimer : Timer
defaultTimer =
    { name = "timer"
    , length = 3600
    , timePassed = 0
    }


type Model
    = Stopped (List Timer) Timer
    | Paused (List Timer) Timer (List Timer)
    | Running (List Timer) Timer (List Timer)
    | Ended Timer (List Timer)


init : Model
init =
    Running (List.repeat 2 defaultTimer) defaultTimer (List.repeat 2 defaultTimer)


type Msg
    = Tick Posix
    | StartPauseClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            case model of
                Running que active done ->
                    ( Running que { active | timePassed = active.timePassed + 1 } done, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        StartPauseClicked ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick


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
    case model of
        Stopped que active ->
            row
                [ paddingXY 30 100
                , width fill
                , spacing 10
                ]
            <|
                [ queView que, pausedMain active ]

        Paused que active done ->
            row
                [ paddingXY 15 100
                , width fill
                , spacing 10
                ]
            <|
                [ queView que, pausedMain active, queView done ]

        Running que active done ->
            row
                [ paddingXY 15 100
                , width fill
                , spacing 10
                ]
            <|
                [ queView que, activeMain active, queView done ]

        _ ->
            Debug.todo "Timer model view cases"


activeMain : Timer -> Element Msg
activeMain t =
    column
        [ Font.color C.accent2
        , Font.size 60
        , width <| fillPortion 3
        ]
        [ el [ centerX ] <| text <| t.name
        , el [ centerX ] <| text <| timeToString t.length
        , el [ centerX ] <| text <| timeToString <| timeLeft t
        ]


pausedMain : Timer -> Element Msg
pausedMain t =
    column
        [ Font.color C.accent3
        , Font.size 60
        , width <| fillPortion 3
        ]
        [ el [ centerX ] <| text <| t.name
        , el [ centerX ] <| text <| timeToString t.length
        , el [ centerX ] <| text <| timeToString <| timeLeft t
        ]


timeLeft : Timer -> Int
timeLeft t =
    let
        diff =
            t.length - t.timePassed
    in
    if diff < 0 then
        Debug.todo "do I need to handle this?"

    else
        diff


queView : List Timer -> Element Msg
queView que =
    row
        [ Font.color C.accent4
        , width <| fillPortion 1
        , spaceEvenly
        ]
    <|
        List.map timerListItem que


timerListItem : Timer -> Element Msg
timerListItem t =
    column []
        [ el [ centerX ] <| text <| t.name
        , el [ centerX ] <| text <| String.fromInt t.length
        , el [ centerX ] <| text <| String.fromInt <| timeLeft t
        ]


timeToString : Int -> String
timeToString timeInSecs =
    let
        min =
            String.fromInt <| timeInSecs // 60

        secIntermediate =
            String.fromInt <| modBy 60 timeInSecs

        sec =
            String.padLeft 2 '0' secIntermediate

        d1 =
            Debug.log "sec" sec
    in
    min ++ ":" ++ sec
