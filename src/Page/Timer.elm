module Page.Timer exposing (..)

import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
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
    , length = 10
    , timePassed = 1
    }


type Model
    = Stopped (List Timer) Timer
    | Paused (List Timer) Timer (List Timer)
    | Running (List Timer) Timer (List Timer)
    | Ended Timer (List Timer)


init : Model
init =
    Running (List.repeat 10 defaultTimer) defaultTimer (List.repeat 0 defaultTimer)


type Msg
    = Tick Posix
    | StartPauseClicked


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            case model of
                Running que active done ->
                    let
                        newElapsed =
                            active.timePassed + 1
                    in
                    if timeLeft active > 0 then
                        ( Running que { active | timePassed = newElapsed } done, Cmd.none )

                    else
                        let
                            ( newQue, newActive, newDone ) =
                                tryShiftForward ( que, active, done )
                        in
                        ( Running newQue newActive newDone, Cmd.none )

                _ ->
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
        , Border.innerGlow C.accent2 10
        , padding 10
        , Border.rounded 50
        , BG.color C.darkBase1
        ]
        [ el [ centerX ] <| text <| t.name
        , el [ centerX ] <| text <| timeToString <| timeLeft t
        ]


pausedMain : Timer -> Element Msg
pausedMain t =
    column
        [ Font.color C.accent3
        , Font.size 60
        , width <| fillPortion 3
        , Border.innerGlow C.accent3 10
        , padding 10
        , Border.rounded 50
        , BG.color C.darkBase1
        ]
        [ el [ centerX ] <| text <| t.name
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
        , spacing 5
        ]
    <|
        List.map timerListItem que


timerListItem : Timer -> Element Msg
timerListItem t =
    column
        [ Border.innerGlow C.accent4 2
        , padding 5
        , Border.rounded 10
        , BG.color C.darkBase1
        ]
        [ el [ centerX ] <| text <| t.name
        , el [ centerX ] <| text <| timeToString t.length
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


tryShiftForward : ( List Timer, Timer, List Timer ) -> ( List Timer, Timer, List Timer )
tryShiftForward ( que, active, done ) =
    let
        maybeActive =
            List.head que

        shorterTail =
            Maybe.withDefault [] <| List.tail que

        newDone =
            active :: done
    in
    case maybeActive of
        Just activeTimer ->
            ( shorterTail, activeTimer, newDone )

        Nothing ->
            ( que, active, done )
