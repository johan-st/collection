module Page.Timer exposing (..)

import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events as Event exposing (onClick)
import Element.Font as Font
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
    , length = 1000
    , timePassed = 1
    }


type Model
    = Stopped (List Timer) Timer
    | Paused (List Timer) Timer (List Timer)
    | Running (List Timer) Timer (List Timer)
    | Ended Timer (List Timer)


init : Model
init =
    Running (List.repeat 2 defaultTimer) defaultTimer (List.repeat 0 defaultTimer)


type Msg
    = Tick Posix
    | StartPauseClicked


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
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
            ( playPause model, Cmd.none )


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
            timersView C.accent4 que active []

        Paused que active done ->
            timersView C.accent3 que active done

        Running que active done ->
            timersView C.accent2 que active done

        _ ->
            Debug.todo "Timer model view cases"


timersView : Color -> List Timer -> Timer -> List Timer -> Element Msg
timersView color que active done =
    column [ centerX ]
        [ row
            [ paddingXY 15 100
            , width fill
            , spacing 10
            ]
          <|
            [ queView que, activeMain color active, queView done ]
        ]


activeMain : Color -> Timer -> Element Msg
activeMain color t =
    el
        [ Font.color color
        , Font.size 60
        , width <| fillPortion 3
        , Border.innerGlow color 10
        , width (px 250)
        , height (px 250)
        , Border.rounded 50
        , BG.color C.darkBase1
        , onClick StartPauseClicked
        ]
        (column
            [ mouseOver [ Border.glow color 10 ]
            , width (px 245)
            , height (px 245)
            , Border.rounded 50
            , centerX
            , centerY
            ]
            [ el [ centerX, centerY ] <| text <| t.name
            , el [ centerX, centerY ] <|
                text <|
                    timeToString <|
                        timeLeft t
            ]
        )


pausedMain : Timer -> Element Msg
pausedMain t =
    column
        [ Font.color C.accent3
        , Font.size 60
        , width <| fillPortion 3
        , Border.innerGlow C.accent3 10
        , padding 30
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
