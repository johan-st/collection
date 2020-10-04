port module Page.Timer exposing (..)

import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Page.PrimeFactorization exposing (Model)
import Time exposing (Posix)
import Utils.Color as C


type alias Timer =
    { name : String
    , length : Int
    , timePassed : Int
    }


defaultTimer : String -> Int -> Timer
defaultTimer s i =
    { name = s
    , length = i
    , timePassed = 0
    }


type Model
    = Stopped (List Timer) Timer
    | Paused (List Timer) Timer (List Timer)
    | Running (List Timer) Timer (List Timer)
    | Ended Timer (List Timer)


init : Model
init =
    Running
        [ { name = "Paus"
          , length = 60
          , timePassed = 0
          }
        , { name = "Johan"
          , length = 900
          , timePassed = 0
          }
        , { name = "paus"
          , length = 60
          , timePassed = 0
          }
        , { name = "Viktor"
          , length = 900
          , timePassed = 0
          }
        ]
        (defaultTimer
            "Vlad"
            900
        )
        []


type Msg
    = Tick Posix
    | StartPauseClicked
    | QueTimerClicked Int
    | DoneTimerClicked Int
    | Beep String


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
                        ( Running newQue newActive newDone, sound "correct" )

                _ ->
                    ( model, Cmd.none )

        StartPauseClicked ->
            ( playPause model, sound "click" )

        QueTimerClicked i ->
            ( reccTryShiftForward i model, sound "click" )

        DoneTimerClicked i ->
            ( reccTryShiftBackwardsEntry i model, sound "click" )

        Beep soundString ->
            ( model, sound soundString )


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
        [ wrappedRow
            [ paddingXY 15 100
            , width fill
            , spacing 10
            ]
          <|
            [ queView que, activeTimer color active, doneView done ]
        ]


activeTimer : Color -> Timer -> Element Msg
activeTimer color t =
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
        , centerX
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
        List.indexedMap (queListItem C.accent4) <|
            List.reverse que


doneView : List Timer -> Element Msg
doneView que =
    row
        [ Font.color C.subtle
        , width <| fillPortion 1
        , spacing 5
        ]
    <|
        List.indexedMap (doneListItem C.subtle) que


queListItem : Color -> Int -> Timer -> Element Msg
queListItem c i t =
    el
        [ Font.color c
        , Font.size 20
        , width <| fillPortion 3
        , Border.innerGlow c 3
        , width (px 75)
        , height (px 75)
        , Border.rounded 10
        , BG.color C.darkBase1
        , onClick <| QueTimerClicked i
        ]
        (column
            [ mouseOver [ Border.glow c 3 ]
            , width (px 70)
            , height (px 70)
            , Border.rounded 10
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


doneListItem : Color -> Int -> Timer -> Element Msg
doneListItem c i t =
    el
        [ Font.color c
        , Font.size 20
        , width <| fillPortion 3
        , Border.innerGlow c 3
        , width (px 75)
        , height (px 75)
        , Border.rounded 10
        , BG.color C.darkBase1
        , onClick <| DoneTimerClicked i
        ]
        (column
            [ mouseOver [ Border.glow c 3 ]
            , width (px 70)
            , height (px 70)
            , Border.rounded 10
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
            que
                |> List.head

        shorterTail =
            que
                |> List.tail
                |> Maybe.withDefault []

        newDone =
            active :: done
    in
    case maybeActive of
        Just justActive ->
            ( shorterTail, justActive, newDone )

        Nothing ->
            ( que, active, done )


tryShiftBackwards : ( List Timer, Timer, List Timer ) -> ( List Timer, Timer, List Timer )
tryShiftBackwards ( que, active, done ) =
    let
        maybeActive =
            List.head done

        shorterTail =
            List.tail done
                |> Maybe.withDefault []

        newQue =
            que
                |> (::) active
    in
    case maybeActive of
        Just justActive ->
            ( newQue, justActive, shorterTail )

        Nothing ->
            ( que, active, done )


reccTryShiftForward : Int -> Model -> Model
reccTryShiftForward i m =
    case m of
        Running que active done ->
            if List.length que == i then
                m

            else
                let
                    ( newQue, newActive, newDone ) =
                        tryShiftForward ( que, active, done )
                in
                reccTryShiftForward i (Running newQue newActive newDone)

        Paused que active done ->
            if List.length que == i then
                m

            else
                let
                    ( newQue, newActive, newDone ) =
                        tryShiftForward ( que, active, done )
                in
                reccTryShiftForward i (Paused newQue newActive newDone)

        Stopped que active ->
            if List.length que == i then
                m

            else
                let
                    ( newQue, newActive, newDone ) =
                        tryShiftForward ( que, active, [] )
                in
                reccTryShiftForward i (Running newQue newActive newDone)

        _ ->
            Debug.todo "rcc try shiftFwd"


reccTryShiftBackwardsEntry : Int -> Model -> Model
reccTryShiftBackwardsEntry i m =
    case m of
        Paused _ _ done ->
            let
                targetLength =
                    List.length done - (i + 1)
            in
            reccTryShiftBackwards targetLength m

        Running _ _ done ->
            let
                targetLength =
                    List.length done - (i + 1)
            in
            reccTryShiftBackwards targetLength m

        _ ->
            m


reccTryShiftBackwards : Int -> Model -> Model
reccTryShiftBackwards i m =
    case m of
        Running que active done ->
            if List.length done == i then
                m

            else
                let
                    ( newQue, newActive, newDone ) =
                        tryShiftBackwards ( que, active, done )
                in
                reccTryShiftBackwards i (Running newQue newActive newDone)

        Paused que active done ->
            if List.length done == i then
                m

            else
                let
                    ( newQue, newActive, newDone ) =
                        tryShiftBackwards ( que, active, done )
                in
                reccTryShiftBackwards i (Paused newQue newActive newDone)

        _ ->
            Debug.todo "rcc try shiftBwd"


port sound : String -> Cmd msg
