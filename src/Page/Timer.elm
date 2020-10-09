port module Page.Timer exposing (..)

import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events exposing (onClick, onDoubleClick)
import Element.Font as Font
import Element.Input as Input
import Html exposing (input)
import Page.PrimeFactorization exposing (Model)
import Time exposing (Posix)
import Utils.Color as C


type alias Timer =
    { name : String
    , length : Int
    , timePassed : Int
    , sound : Sound
    }


timer : String -> Int -> Sound -> Timer
timer name length sound =
    { name = name
    , length = length
    , timePassed = 0
    , sound = sound
    }


type Model
    = Paused (List Timer) Timer (List Timer)
    | Running (List Timer) Timer (List Timer)
    | Edit (List Timer) Timer (List Timer) ( Int, Int )


init : Model
init =
    Paused
        [ timer "short" 60 Coin
        , timer "Vlad" 900 Correct
        , timer "short" 60 Coin
        , timer "Viktor" 900 Correct
        ]
        (timer "Johan" 900 Correct)
        []


type Msg
    = Tick Posix
    | StartPauseClicked
    | QueTimerClicked Int
    | DoneTimerClicked Int
    | Beep Sound
    | ResetAll
    | ResetActive
    | EditTimersClicked
    | TimerNameChanged String
    | TimerMinuteChanged String
    | TimerSecondChanged String


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 25 Tick


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
                        ( Running newQue newActive newDone, soundPort Correct )

                _ ->
                    ( model, Cmd.none )

        StartPauseClicked ->
            ( playPause model, soundPort Click )

        QueTimerClicked i ->
            ( reccTryShiftForward i model, soundPort Click )

        DoneTimerClicked i ->
            ( reccTryShiftBackwardsEntry i model, soundPort Click )

        Beep sound ->
            ( model, soundPort sound )

        ResetAll ->
            ( resetAll model, soundPort Click )

        ResetActive ->
            ( resetActive model, Cmd.none )

        EditTimersClicked ->
            ( editToggle model, soundPort Click )

        TimerNameChanged name ->
            case model of
                Edit que active done ( min, sec ) ->
                    let
                        newActive =
                            { active | name = name }
                    in
                    ( Edit que newActive done (toMinSec newActive), soundPort Click )

                _ ->
                    ( model, Cmd.none )

        TimerMinuteChanged newMin ->
            case model of
                Edit que active done ( min, sec ) ->
                    case String.toInt newMin of
                        Just int ->
                            if int < 99 then
                                let
                                    newActive =
                                        { active | length = int * 60 + sec }
                                in
                                ( Edit que newActive done (toMinSec newActive), Cmd.none )

                            else
                                ( Edit que active done (toMinSec active), Cmd.none )

                        Nothing ->
                            ( Edit que active done ( 0, sec ), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        TimerSecondChanged newSec ->
            case model of
                Edit que active done ( min, sec ) ->
                    case String.toInt newSec of
                        Just int ->
                            if int < 60 then
                                let
                                    newActive =
                                        { active | length = min * 60 + int }
                                in
                                ( Edit que newActive done (toMinSec newActive), Cmd.none )

                            else
                                ( Edit que active done ( min, sec ), Cmd.none )

                        Nothing ->
                            ( Edit que active done ( min, 0 ), Cmd.none )

                _ ->
                    ( model, Cmd.none )


resetActive : Model -> Model
resetActive model =
    case model of
        Paused que active done ->
            Paused que { active | timePassed = 0 } done

        Running que active done ->
            Paused que { active | timePassed = 0 } done

        Edit que active done _ ->
            Paused que { active | timePassed = 0 } done



--  TODO: Have reset all move all the way back and pause the timers


resetAll : Model -> Model
resetAll model =
    case model of
        Paused que active done ->
            Paused
                (List.map (\t -> { t | timePassed = 0 }) que)
                { active | timePassed = 0 }
                (List.map (\t -> { t | timePassed = 0 }) done)

        Running que active done ->
            Paused
                (List.map (\t -> { t | timePassed = 0 }) que)
                { active | timePassed = 0 }
                (List.map (\t -> { t | timePassed = 0 }) done)

        Edit que active done ( min, sec ) ->
            Paused
                (List.map (\t -> { t | timePassed = 0 }) que)
                { active | timePassed = 0 }
                (List.map (\t -> { t | timePassed = 0 }) done)


playPause : Model -> Model
playPause model =
    case model of
        Paused que active done ->
            Running que active done

        Running que active done ->
            Paused que active done

        Edit que active done ( min, sec ) ->
            Edit que active done ( min, sec )


editToggle : Model -> Model
editToggle model =
    case model of
        Paused que active done ->
            Edit que active done (toMinSec active)

        Running que active done ->
            Edit que active done (toMinSec active)

        Edit que active done _ ->
            Paused que active done


view : Model -> Element Msg
view model =
    case model of
        Paused que active done ->
            timersView C.accent3 que active done

        Running que active done ->
            timersView C.accent2 que active done

        Edit que active done ( min, sec ) ->
            editorView que active done ( min, sec )


editorView : List Timer -> Timer -> List Timer -> ( Int, Int ) -> Element Msg
editorView que active done ( min, sec ) =
    column [ centerX ]
        [ wrappedRow
            [ paddingXY 15 100, width fill, spacing 10 ]
            [ editQueView que, editTimer active ( min, sec ), editDoneView done ]
        , wrappedRow [ padding 15, spacing 10, centerX ]
            [ resetButton, editButton ]
        ]


editTimer : Timer -> ( Int, Int ) -> Element Msg
editTimer t ( min, sec ) =
    el
        [ Font.color C.accent1
        , Font.size 60
        , width <| fillPortion 3
        , Border.innerGlow C.accent1 10
        , width (px 250)
        , height (px 250)
        , Border.rounded 50
        , BG.color C.darkBase1
        , centerX
        ]
        (column
            [ mouseOver [ Border.glow C.accent1 10 ]
            , spacing 10
            , width (px 245)
            , height (px 245)
            , Border.rounded 50
            , centerX
            , centerY
            ]
            [ Input.text
                [ width (px 225)
                , height (px 80)
                , padding 10
                , Border.rounded 50
                , centerX
                , centerY
                , BG.color (rgba 0 0 0 0)
                ]
                { onChange = TimerNameChanged
                , text = t.name
                , placeholder = Nothing
                , label = Input.labelHidden "timer label"
                }
            , row [ centerX, centerY ]
                [ Input.text
                    [ width (px 100)
                    , height (px 80)
                    , padding 10
                    , Border.roundEach
                        { topLeft = 50
                        , topRight = 0
                        , bottomLeft = 50
                        , bottomRight = 0
                        }
                    , centerX
                    , centerY
                    , BG.color (rgba 0 0 0 0)
                    ]
                    { onChange = TimerMinuteChanged
                    , text = String.fromInt min
                    , placeholder = Nothing
                    , label = Input.labelHidden "minutes"
                    }
                , Input.text
                    [ width (px 100)
                    , height (px 80)
                    , padding 10
                    , Border.roundEach
                        { topLeft = 0
                        , topRight = 50
                        , bottomLeft = 0
                        , bottomRight = 50
                        }
                    , centerX
                    , centerY
                    , BG.color (rgba 0 0 0 0)
                    ]
                    { onChange = TimerSecondChanged
                    , text = String.fromInt sec
                    , placeholder = Nothing
                    , label = Input.labelHidden "seconds"
                    }
                ]
            ]
        )


timersView : Color -> List Timer -> Timer -> List Timer -> Element Msg
timersView color que active done =
    column [ centerX ]
        [ wrappedRow
            [ paddingXY 15 100, width fill, spacing 10 ]
            [ queView que, activeTimer color active, doneView done ]
        , wrappedRow [ padding 15, spacing 10, centerX ]
            [ resetButton, editButton ]
        ]


resetButton : Element Msg
resetButton =
    el
        [ Border.rounded 10
        , Border.width 1
        , padding 20
        , centerX
        , onClick ResetAll
        , pointer
        , mouseOver
            [ Border.glow C.accent3 5 ]
        ]
        (text "reset all")


editButton : Element Msg
editButton =
    el
        [ Border.rounded 10
        , Border.width 1
        , padding 20
        , centerX
        , onClick EditTimersClicked
        , pointer
        , mouseOver
            [ Border.glow C.accent1 5 ]
        ]
        (text "Editor on/off")


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
        , onDoubleClick ResetActive
        , centerX
        , pointer
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
        0

    else
        diff


editQueView : List Timer -> Element Msg
editQueView que =
    row
        [ Font.color C.accent1
        , width <| fillPortion 1
        , spacing 5
        ]
    <|
        List.indexedMap (queListItem C.accent1) <|
            List.reverse que


editDoneView : List Timer -> Element Msg
editDoneView que =
    row
        [ Font.color C.accent1
        , width <| fillPortion 1
        , spacing 5
        ]
    <|
        List.indexedMap (doneListItem C.accent1) que


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
        , pointer
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
        , pointer
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



-- TODO Refactor away


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

        Edit que active done ( min, sec ) ->
            if List.length que == i then
                m

            else
                let
                    ( newQue, newActive, newDone ) =
                        tryShiftForward ( que, active, done )
                in
                reccTryShiftForward i (Edit newQue newActive newDone (toMinSec newActive))


toMinSec : Timer -> ( Int, Int )
toMinSec t =
    ( t.length // 60, modBy 60 t.length )


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

        Edit _ _ done _ ->
            let
                targetLength =
                    List.length done - (i + 1)
            in
            reccTryShiftBackwards targetLength m


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

        Edit que active done ( min, sec ) ->
            if List.length done == i then
                m

            else
                let
                    ( newQue, newActive, newDone ) =
                        tryShiftBackwards ( que, active, done )
                in
                reccTryShiftBackwards i (Edit newQue newActive newDone (toMinSec newActive))



-- SOUND


type Sound
    = Click
    | Jingle
    | Coin
    | Correct


soundPort : Sound -> Cmd Msg
soundPort sound =
    case sound of
        Click ->
            soundPortActual "click"

        Jingle ->
            soundPortActual "jingle"

        Coin ->
            soundPortActual "coin"

        Correct ->
            soundPortActual "correct"


port soundPortActual : String -> Cmd msg
