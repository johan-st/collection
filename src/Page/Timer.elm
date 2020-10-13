port module Page.Timer exposing (..)

import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events exposing (onClick, onDoubleClick)
import Element.Font as Font
import Element.Input as Input
import Html exposing (input)
import Json.Decode as D
import Json.Encode as E
import Page.PrimeFactorization exposing (Model)
import Time exposing (Posix)
import Utils.Color as C


type alias Timer =
    { name : String
    , length : Int
    , timePassed : Int
    , sound : Sound
    }


defaultTimer : Timer
defaultTimer =
    Timer "timer" 540 0 Correct


type Model
    = Paused (List Timer) Timer (List Timer)
    | Running (List Timer) Timer (List Timer)
    | Edit (List Timer) Timer (List Timer) ( Int, Int )


init : Model
init =
    Paused
        []
        defaultTimer
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
    | DeleteTimer
    | AddTimer


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            case model of
                Running done active que ->
                    let
                        newElapsed =
                            active.timePassed + 1
                    in
                    if timeLeft active > 0 then
                        ( Running done { active | timePassed = newElapsed } que, Cmd.none )

                    else
                        let
                            ( newDone, newActive, newQue ) =
                                tryShiftForward ( done, active, que )
                        in
                        ( Running newDone newActive newQue, soundPort active.sound )

                _ ->
                    ( model, Cmd.none )

        StartPauseClicked ->
            ( playPause model, soundPort Click )

        QueTimerClicked i ->
            ( reccTryShiftForwardEntry i model, soundPort Click )

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
                Edit done active que ( min, sec ) ->
                    let
                        newActive =
                            { active | name = name }
                    in
                    ( Edit done newActive que (toMinSec newActive), soundPort Click )

                _ ->
                    ( model, Cmd.none )

        TimerMinuteChanged newMin ->
            case model of
                Edit done active que ( min, sec ) ->
                    case String.toInt newMin of
                        Just int ->
                            if int < 99 then
                                let
                                    newActive =
                                        { active | timePassed = 0, length = int * 60 + sec }
                                in
                                ( Edit done newActive que (toMinSec newActive), soundPort Click )

                            else
                                ( Edit done active que (toMinSec active), soundPort Wrong )

                        Nothing ->
                            ( Edit done active que ( 0, sec ), soundPort Wrong )

                _ ->
                    ( model, Cmd.none )

        TimerSecondChanged newSec ->
            case model of
                Edit done active que ( min, sec ) ->
                    case String.toInt newSec of
                        Just int ->
                            if int < 60 then
                                let
                                    newActive =
                                        { active | timePassed = 0, length = min * 60 + int }
                                in
                                ( Edit done newActive que (toMinSec newActive), soundPort Click )

                            else
                                ( Edit done active que ( min, sec ), soundPort Wrong )

                        Nothing ->
                            ( Edit done active que ( min, 0 ), soundPort Wrong )

                _ ->
                    ( model, Cmd.none )

        DeleteTimer ->
            case model of
                Edit done active que ( min, sec ) ->
                    case que of
                        head :: tail ->
                            ( Edit done head tail (toMinSec head), soundPort Click )

                        _ ->
                            case done of
                                head :: tail ->
                                    ( Edit tail head que (toMinSec head), soundPort Click )

                                _ ->
                                    ( Edit [] defaultTimer [] (toMinSec defaultTimer), soundPort Wrong )

                _ ->
                    ( model, Cmd.none )

        AddTimer ->
            case model of
                Edit done active que ( min, sec ) ->
                    ( Edit done defaultTimer (active :: que) ( min, sec ), soundPort Click )

                _ ->
                    ( model, Cmd.none )


resetActive : Model -> Model
resetActive model =
    case model of
        Paused done active que ->
            Paused done { active | timePassed = 0 } que

        Running done active que ->
            Paused done { active | timePassed = 0 } que

        Edit done active que _ ->
            Paused done { active | timePassed = 0 } que



--  TODO: Have reset all move all the way back and pause the timers


resetAll : Model -> Model
resetAll model =
    case model of
        Paused done active que ->
            Paused
                (List.map (\t -> { t | timePassed = 0 }) done)
                { active | timePassed = 0 }
                (List.map (\t -> { t | timePassed = 0 }) que)

        Running done active que ->
            Paused
                (List.map (\t -> { t | timePassed = 0 }) done)
                { active | timePassed = 0 }
                (List.map (\t -> { t | timePassed = 0 }) que)

        Edit done active que ( min, sec ) ->
            Paused
                (List.map (\t -> { t | timePassed = 0 }) done)
                { active | timePassed = 0 }
                (List.map (\t -> { t | timePassed = 0 }) que)


playPause : Model -> Model
playPause model =
    case model of
        Paused done active que ->
            Running done active que

        Running done active que ->
            Paused done active que

        Edit done active que ( min, sec ) ->
            Edit done active que ( min, sec )


editToggle : Model -> Model
editToggle model =
    case model of
        Paused done active que ->
            Edit done active que (toMinSec active)

        Running done active que ->
            Edit done active que (toMinSec active)

        Edit done active que _ ->
            Paused done active que


view : Model -> Element Msg
view model =
    case model of
        Paused done active que ->
            timersView C.accent3 done active que

        Running done active que ->
            timersView C.accent2 done active que

        Edit done active que ( min, sec ) ->
            editorView done active que ( min, sec )


editorView : List Timer -> Timer -> List Timer -> ( Int, Int ) -> Element Msg
editorView done active que ( min, sec ) =
    column [ centerX ]
        [ wrappedRow
            [ paddingXY 15 100, width fill, spacing 10 ]
            [ editDoneView done, editTimer active ( min, sec ), editQueView que ]
        , row [ padding 15, spacing 10, centerX ]
            [ resetButton, editButton ]
        , row [ padding 15, spacing 10, centerX ]
            [ deleteButton, addButton ]
        ]


bigFont =
    Font.size 50


smallFont =
    Font.size 17


editTimer : Timer -> ( Int, Int ) -> Element Msg
editTimer t ( min, sec ) =
    el
        [ Font.color C.accent1
        , bigFont
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
                , label = Input.labelHidden "name"
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
timersView color done active que =
    column [ centerX ]
        [ row
            [ paddingXY 15 100, width fill, spacing 10 ]
            [ doneView done, activeTimer color active, queView que ]
        , row [ padding 15, spacing 10, centerX ]
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


deleteButton : Element Msg
deleteButton =
    el
        [ Border.rounded 10
        , Border.width 1
        , padding 20
        , centerX
        , onClick DeleteTimer
        , pointer
        , mouseOver
            [ Border.glow C.accent3 5 ]
        ]
        (text "delete selected")


addButton : Element Msg
addButton =
    el
        [ Border.rounded 10
        , Border.width 1
        , padding 20
        , centerX
        , onClick AddTimer
        , pointer
        , mouseOver
            [ Border.glow C.accent2 5 ]
        ]
        (text "add timer")


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
        , bigFont
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
            , el [ centerX, centerY, bigFont ] <|
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
editDoneView done =
    row
        [ Font.color C.accent1
        , width <| fillPortion 1
        , spacing 5
        ]
    <|
        List.indexedMap (doneListItem C.accent1) done


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
doneView done =
    row
        [ Font.color C.subtle
        , width <| fillPortion 1
        , spacing 5
        ]
    <|
        List.indexedMap (doneListItem C.subtle) done


queListItem : Color -> Int -> Timer -> Element Msg
queListItem c i t =
    el
        [ Font.color c
        , smallFont
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
            , el [ centerX, centerY, smallFont ] <|
                text <|
                    timeToString <|
                        timeLeft t
            ]
        )


doneListItem : Color -> Int -> Timer -> Element Msg
doneListItem c i t =
    el
        [ Font.color c
        , smallFont
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
            , el [ centerX, centerY, smallFont ] <|
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
tryShiftForward ( done, active, que ) =
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
            ( newDone, justActive, shorterTail )

        Nothing ->
            ( done, active, que )


tryShiftBackwards : ( List Timer, Timer, List Timer ) -> ( List Timer, Timer, List Timer )
tryShiftBackwards ( done, active, que ) =
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
            ( shorterTail, justActive, newQue )

        Nothing ->
            ( done, active, que )


reccTryShiftForwardEntry : Int -> Model -> Model
reccTryShiftForwardEntry i m =
    case m of
        Paused _ _ que ->
            let
                targetLength =
                    List.length que - (i + 1)
            in
            reccTryShiftForward targetLength m

        -- reccTryShiftForward targetLength m
        Running _ _ que ->
            let
                targetLength =
                    List.length que - (i + 1)
            in
            reccTryShiftForward targetLength m

        Edit _ _ que _ ->
            let
                targetLength =
                    List.length que - (i + 1)
            in
            reccTryShiftForward targetLength m


reccTryShiftForward : Int -> Model -> Model
reccTryShiftForward i m =
    case m of
        Running done active que ->
            if List.length que == i then
                m

            else
                let
                    ( newDone, newActive, newQue ) =
                        tryShiftForward ( done, active, que )

                    targetLength =
                        i
                in
                reccTryShiftForward i (Paused newDone newActive newQue)

        Paused done active que ->
            if List.length que == i then
                m

            else
                let
                    ( newDone, newActive, newQue ) =
                        tryShiftForward ( done, active, que )
                in
                reccTryShiftForward i (Paused newDone newActive newQue)

        Edit done active que ( min, sec ) ->
            if List.length que == i then
                m

            else
                let
                    ( newDone, newActive, newQue ) =
                        tryShiftForward ( done, active, que )
                in
                reccTryShiftForward i (Edit newDone newActive newQue (toMinSec newActive))


toMinSec : Timer -> ( Int, Int )
toMinSec t =
    ( t.length // 60, modBy 60 t.length )


reccTryShiftBackwardsEntry : Int -> Model -> Model
reccTryShiftBackwardsEntry i m =
    case m of
        Paused _ _ _ ->
            let
                targetLength =
                    i
            in
            reccTryShiftBackwards targetLength m

        -- reccTryShiftBackwards targetLength m
        Running _ _ _ ->
            let
                targetLength =
                    i
            in
            reccTryShiftBackwards targetLength m

        Edit _ _ _ _ ->
            let
                targetLength =
                    i
            in
            reccTryShiftBackwards targetLength m


reccTryShiftBackwards : Int -> Model -> Model
reccTryShiftBackwards i m =
    case m of
        Running done active que ->
            if List.length done == i then
                m

            else
                let
                    ( newDone, newActive, newQue ) =
                        tryShiftBackwards ( done, active, que )
                in
                reccTryShiftBackwards i (Paused newDone newActive newQue)

        Paused done active que ->
            if List.length done == i then
                m

            else
                let
                    ( newDone, newActive, newQue ) =
                        tryShiftBackwards ( done, active, que )
                in
                reccTryShiftBackwards i (Paused newDone newActive newQue)

        Edit done active que ( min, sec ) ->
            if List.length done == i then
                m

            else
                let
                    ( newDone, newActive, newQue ) =
                        tryShiftBackwards ( done, active, que )
                in
                reccTryShiftBackwards i (Edit newDone newActive newQue (toMinSec newActive))



-- SOUND


type Sound
    = Click
    | Jingle
    | Coin
    | Correct
    | Wrong


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

        Wrong ->
            soundPortActual "wrong"


port soundPortActual : String -> Cmd msg



-- ENDODE / DECODE


modelDecoder : D.Decoder Model
modelDecoder =
    D.field "state" D.string
        |> D.andThen modelDecoderHelper


modelDecoderHelper : String -> D.Decoder Model
modelDecoderHelper state =
    case state of
        "Paused" ->
            D.map3 Paused
                (D.field "que" (D.list timerDecoder))
                (D.field "active" timerDecoder)
                (D.field "done" (D.list timerDecoder))

        _ ->
            Debug.todo ""


timerDecoder : D.Decoder Timer
timerDecoder =
    D.map4 Timer
        (D.field "name" D.string)
        (D.field "length" D.int)
        (D.field "timePassed" D.int)
        (D.field "sound" (D.andThen soundDecoder D.string))


soundDecoder : String -> D.Decoder Sound
soundDecoder sound =
    case sound of
        "click" ->
            D.succeed Click

        "jingle" ->
            D.succeed
                Jingle

        "coin" ->
            D.succeed Coin

        "correct" ->
            D.succeed Correct

        "wrong" ->
            D.succeed Wrong

        _ ->
            D.fail "Failed to decode sound"


modelEncoder : Model -> E.Value
modelEncoder model =
    case model of
        Paused que act done ->
            E.object
                [ ( "state", E.string "Paused" )
                , ( "que", E.list timerEncoder que )
                , ( "active", timerEncoder act )
                , ( "done", E.list timerEncoder done )
                ]

        Running que act done ->
            E.object
                [ ( "state", E.string "Running" )
                , ( "que", E.list timerEncoder que )
                , ( "active", timerEncoder act )
                , ( "done", E.list timerEncoder done )
                ]

        _ ->
            E.string "broken"


timerEncoder : Timer -> E.Value
timerEncoder t =
    E.object
        [ ( "name", E.string t.name )
        , ( "length", E.int t.length )
        , ( "timePassed", E.int t.timePassed )
        , ( "sound", soundEncoder t.sound )
        ]


soundEncoder : Sound -> E.Value
soundEncoder s =
    case s of
        Click ->
            E.string "click"

        Jingle ->
            E.string "jingle"

        Coin ->
            E.string "coin"

        Correct ->
            E.string "correct"

        Wrong ->
            E.string "wrong"
