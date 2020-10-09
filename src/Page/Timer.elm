port module Page.Timer exposing (..)

import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
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


timer : String -> Int -> Sound -> Timer
timer name length sound =
    { name = name
    , length = length
    , timePassed = 0
    , sound = sound
    }


type Model
    = Stopped (List Timer) Timer
    | Paused (List Timer) Timer (List Timer)
    | Running (List Timer) Timer (List Timer)
    | Ended Timer (List Timer)


init : Model
init =
    Paused
        [ timer "stretch" 6 Coin
        , timer "Vlad" 9 Correct
        , timer "stretch" 6 Coin
        , timer "Viktor" 9 Correct
        ]
        (timer "Johan" 9 Correct)
        []


type Msg
    = Tick Posix
    | StartPauseClicked
    | QueTimerClicked Int
    | DoneTimerClicked Int
    | Beep Sound


subscriptions : Model -> Sub Msg
subscriptions _ =
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
                        ( Running newQue newActive newDone, soundPort active.sound )

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
            timersView C.accent1 que active []

        Paused que active done ->
            timersView C.accent3 que active done

        Running que active done ->
            timersView C.accent2 que active done

        _ ->
            Element.none


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
        0

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


timerListItem : Color -> Int -> Timer -> (Int -> Msg) -> Element Msg
timerListItem accent index timer_ msg =
    el
        [ Font.color accent
        , Font.size 20
        , width <| fillPortion 3
        , Border.innerGlow accent 3
        , width (px 75)
        , height (px 75)
        , Border.rounded 10
        , BG.color C.darkBase1
        , onClick <| msg index
        ]
        (column
            [ mouseOver [ Border.glow accent 3 ]
            , width (px 70)
            , height (px 70)
            , Border.rounded 10
            , centerX
            , centerY
            ]
            [ el [ centerX, centerY ] <| text <| timer_.name
            , el [ centerX, centerY ] <|
                text <|
                    timeToString <|
                        timeLeft timer_
            ]
        )


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
            m


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
            m



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



-- ENDODE / DECODE
-- type Model
--     = Stopped (List Timer) Timer
--     | Paused (List Timer) Timer (List Timer)
--     | Running (List Timer) Timer (List Timer)
--     | Ended Timer (List Timer)


modelDecoder : D.Decoder Model
modelDecoder =
    D.field "state" D.string
        |> D.andThen modelDecoderHelper


modelDecoderHelper : String -> D.Decoder Model
modelDecoderHelper state =
    case state of
        "Stopped" ->
            D.map2 Stopped
                (D.field "que" (D.list timerDecoder))
                (D.field "active" timerDecoder)

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
            D.succeed Jingle

        "coin" ->
            D.succeed Coin

        "correct" ->
            D.succeed Correct

        _ ->
            D.fail "Failed to decode sound"


modelEncoder : Model -> E.Value
modelEncoder model =
    case model of
        Stopped que act ->
            E.object
                [ ( "state", E.string "Stopped" )
                , ( "que", E.list timerEncoder que )
                , ( "active", timerEncoder act )
                ]

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

        Ended act done ->
            E.object
                [ ( "state", E.string "Ended" )
                , ( "active", timerEncoder act )
                , ( "done", E.list timerEncoder done )
                ]


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
