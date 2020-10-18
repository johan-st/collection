port module Page.Timer exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onDoubleClick)
import Json.Decode as D
import Json.Encode as E
import Page.PrimeFactorization exposing (Model)
import Time exposing (Posix)


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
        []
        (timer "sample" 5 Correct)
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
                    if timeLeft active > 1 then
                        ( Running que { active | timePassed = newElapsed } done, Cmd.none )

                    else if List.length que > 0 then
                        let
                            ( newQue, newActive, newDone ) =
                                tryShiftForward ( que, active, done )
                        in
                        ( Running newQue newActive newDone, soundPort active.sound )

                    else
                        ( Paused que { active | timePassed = active.length } done, soundPort active.sound )

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


view : Model -> Html Msg
view model =
    case model of
        Paused que active done ->
            timersView que active done

        Running que active done ->
            timersView que active done

        Edit que active done ( min, sec ) ->
            editorView que active done ( min, sec )


editorView : List Timer -> Timer -> List Timer -> ( Int, Int ) -> Html Msg
editorView que active done ( min, sec ) =
    section []
        [ text "EDITOR"
        , controls
        ]


editTimer : Timer -> ( Int, Int ) -> Html Msg
editTimer t ( min, sec ) =
    div [] [ text "EDIT TIMER" ]


timersView : List Timer -> Timer -> List Timer -> Html Msg
timersView que active done =
    section []
        [ div
            [ class "timers" ]
            [ queView que
            , activeTimer active
            , doneView done
            ]
        , controls
        ]


controls : Html Msg
controls =
    let
        playOrPause =
            "pp"
    in
    div [ class "timers__control" ]
        [ button [ class "timers__button", onClick StartPauseClicked ] [ text playOrPause ]
        , button [ class "timers__button", onClick ResetActive ] [ text "reset" ]
        , button [ class "timers__button", onClick ResetAll ] [ text "reset all" ]
        , button [ class "timers__button", onClick EditTimersClicked ] [ text "edit" ]
        , button [ class "timers__button", onClick <| Beep Click ] [ text "click" ]
        , button [ class "timers__button", onClick <| Beep Coin ] [ text "coin" ]
        , button [ class "timers__button", onClick <| Beep Correct ] [ text "correct" ]
        , button [ class "timers__button", onClick <| Beep Jingle ] [ text "jingle" ]
        ]


resetButton : Html Msg
resetButton =
    div [] [ text "reset all" ]


editButton : Html Msg
editButton =
    --     ]
    div [] [ text "Editor on/off" ]


activeTimer : Timer -> Html Msg
activeTimer t =
    div [ class "timers__timer" ]
        [ div [ class "timers__name" ] [ text t.name ]
        , div [ class "timers__time-left" ] [ text <| String.fromInt (t.length - t.timePassed) ]
        ]


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


editQueView : List Timer -> Html Msg
editQueView que =
    div [] [ text "EDIT QUE VIEW" ]


editDoneView : List Timer -> Html Msg
editDoneView que =
    div [] [ text "EDIT DONE VIEW" ]


queView : List Timer -> Html Msg
queView que =
    div [ class "timer__que" ]
        [ text "que"
        , div [] <|
            List.indexedMap queListItem <|
                List.reverse que
        ]


doneView : List Timer -> Html Msg
doneView que =
    section []
        [ text "done view"
        , div [] (List.indexedMap doneListItem que)
        ]


queListItem : Int -> Timer -> Html Msg
queListItem i t =
    div [] [ text "QUE ITEM VIEW" ]


doneListItem : Int -> Timer -> Html Msg
doneListItem i t =
    div []
        [ text "DONE ITEM VIEW" ]



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



-- ENDODE / DECODE


modelDecoder : D.Decoder Model
modelDecoder =
    D.field "state" D.string
        |> D.andThen modelDecoderHelper


modelDecoderHelper : String -> D.Decoder Model
modelDecoderHelper state =
    case state of
        "Running" ->
            D.map3 Paused
                (D.field "que" (D.list timerDecoder))
                (D.field "active" timerDecoder)
                (D.field "done" (D.list timerDecoder))

        "Paused" ->
            D.map3 Paused
                (D.field "que" (D.list timerDecoder))
                (D.field "active" timerDecoder)
                (D.field "done" (D.list timerDecoder))

        _ ->
            D.fail "invalid timer state"


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
        -- Stopped que act ->
        --     E.object
        --         [ ( "state", E.string "Stopped" )
        --         , ( "que", E.list timerEncoder que )
        --         , ( "active", timerEncoder act )
        --         ]
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
