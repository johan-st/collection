module Page.PrimeFactorization exposing (..)

import Arithmetic exposing (primeFactors)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (onInput)
import Json.Decode as D
import Json.Encode as E
import Utils.Color as C



-- TODO: Make versioned models?


type alias Model =
    { input : String }


init : Model
init =
    { input = "12" }


type Msg
    = InputChanged String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged numberInput ->
            case String.toInt numberInput of
                Just _ ->
                    ( { model | input = numberInput }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    section [ class "kata" ]
        [ div []
            [ h1 [ class "kata__heading" ] [ text "Prime Factorization" ]
            , input [ type_ "number", value model.input, onInput InputChanged ] []
            ]
        , span [ class "kata__result" ] <|
            List.map
                (\int ->
                    text <|
                        " "
                            ++ String.fromInt int
                )
                (primeFactors (Maybe.withDefault 0 (String.toInt model.input)))
        ]


factorize : Int -> Maybe (List Int)
factorize int =
    if int < 2 then
        Nothing

    else
        Just <| fact ( int, [] )


fact : ( Int, List Int ) -> List Int
fact ( int, list ) =
    if int == 0 then
        List.reverse list

    else if modBy 2 int == 0 then
        fact ( int // 2, 2 :: list )

    else if modBy 3 int == 0 then
        fact ( int // 3, 3 :: list )

    else if modBy 5 int == 0 then
        fact ( int // 5, 5 :: list )

    else if modBy 7 int == 0 then
        fact ( int // 7, 7 :: list )

    else if modBy 11 int == 0 then
        fact ( int // 11, 11 :: list )

    else if modBy 13 int == 0 then
        fact ( int // 13, 13 :: list )

    else if modBy 17 int == 0 then
        fact ( int // 17, 17 :: list )

    else if modBy 19 int == 0 then
        fact ( int // 19, 19 :: list )

    else if modBy 21 int == 0 then
        fact ( int // 21, 21 :: list )

    else if modBy 23 int == 0 then
        fact ( int // 23, 23 :: list )

    else if modBy 29 int == 0 then
        fact ( int // 29, 29 :: list )

    else if modBy 31 int == 0 then
        fact ( int // 31, 31 :: list )

    else if modBy 37 int == 0 then
        fact ( int // 37, 37 :: list )

    else if modBy 41 int == 0 then
        fact ( int // 41, 41 :: list )

    else
        List.reverse list



-- ENCODE / DECODE --


modelEncoder : Model -> E.Value
modelEncoder model =
    E.object
        [ ( "input", E.string model.input )
        ]


modelDecoder : D.Decoder Model
modelDecoder =
    D.map Model
        (D.field "input" D.string)
