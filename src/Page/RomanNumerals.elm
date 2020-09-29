module Page.RomanNumerals exposing (..)

import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Json.Decode as D
import Json.Encode as E
import Utils.Color as C


type Msg
    = InputChanged String


type alias Model =
    { input : String
    , nums : List Numeral
    }


view : Model -> Element Msg
view { input, nums } =
    column
        [ centerX
        , centerY
        ]
        [ Input.text
            [ width (px 300)
            , centerX
            ]
            { onChange = InputChanged
            , text = input
            , placeholder = Nothing
            , label =
                Input.labelAbove []
                    (text "enter a number (1-3999)")
            }
        , column
            [ centerX
            , padding 30
            ]
            [ el
                [ Font.size 60
                , Font.family [ Font.serif ]
                , Font.color C.accent4
                ]
              <|
                text (numsToString nums)
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged input ->
            case String.toInt input of
                Just int ->
                    if int < 4000 && int > 0 then
                        ( { model | input = input, nums = toRoman int }, Cmd.none )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( { input = "", nums = [] }, Cmd.none )


type Numeral
    = I
    | V
    | X
    | L
    | C
    | D
    | M


numsToString : List Numeral -> String
numsToString nums =
    List.map singleToString nums
        |> List.foldr (++) ""


singleToString : Numeral -> String
singleToString num =
    case num of
        I ->
            "I"

        V ->
            "V"

        X ->
            "X"

        L ->
            "L"

        C ->
            "C"

        D ->
            "D"

        M ->
            "M"


toRoman : Int -> List Numeral
toRoman num =
    toRomanRecc num []


toRomanRecc : Int -> List Numeral -> List Numeral
toRomanRecc x r =
    case x of
        0 ->
            List.reverse r

        _ ->
            if x // 1000 > 0 then
                toRomanRecc (x - 1000) (M :: r)

            else if x // 900 > 0 then
                toRomanRecc (x - 900) (M :: C :: r)

            else if x // 500 > 0 then
                toRomanRecc (x - 500) (D :: r)

            else if x // 400 > 0 then
                toRomanRecc (x - 400) (D :: C :: r)

            else if x // 100 > 0 then
                toRomanRecc (x - 100) (C :: r)

            else if x // 90 > 0 then
                toRomanRecc (x - 90) (C :: X :: r)

            else if x // 50 > 0 then
                toRomanRecc (x - 50) (L :: r)

            else if x // 40 > 0 then
                toRomanRecc (x - 40) (L :: X :: r)

            else if x // 10 > 0 then
                toRomanRecc (x - 10) (X :: r)

            else if x // 9 > 0 then
                toRomanRecc (x - 9) (X :: I :: r)

            else if x // 5 > 0 then
                toRomanRecc (x - 5) (V :: r)

            else if x // 4 > 0 then
                toRomanRecc (x - 4) (V :: I :: r)

            else
                toRomanRecc (x - 1) (I :: r)



-- ENCODE / DECODE --


modelEncoder : Model -> E.Value
modelEncoder model =
    E.object
        [ ( "input", E.string model.input )
        , ( "nums", E.list numeralEncoder model.nums )
        ]


numeralEncoder : Numeral -> E.Value
numeralEncoder num =
    case num of
        M ->
            E.string "M"

        D ->
            E.string "D"

        C ->
            E.string "C"

        L ->
            E.string "L"

        X ->
            E.string "X"

        V ->
            E.string "V"

        I ->
            E.string "I"


modelDecoder : D.Decoder Model
modelDecoder =
    D.map2 Model
        (D.field "input" D.string)
        (D.field "nums" (D.list numeralDecoder))


numeralDecoder : D.Decoder Numeral
numeralDecoder =
    D.string
        |> D.andThen numHelper


numHelper : String -> D.Decoder Numeral
numHelper num =
    case num of
        "M" ->
            D.succeed M

        "D" ->
            D.succeed D

        "C" ->
            D.succeed C

        "L" ->
            D.succeed L

        "X" ->
            D.succeed X

        "V" ->
            D.succeed V

        --   commented out for testing purposes
        -- "I" ->
        --   D.succeed I
        _ ->
            D.fail <|
                "Trying to decode a Numeral, but "
                    ++ num
                    ++ " is not a valid one."



-- infoDecoder4 : Decoder Info
-- infoDecoder3 : Decoder Info
