module Page.FizzBuzz exposing (..)

import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Json.Decode as D
import Json.Encode as E
import Utils.Color as C


type Soda
    = Uncarbonated Int
    | Fizzy
    | Buzzy
    | FizzyBuzzy


type alias Model =
    { slider : Float
    , sliderMax : Float
    }


type Msg
    = SliderMoved Float


init : Float -> Model
init slider =
    { slider = slider
    , sliderMax = 10
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SliderMoved newPos ->
            ( { model | slider = newPos }, Cmd.none )


view : Model -> Element Msg
view model =
    let
        sliderMax =
            model.slider + 100
    in
    column [ width fill ]
        [ el
            [ centerX
            , padding 30
            , Font.size 60
            , Font.color C.accent3
            ]
          <|
            text "welcome to fizzbuzz"
        , Input.slider
            [ BG.color C.darkBase3
            , width fill
            , paddingXY 100 0
            , Border.width 1
            , Border.rounded 50
            ]
            { onChange = SliderMoved
            , label = Input.labelLeft [ paddingXY 5 15 ] <| text "How fizzy you ask?"
            , min = 0
            , max = sliderMax
            , value = model.slider
            , thumb = Input.defaultThumb
            , step = Just 1
            }
        , wrappedRow [ spacing 5 ] <| List.map sodaToEl <| List.map carbonate (List.range 1 (ceiling model.slider))
        ]


sodaToEl : Soda -> Element Msg
sodaToEl soda =
    case soda of
        Uncarbonated int ->
            el [ Font.color C.subtle ] <| text <| String.fromInt int

        Fizzy ->
            el [ Font.color C.accent2 ] <| text <| "Fizz"

        Buzzy ->
            el [ Font.color C.accent4 ] <| text <| "Buzz"

        FizzyBuzzy ->
            el [ Font.color C.accent3 ] <| text <| "FizzBuzz"


carbonate : Int -> Soda
carbonate int =
    if int < 1 then
        Uncarbonated int

    else if modBy 15 int == 0 then
        FizzyBuzzy

    else if modBy 3 int == 0 then
        Fizzy

    else if modBy 5 int == 0 then
        Buzzy

    else
        Uncarbonated int



-- ENCODE / DECODE --


modelEncoder : Model -> E.Value
modelEncoder model =
    E.object
        [ ( "slider", E.float model.slider )
        , ( "sliderMax", E.float model.sliderMax )
        ]


modelDecoder : D.Decoder Model
modelDecoder =
    D.map2 Model
        (D.field "slider" D.float)
        (D.field "sliderMax" D.float)
