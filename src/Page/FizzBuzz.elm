module Page.FizzBuzz exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (onInput)
import Json.Decode as D
import Json.Encode as E
import Utils.Color as C


type Soda
    = Uncarbonated Int
    | Fizzy
    | Buzzy
    | FizzyBuzzy


type alias Model =
    { input : Int
    }


type Msg
    = InputChanged String


init : Model
init =
    { input = 16
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged input ->
            let maybeInt = String.fromInt input 
            
            in
            case maybeInt of 
                Just int ->
                    ( { model | input = int }, Cmd.none )
                Nothing -> 
                    (model , Cmd.none )



view : Model -> Html Msg
view model =
    section []
        [ h1 [] [ text "fizzbuzzzer" ]
        , input [type_ "number" , value (String.fromInt model.input), onInput InputChanged ] []
        , span [] <| List.map sodaToEl <| List.map carbonate (List.range 1 ( model.input))
        ]



sodaToEl : Soda -> Html Msg
sodaToEl soda =
    case soda of
        Uncarbonated int ->
            span [] [ text (String.fromInt int) ]

        Fizzy ->
            span [] [ text "Fizz" ]

        Buzzy ->
            span [] [ text "Buzz" ]

        FizzyBuzzy ->
            span [] [ text "FizzBuzz" ]


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
        [ ( "slider", E.int model.input )
        ]


modelDecoder : D.Decoder Model
modelDecoder =
    D.map Model
        (D.field "slider" D.int)
