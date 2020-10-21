module Page.Visuals exposing (..)

import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Utils.Color as C


type Msg
    = NoOp


type alias Model =
    String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


init : Model
init =
    "ready"


view : Model -> Html Msg
view _ =
    layout [] <|
        wrappedRow
            [ width fill
            , centerY
            , spaceEvenly
            , padding 50
            , Font.color C.darkBase3
            ]
            [ el
                [ BG.color C.accent1
                , width (px 100)
                , height (px 100)
                , Border.innerShadow { blur = 6, color = C.highlight, offset = ( 4, 4 ), size = 4 }
                , Border.roundEach { topLeft = 15, topRight = 35, bottomRight = 15, bottomLeft = 35 }
                ]
              <|
                el
                    [ width fill
                    , height fill
                    , Border.innerShadow { blur = 6, color = C.shadow, offset = ( -2, -2 ), size = 4 }
                    , Border.roundEach { topLeft = 15, topRight = 35, bottomRight = 15, bottomLeft = 35 }
                    ]
                <|
                    el [ centerX, centerY ] <|
                        text "accent 1"
            , el
                [ BG.color C.accent2
                , width (px 100)
                , height (px 100)
                , Border.shadow { blur = 0, color = C.shadow, offset = ( 4, 4 ), size = 0 }
                , Border.roundEach { topLeft = 15, topRight = 35, bottomRight = 15, bottomLeft = 35 }
                ]
              <|
                el [ centerX, centerY ] <|
                    text "accent 2"
            , el
                [ BG.color C.accent3
                , width (px 100)
                , height (px 100)
                , Border.roundEach { topLeft = 15, topRight = 35, bottomRight = 15, bottomLeft = 35 }
                ]
              <|
                el [ centerX, centerY ] <|
                    text "accent 3"
            , el
                [ BG.color C.accent4
                , width (px 100)
                , height (px 100)
                , Border.roundEach { topLeft = 15, topRight = 35, bottomRight = 15, bottomLeft = 35 }
                ]
              <|
                el [ centerX, centerY ] <|
                    text "accent 4"
            ]
