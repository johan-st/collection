module Page.Search exposing (..)

import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Url
import Utils.Color as C


type alias Model =
    { url : Url.Url
    , input : String
    }


type Msg
    = InputChanged String
    | SearchClicked


init : Url.Url -> Model
init url =
    { url = url
    , input = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged input ->
            ( { model | input = input }, Cmd.none )

        SearchClicked ->
            ( { model | input = "" }, Cmd.none )


view : Model -> Element Msg
view model =
    column [ width fill ]
        [ text "Find Things"
        , text "not implemented yet"
        , row [ width fill, spacing 30, paddingXY 30 10 ]
            [ Input.search []
                { onChange = InputChanged
                , text = model.input
                , placeholder = Just (Input.placeholder [] <| text "This search box is not working")
                , label = Input.labelHidden "search"
                }
            , Input.button
                [ padding 15
                , Border.innerGlow C.accent3 3
                ]
                { onPress = Just SearchClicked
                , label = text "My Button"
                }
            ]
        ]
