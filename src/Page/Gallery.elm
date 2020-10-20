module Page.Gallery exposing (..)

import Bootstrap.Accordion exposing (Card)
import Bootstrap.Utilities.DomHelper exposing (className)
import Html exposing (..)
import Html.Attributes as Attr exposing (class, placeholder, src, type_)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Json.Encode as E


loremUrl =
    "https://picsum.photos/300/300"


type alias Model =
    { input : String
    , images : Images
    }


type Images
    = Lorem (List LoremPix)
    | None


type Msg
    = GotImages (Result Http.Error Images)
    | SearchClicked
    | CardClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotImages result ->
            case result of
                Ok images ->
                    ( { model | images = images }, Cmd.none )

                Err err ->
                    ( model, Cmd.none )

        SearchClicked ->
            ( model, Cmd.none )

        CardClicked ->
            ( model, Cmd.none )


init : Model
init =
    { input = ""
    , images = None
    }



-- VIEW


view : Model -> Html Msg
view _ =
    section [ class "gallery" ]
        [ h2 [ class "gallery__header" ] [ text "by the power of unsplash" ]
        , form [ class "gallery_search" ]
            [ input [ class "gallery__input", type_ "text", placeholder "image search" ] []
            , input
                [ class "gallery__submit"
                , type_ "button"
                , Attr.value "show me"
                , onClick SearchClicked
                ]
                []
            ]
        , div [ class "gallery__cardholder" ]
            (List.repeat 9 imageCard)
        ]


imageCard : Html Msg
imageCard =
    img [ class "gallery__card", src loremUrl, onClick CardClicked ] []


type alias LoremPix =
    { title : String
    , url : String
    }


getLorem : Cmd Msg
getLorem =
    Http.get
        { url = "https://elm-lang.org/assets/public-opinion.txt"
        , expect = Http.expectJson GotImages loremDecoder
        }



-- ENCODE / DECODE


loremDecoder : D.Decoder Images
loremDecoder =
    D.succeed (Lorem [])



-- TODO: model decode / encode


modelEncoder : Model -> E.Value
modelEncoder model =
    E.string "yup"


modelDecoder : D.Decoder Model
modelDecoder =
    D.succeed init
