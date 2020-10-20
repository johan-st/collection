module Page.Gallery exposing (..)

import Bootstrap.Accordion exposing (Card)
import Bootstrap.Utilities.DomHelper exposing (className)
import Html exposing (..)
import Html.Attributes as Attr exposing (class, classList, placeholder, src, type_)
import Html.Events exposing (onClick, onInput)
import Http
import Array exposing (Array)
import Json.Decode as D
import Json.Encode as E


type alias Model =
    { input : String
    , cards : Array Card
    }


type Image
    = Lorem LoremPix
    | None


type Msg
    = GotImages (Result Http.Error (List Card))
    | SearchClicked
    | CardClicked Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotImages result ->
            case result of
                Ok cards ->
                    ( { model | cards = (Array.fromList cards) }, Cmd.none )

                Err err ->
                    ( model, Cmd.none )

        SearchClicked ->
            ( model, Cmd.none )

        CardClicked index ->
            let
                newCards =
                    toggleCard  index model.cards
            in
            ( { model | cards = newCards }, Cmd.none )


init : Model
init =
    { input = ""
    , cards = Array.initialize 9 (\_ -> (Card loremImage False)) 
    }


toggleCard : Int -> Array Card -> Array Card
toggleCard  index cards =
    let 
        maybeCard = Array.get index cards
    in 
        case maybeCard of    
            Just card ->
                Array.set index ({card | selected = not card.selected}) cards
            Nothing -> cards



-- VIEW


view : Model -> Html Msg
view model =
    section [ class "gallery" ]
        [ h2 [ class "gallery__header" ] [ text "by the power of the unsplash api" ]
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
          (List.map imageCard (Array.toIndexedList model.cards))
        ]


imageCard : (Int,Card) -> Html Msg
imageCard (index,card) =
    div
        [ classList
            [ ( "gallery__card", True )
            , ( "gallery__card--selected", card.selected )
            ]
        , onClick (CardClicked index)
        ]
        [ div [ class "gallery__card-inner" ]
            [ div [ class "gallery__card-front" ]
             [ img [ class "gallery__card-img"
            ,src (imageUrl card.src) ] [] , div[class "gallery__card-front-text"][text "card title"]]
            , div [ class "gallery__card-back" ] [ text "2020 is the best year." ]
            ]
        ]


type alias Card =
    { src : Image
    , selected : Bool
    }


imageUrl : Image -> String
imageUrl img =
    case img of
        Lorem lorem ->
            lorem.url

        None ->
            "404"


type alias LoremPix =
    { title : String
    , url : String
    }


loremImage : Image
loremImage =
    Lorem
        { title = "Just a lorem pixum photo"
        , url = "https://picsum.photos/300/300"
        }


getLorem : Cmd Msg
getLorem =
    Http.get
        { url = "https://elm-lang.org/assets/public-opinion.txt"
        , expect = Http.expectJson GotImages (D.list cardDecoder)
        }



-- ENCODE / DECODE


cardDecoder : D.Decoder Card
cardDecoder =
    D.succeed (Card None False)



-- TODO: model decode / encode


modelEncoder : Model -> E.Value
modelEncoder model =
    E.string "yup"


modelDecoder : D.Decoder Model
modelDecoder =
    D.succeed init
