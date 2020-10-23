module Page.Gallery exposing (..)

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes as Attr exposing (alt, class, classList, placeholder, src, type_)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Json.Encode as E


type alias Model =
    { input : String
    , cards : Array Card
    , pageMeta : UnsplashMeta
    }


type Msg
    = GotUnsplashPage (Result Http.Error UnsplashPage)
    | SearchClicked String
    | CardClicked Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotUnsplashPage result ->
            case result of
                Ok unsplashPage ->
                    ( { model | cards = Array.fromList <| List.map imgToCard unsplashPage.images }, Cmd.none )

                Err err ->
                    ( model, Cmd.none )

        SearchClicked query ->
            ( model, unsplashSearch query )

        CardClicked index ->
            let
                newCards =
                    toggleCard index model.cards
            in
            ( { model | cards = newCards }, Cmd.none )


init : Model
init =
    { input = ""
    , cards = Array.empty
    , pageMeta = UnsplashMeta 1 Nothing Nothing
    }


toggleCard : Int -> Array Card -> Array Card
toggleCard index cards =
    let
        maybeCard =
            Array.get index cards

        newCards =
            Array.map (\c -> { c | selected = False }) cards
    in
    case maybeCard of
        Just card ->
            Array.set index { card | selected = not card.selected } newCards

        Nothing ->
            cards



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
                , Attr.value "get random image"
                , onClick (SearchClicked model.input)
                ]
                []
            ]
        , div [ class "gallery__cardholder" ]
            (List.map imageCard (Array.toIndexedList model.cards))
        ]


imageCard : ( Int, Card ) -> Html Msg
imageCard ( index, card ) =
    div
        [ classList
            [ ( "gallery__card", True )
            , ( "gallery__card--selected", card.selected )
            ]
        , onClick (CardClicked index)
        ]
        [ div [ class "gallery__card-inner" ]
            [ div [ class "gallery__card-front" ]
                [ img
                    [ class "gallery__card-img"
                    , src card.src.url
                    , alt card.src.alt
                    ]
                    []
                , div [ class "gallery__card-front-text" ] [ text (String.fromInt card.src.likes ++ " people liked this") ]
                ]
            , div [ class "gallery__card-back" ]
                [ text
                    ("by " ++ card.src.user.name ++ " from " ++ card.src.user.location)
                ]
            ]
        ]


imgToCard : Image -> Card
imgToCard img =
    { src = img, selected = False }


type alias Card =
    { src : Image
    , selected : Bool
    }


unsplashSearch : String -> Cmd Msg
unsplashSearch query =
    Http.get
        { url = "/api/search?query=THE" ++ query
        , expect = Http.expectJson GotUnsplashPage unsplashPageDecoder
        }


type alias UnsplashPage =
    { current : Int
    , next : Maybe Int
    , prev : Maybe Int
    , images : List Image
    }


type alias UnsplashMeta =
    { current : Int
    , next : Maybe Int
    , prev : Maybe Int
    }


type alias Image =
    { id : String
    , desc : String
    , alt : String
    , url : String
    , likes : Int
    , user : User
    }


type alias User =
    { name : String
    , location : String
    , bio : String
    }



-- ENCODE / DECODE


unsplashPageDecoder : D.Decoder UnsplashPage
unsplashPageDecoder =
    D.map4 UnsplashPage
        (D.field "current" D.int)
        (D.field "next" (D.nullable D.int))
        (D.field "prev" (D.nullable D.int))
        (D.field "results" (D.list unsplashDecoder))


unsplashDecoder : D.Decoder Image
unsplashDecoder =
    D.map6 Image
        (D.field "id" D.string)
        (D.field "desc" D.string)
        (D.field "alt" D.string)
        (D.field "url" D.string)
        (D.field "likes" D.int)
        (D.field "user" userDecoder)


userDecoder : D.Decoder User
userDecoder =
    D.map3 User
        (D.field "name" D.string)
        (D.field "location" D.string)
        (D.field "bio" D.string)



-- TODO: model decode / encode


modelEncoder : Model -> E.Value
modelEncoder model =
    E.string "yup"


modelDecoder : D.Decoder Model
modelDecoder =
    D.succeed init
