module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes exposing (title)
import Page.Page exposing (Page(..))
import Url
import Utils.Color as C



---- MODEL ----


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , page : Page
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key, url = url, page = Landing }, Cmd.none )



---- UPDATE ----


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "App Title"
    , body =
        [ Element.layout
            [ BG.color C.darkBase
            , Font.color C.accent1
            ]
          <|
            viewFrame model
        ]
    }


viewFrame : Model -> Element Msg
viewFrame model =
    column
        [ width fill
        , height fill

        -- , explain Debug.todo
        ]
        [ navBar model
        , pageViewer model.page
        ]


navBar : Model -> Element Msg
navBar model =
    el [ width fill, BG.color C.darkBase2 ] <| text "navigation goes here"


pageViewer : Page -> Element Msg
pageViewer page =
    case page of
        Landing ->
            row
                [ width fill
                , height fill
                , Border.width 2

                -- , explain Debug.todo
                ]
                [ spacer 1
                , column
                    [ width (fillPortion 4)
                    , Border.width 2
                    , Font.center

                    -- , explain Debug.todo
                    ]
                  <|
                    [ text "Landing Page" ]
                , spacer 1
                ]

        FizzBuzz ->
            Debug.todo "Implememtn FizzBuzz"

        RomanNumerals ->
            Debug.todo "Implememtn Roman Numerals"


spacer : Int -> Element Msg
spacer portion =
    el [ width (fillPortion portion), height (fillPortion portion) ] Element.none



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
