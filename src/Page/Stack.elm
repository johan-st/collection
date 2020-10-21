module Page.Stack exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Json.Decode as D
import Json.Encode as E


type alias Model =
    String


type Msg
    = NoOp


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
    section [ class "stack" ]
        [ h2 [ class "stack__header" ] [ text "what you are looking at is.. " ]
        , ul [ class "stack__list" ]
            [ li [ class "stack__text" ] [ text "An Elm SPA" ]
            , li [ class "stack__text" ] [ text "running on node express" ]
            , li [ class "stack__text" ] [ text "containerized" ]
            , li [ class "stack__text" ] [ text "docker backed" ]
            , li [ class "stack__text" ] [ text "and running on aws" ]
            ]
        , h3 [ class "stack__header" ] [ text "also.." ]
        , ul [ class "stack__list" ]
            [ li [ class "stack__text" ] [ text "using local storage for persistence" ]
            , li [ class "stack__text" ] [ text "planing to set up serviceworker and manifest" ]
            ]
        , h3 [ class "stack__header" ] [ text "plans and ideas" ]
        , ul [ class "stack__list" ]
            [ li [ class "stack__text" ] [ text "finnish the port of my mob-timer from elm-ui to vanilla html and css" ]
            , li [ class "stack__text" ] [ text "connect to scb and practice data-vizualization" ]
            , li [ class "stack__text" ] [ text "Build a reminder app" ]
            , li [ class "stack__text" ] [ text "integrate with OAuth" ]
            , li [ class "stack__text" ] [ text "add backend storage and session handling" ]
            , li [ class "stack__text" ] [ text "a todo-app" ]
            , li [ class "stack__text" ] [ text "a chat-server w/ client" ]
            , li [ class "stack__text" ] [ text "news aggregation and statistical comparison and fact checking" ]
            , li [ class "stack__text" ] [ text "portal for (interactive?) written content w/ a twist?" ]
            ]
        ]


modelEncoder : Model -> E.Value
modelEncoder model =
    E.string "yup"


modelDecoder : D.Decoder Model
modelDecoder =
    D.succeed "stack"
