module Page.Login exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (alt, class, classList, placeholder, src, type_)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D
import Json.Encode as E


type alias Model =
    { userInput : String
    , pwInput : String
    }


type Msg
    = UserInputChanged String
    | PwInputChanged String
    | LoginClicked


init : Model
init =
    { userInput = ""
    , pwInput = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserInputChanged str ->
            ( { model | userInput = str }, Cmd.none )

        PwInputChanged str ->
            ( { model | pwInput = str }, Cmd.none )

        LoginClicked ->
            Debug.todo "login action"


view : Model -> Html Msg
view model =
    section []
        [ p [] [ text model.userInput ]
        , p [] [ text model.pwInput ]
        ]
