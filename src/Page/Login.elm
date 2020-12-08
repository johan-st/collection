module Page.Login exposing (..)

import Bootstrap.Form.Select exposing (onChange)
import Html exposing (..)
import Html.Attributes as Attr exposing (class, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Json.Encode as E
import JsonWebToken as JWT exposing (hmacSha256)
import Page.Gallery exposing (User)
import Session exposing (Session(..))


type alias Model =
    { userInput : String
    , pwInput : String
    , notification : String
    , jwtSecret : String
    }


type Msg
    = UserInputChanged String
    | PwInputChanged String
    | GotLoginResult (Result Http.Error LoginRes)
    | LoginClicked


init : Model
init =
    { userInput = ""
    , pwInput = ""
    , notification = ""
    , jwtSecret = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserInputChanged str ->
            ( { model | userInput = str }, Cmd.none )

        PwInputChanged str ->
            ( { model | pwInput = str }, Cmd.none )

        LoginClicked ->
            ( model
            , Http.get
                { url = "/jwt/get"
                , expect = Http.expectJson GotLoginResult loginResultDecoder
                }
            )

        GotLoginResult res ->
            case res of
                Ok { token, secret } ->
                    ( { model | notification = token, jwtSecret = secret }, Cmd.none )

                Err err ->
                    Debug.todo <| Debug.toString err


view : Model -> Html Msg
view model =
    section [ class "login__form" ]
        [ input [ type_ "text", class "login__input", value model.userInput, onInput UserInputChanged ] []
        , input [ type_ "text", class "login__input", value model.pwInput, onInput PwInputChanged ] []
        , button [ type_ "submit", class "login__button", onClick LoginClicked ] [ text "login" ]
        , p [ class "login__notification" ] [ text model.notification ]
        ]


type alias LoginRes =
    { token : String, secret : String }


loginResultDecoder : D.Decoder LoginRes
loginResultDecoder =
    -- Really stupid implementation
    D.map2 LoginRes
        (D.field "token" D.string)
        (D.field "secret" D.string)


type alias Token =
    { iat : Int
    , user : String
    , permissions : List String
    }


test =
    JWT.encode hmacSha256 Json.Encode.string "secret" "some payload"
        |> JWT.decode Json.Decode.string "secret"


tokenDecoder : D.Decoder Token
tokenDecoder =
    D.map3 Token
        (D.field "iat" D.int)
        (D.field "user" D.string)
        (D.field "permissions" (D.list D.string))
