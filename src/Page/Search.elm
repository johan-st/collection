module Page.Search exposing (..)

import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (search)
import Html exposing (input)
import Url exposing (Url)
import Utils.Color as C



-- SEARCH


type Search
    = Empty
    | Uncommited String
    | Pending
    | Resolved


toString : Search -> String
toString search =
    case search of
        Uncommited string ->
            string

        _ ->
            ""



-----------------------


type alias Model =
    { url : Url.Url
    , search : Search
    }


type Msg
    = InputChanged String
    | SearchClicked


init : Url.Url -> Model
init url =
    { url = url
    , search = Empty
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged input ->
            ( { model | search = Uncommited input }, Cmd.none )

        SearchClicked ->
            ( { model | search = Empty }, Cmd.none )


view : Model -> Element Msg
view model =
    column [ width fill, padding 30, spacing 20 ]
        [ el [ Font.color C.accent2, centerX, Font.size 60 ] <| text "ThingsFinder"
        , el [ Font.color C.accent4, alignRight ] <| text "not yet implemented!"
        , row [ width fill, spacing 30 ]
            [ Input.search []
                { onChange = InputChanged
                , text = toString model.search
                , placeholder = Just (Input.placeholder [] <| text "This search box is not working")
                , label = Input.labelHidden "search"
                }
            , Input.button
                [ padding 15
                , Border.innerGlow C.accent3 3
                ]
                { onPress = Just SearchClicked
                , label = text "TF me!"
                }
            ]
        , queryResult model
        ]


queryResult : Model -> Element Msg
queryResult model =
    case model.search of
        Empty ->
            text <| "Please enter a search term."

        Uncommited string ->
            text <| "press \"TF me\" to search for " ++ string ++ "."

        Pending ->
            text <| "please hold...."

        Resolved ->
            text <| "here you will have results "


queryDisplay : Url.Url -> Element Msg
queryDisplay url =
    let
        query =
            case url.query of
                Nothing ->
                    ""

                Just qString ->
                    qString
    in
    el [] <| column [ Font.size 60 ] [ text query ]



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
