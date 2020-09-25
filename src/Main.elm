module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Page.FizzBuzz exposing (Soda(..), carbonate)
import Page.Page as Page exposing (Page(..))
import Page.RomanNumerals exposing (Numeral, toRoman)
import Url
import Utils.Color as C



---- MODEL ----


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , page : Page
    , persistance : Persist
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key, url = url, page = Landing, persistance = initialPersistance }, Cmd.none )


initialPersistance : Persist
initialPersistance =
    { numerals = RomanNumerals "" []
    , fizzbuzz = FizzBuzz
    }


type alias Persist =
    { numerals : Page
    , fizzbuzz : Page
    }



---- UPDATE ----


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | NumInputChanged String


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
            let
                page =
                    case url.path of
                        "/" ->
                            Page.Landing

                        "/fizzbuzz" ->
                            model.persistance.fizzbuzz

                        "/numerals" ->
                            model.persistance.numerals

                        "/visuals" ->
                            Page.Visuals

                        _ ->
                            Page.NotFound_404
            in
            ( { model | url = url, page = page }
            , Cmd.none
            )

        NumInputChanged input ->
            let
                persist =
                    { numerals = updateNumsInput model.persistance.numerals input
                    , fizzbuzz = model.persistance.fizzbuzz
                    }
            in
            ( { model | persistance = persist, page = persist.numerals }, Cmd.none )


updateNumsInput : Page -> String -> Page
updateNumsInput page input =
    case page of
        Page.RomanNumerals _ _ ->
            case String.toInt input of
                Just int ->
                    if int < 4000 && int > 0 then
                        Page.RomanNumerals input (toRoman int)

                    else
                        page

                Nothing ->
                    Page.RomanNumerals "" (toRoman 0)

        _ ->
            Debug.todo "Should never trigger. refactor away?"



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "url: " ++ model.url.path
    , body =
        [ Element.layout
            [ BG.color C.darkBase1
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
        ]
        [ navBar model
        , pageViewer model.page
        ]



-- NAVIGATION --


navBar : Model -> Element Msg
navBar model =
    row
        [ width fill
        , BG.color C.darkBase3
        , spaceEvenly
        ]
    <|
        [ spacer 1
        , link [ width fill ] { label = text "404", url = "/404" }
        , link [ width fill ] { label = text "fizzBuzz", url = "/fizzbuzz" }
        , link [ width fill ] { label = text "Roman Numerals", url = "/numerals" }
        , link [ width fill ] { label = text "Visual Experimentation", url = "/visuals" }
        , spacer 1
        ]


pageViewer : Page -> Element Msg
pageViewer page =
    let
        pageView =
            case page of
                Page.Landing ->
                    landing

                Page.FizzBuzz ->
                    fizzBuzz

                Page.RomanNumerals int nums ->
                    numerals int nums

                Page.Diary ->
                    Debug.todo "Implement Diary"

                Page.NotFound_404 ->
                    notFound

                Page.Visuals ->
                    visualsView
    in
    row
        [ width fill
        , height fill
        ]
        [ spacer 1
        , el
            [ width (fillPortion 4)
            , height fill
            , BG.color C.darkBase2
            ]
            pageView
        , spacer 1
        ]



-- PAGE-VIEW -- fizzBuzz


fizzBuzz : Element Msg
fizzBuzz =
    column [ width fill ]
        [ el [] <| text "welcome to fizzbuzz"
        , wrappedRow [ spacing 5 ] <| List.map sodaToEl <| List.map carbonate (List.range 1 102)
        ]


sodaToEl : Soda -> Element Msg
sodaToEl soda =
    case soda of
        Uncarbonated int ->
            el [ Font.color C.subtle ] <| text <| String.fromInt int

        Fizzy ->
            el [ Font.color C.accent2 ] <| text <| "Fizz"

        Buzzy ->
            el [ Font.color C.accent4 ] <| text <| "Buzz"

        FizzyBuzzy ->
            el [ Font.color C.accent3 ] <| text <| "FizzBuzz"



-- PAGE-VIEW -- landing


landing : Element Msg
landing =
    row
        [ width fill
        , height fill
        , Border.width 2
        ]
        [ column
            [ width fill
            , height fill
            , Font.center
            ]
          <|
            [ text "Landing Page" ]
        ]



-- PAGE-VIEW -- notFound


notFound : Element Msg
notFound =
    row
        [ width fill
        , height fill
        , Border.width 2
        , Border.color C.accent4
        ]
        [ column
            [ width fill
            , height fill
            , Font.center
            ]
          <|
            [ text "404 Not Found"
            , text "This is not the page you are looking for"
            ]
        ]



-- PAGE-VIEW -- numeral


numerals : String -> List Numeral -> Element Msg
numerals input nums =
    column
        [ centerX
        , centerY
        ]
        [ Input.text []
            { onChange = NumInputChanged
            , text = input
            , placeholder = Nothing
            , label =
                Input.labelAbove []
                    (text "enter a number (1-3999)")
            }
        , column [] [ el [] <| text (Page.RomanNumerals.numsToString nums) ]
        ]


spacer : Int -> Element Msg
spacer portion =
    el [ width (fillPortion portion), height (fillPortion portion) ] Element.none



-- Page-View -- visuals


visualsView : Element Msg
visualsView =
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
