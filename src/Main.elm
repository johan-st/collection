module Main exposing (..)

import Arithmetic exposing (primeFactors)
import Browser
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Page.FizzBuzz exposing (Soda(..), carbonate)
import Page.Page as Page exposing (Page(..))
import Page.RomanNumerals exposing (Numeral, toRoman)
import Task
import Time
import Url
import Utils.Color as C



---- MODEL ----


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , page : Page
    , persistance : Persist
    , time : Time.Posix
    , zone : Time.Zone
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        page =
            case url.path of
                "/" ->
                    Page.Landing

                "/fizzbuzz" ->
                    Page.FizzBuzz 15

                "/numerals" ->
                    Page.RomanNumerals "" []

                "/visuals" ->
                    Page.Visuals

                "/primes" ->
                    Page.PrimeFact ""

                _ ->
                    Page.NotFound_404
    in
    ( { key = key
      , url = url
      , page = page
      , persistance = initialPersistance
      , time = Time.millisToPosix 0
      , zone = Time.utc
      }
    , Task.perform AdjustTimeZone Time.here
    )


initialPersistance : Persist
initialPersistance =
    { numerals = RomanNumerals "" []
    , fizzbuzz = FizzBuzz 15
    , primeFactors = Page.PrimeFact ""
    }


type alias Persist =
    { numerals : Page
    , fizzbuzz : Page
    , primeFactors : Page
    }



---- UPDATE ----


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | NumeralInputChanged String
    | FizzBuzzSliderMoved Float
    | PrimeFactChanged String
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        debug =
            Debug.log "update" <| Debug.toString msg
    in
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

                        "/primes" ->
                            model.persistance.primeFactors

                        _ ->
                            Page.NotFound_404
            in
            ( { model | url = url, page = page }
            , Cmd.none
            )

        NumeralInputChanged input ->
            let
                persist =
                    { numerals = updateNumsInput model.persistance.numerals input
                    , fizzbuzz = model.persistance.fizzbuzz
                    , primeFactors = model.persistance.primeFactors
                    }
            in
            ( { model | persistance = persist, page = persist.numerals }, Cmd.none )

        FizzBuzzSliderMoved newPos ->
            let
                persist =
                    { numerals = model.persistance.numerals
                    , fizzbuzz = Page.FizzBuzz newPos
                    , primeFactors = model.persistance.primeFactors
                    }
            in
            ( { model | persistance = persist, page = persist.fizzbuzz }, Cmd.none )

        PrimeFactChanged input ->
            let
                persist =
                    { numerals = model.persistance.numerals
                    , fizzbuzz = model.persistance.fizzbuzz
                    , primeFactors = Page.PrimeFact input
                    }
            in
            ( { model | persistance = persist, page = persist.primeFactors }, Cmd.none )

        Tick time ->
            ( { model | time = time }, Cmd.none )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )


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
                    Page.RomanNumerals "" []

        _ ->
            Debug.todo "Should never trigger. Refactor away?"



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
        , padding 5
        ]
    <|
        [ spacer 1
        , link [ width fill ] { label = text "fizzBuzz", url = "/fizzbuzz" }
        , link [ width fill ] { label = text "Roman Numerals", url = "/numerals" }
        , link [ width fill ] { label = text "Prime Factorization", url = "/primes" }
        , link [ width fill ] { label = text "Visual Experimentation", url = "/visuals" }
        , spacer 1
        , el [ Font.color C.accent3 ] <| text (toTime model.zone model.time)
        ]


pageViewer : Page -> Element Msg
pageViewer page =
    let
        pageView =
            case page of
                Page.Landing ->
                    landing

                Page.FizzBuzz fizzility ->
                    fizzBuzz fizzility

                Page.RomanNumerals int nums ->
                    numerals int nums

                Page.Diary ->
                    Debug.todo "Implement Diary"

                Page.NotFound_404 ->
                    notFound

                Page.Visuals ->
                    visualsView

                Page.PrimeFact num ->
                    primeFactorsView num
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



-- PAGE-VIEW -- primeFactors


primeFactorsView : String -> Element Msg
primeFactorsView num =
    column
        [ centerX
        , centerY
        ]
        [ Input.text
            [ width (px 300)
            , centerX
            ]
            { onChange = PrimeFactChanged
            , text = num
            , placeholder = Nothing
            , label =
                Input.labelAbove []
                    (text "enter a number to factorize")
            }
        , column
            [ centerX
            , padding 30
            ]
            [ wrappedRow
                [ spacing 5 ]
              <|
                List.map
                    (\int ->
                        el [ Font.color C.subtle ] <|
                            text <|
                                String.fromInt int
                    )
                    (primeFactors (Maybe.withDefault 0 (String.toInt num)))
            ]
        ]



-- PAGE-VIEW -- fizzBuzz


fizzBuzz : Float -> Element Msg
fizzBuzz fizzity =
    column [ width fill ]
        [ el
            [ centerX
            , padding 30
            , Font.size 60
            , Font.color C.accent3
            ]
          <|
            text "welcome to fizzbuzz"
        , Input.slider
            [ BG.color C.darkBase3
            , width fill
            , paddingXY 100 0
            , Border.width 1
            , Border.rounded 50
            ]
            { onChange = FizzBuzzSliderMoved
            , label = Input.labelLeft [ paddingXY 5 15 ] <| text "How fizzy you ask?"
            , min = 0
            , max = 500
            , value = fizzity
            , thumb = Input.defaultThumb
            , step = Just 1
            }
        , wrappedRow [ spacing 5 ] <| List.map sodaToEl <| List.map carbonate (List.range 1 (ceiling fizzity))
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
        [ Input.text
            [ width (px 300)
            , centerX
            ]
            { onChange = NumeralInputChanged
            , text = input
            , placeholder = Nothing
            , label =
                Input.labelAbove []
                    (text "enter a number (1-3999)")
            }
        , column
            [ centerX
            , padding 30
            ]
            [ el
                [ Font.size 60
                , Font.family [ Font.serif ]
                , Font.color C.accent4
                ]
              <|
                text (Page.RomanNumerals.numsToString nums)
            ]
        ]



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



-- PAGE-VIEW -- extras


spacer : Int -> Element Msg
spacer portion =
    el [ width (fillPortion portion), height (fillPortion portion) ] Element.none


toTime : Time.Zone -> Time.Posix -> String
toTime zone posix =
    let
        hourInt =
            Time.toHour zone posix

        hour =
            if hourInt < 10 then
                "0" ++ String.fromInt hourInt

            else
                String.fromInt hourInt

        minInt =
            Time.toMinute zone posix

        min =
            if minInt < 10 then
                "0" ++ String.fromInt minInt

            else
                String.fromInt minInt

        secInt =
            Time.toSecond zone posix

        sec =
            if secInt < 10 then
                "0" ++ String.fromInt secInt

            else
                String.fromInt secInt
    in
    hour ++ ":" ++ min ++ ":" ++ sec



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
subscriptions _ =
    Time.every 1000 Tick
