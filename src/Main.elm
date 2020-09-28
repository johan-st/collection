module Main exposing (..)

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Page.FizzBuzz as FizzBuzz exposing (Msg(..), Soda(..))
import Page.Page as Page exposing (Page(..))
import Page.PrimeFactorization as Prime
import Page.RomanNumerals as Numeral
import Page.Visuals as Visuals
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
                    Page.FizzBuzz <| FizzBuzz.init 7

                "/numerals" ->
                    Page.RomanNumerals (Numeral.Model "" [])

                "/visuals" ->
                    Page.Visuals Visuals.init

                "/primes" ->
                    Page.PrimeFactorization ""

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
    , Cmd.batch [ Task.perform AdjustTimeZone Time.here, Task.perform Tick Time.now ]
    )


initialPersistance : Persist
initialPersistance =
    { numerals = RomanNumerals (Numeral.Model "" [])
    , fizzbuzz = Page.FizzBuzz <| FizzBuzz.init 0
    , primeFactors = Page.PrimeFactorization ""
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
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | GotFizzBuzzMsg FizzBuzz.Msg
    | GotNumeralMsg Numeral.Msg
    | GotPrimeMsg Prime.Msg
    | GotVisualsMsg Visuals.Msg


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
                            Page.Visuals ""

                        "/primes" ->
                            model.persistance.primeFactors

                        _ ->
                            Page.NotFound_404
            in
            ( { model | url = url, page = page }
            , Cmd.none
            )

        GotNumeralMsg numMsg ->
            case model.page of
                Page.RomanNumerals numModel ->
                    toNumeral model (Numeral.update numMsg numModel)

                _ ->
                    ( model, Cmd.none )

        GotFizzBuzzMsg fizzBuzzMsg ->
            case model.page of
                Page.FizzBuzz fizzBuzzModel ->
                    toFizzBuzz model (FizzBuzz.update fizzBuzzMsg fizzBuzzModel)

                _ ->
                    ( model, Cmd.none )

        GotPrimeMsg primeMsg ->
            case model.page of
                Page.PrimeFactorization primeModel ->
                    toPrime model (Prime.update primeMsg primeModel)

                _ ->
                    ( model, Cmd.none )

        GotVisualsMsg visMsg ->
            case model.page of
                Page.Visuals visModel ->
                    toVisuals model (Visuals.update visMsg visModel)

                _ ->
                    ( model, Cmd.none )

        Tick time ->
            ( { model | time = time }, Cmd.none )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )


updateTime : Time.Posix -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateTime time ( model, cmd ) =
    ( { model | time = time }, cmd )


toPrime : Model -> ( Prime.Model, Cmd Prime.Msg ) -> ( Model, Cmd Msg )
toPrime model ( primeModel, cmd ) =
    ( { model | page = Page.PrimeFactorization primeModel }
    , Cmd.map GotPrimeMsg cmd
    )


toVisuals : Model -> ( Visuals.Model, Cmd Visuals.Msg ) -> ( Model, Cmd Msg )
toVisuals model ( visModel, cmd ) =
    ( { model | page = Page.Visuals visModel }
    , Cmd.map GotVisualsMsg cmd
    )


toNumeral : Model -> ( Numeral.Model, Cmd Numeral.Msg ) -> ( Model, Cmd Msg )
toNumeral model ( numModel, cmd ) =
    ( { model | page = Page.RomanNumerals numModel }
    , Cmd.map GotNumeralMsg cmd
    )


toFizzBuzz : Model -> ( FizzBuzz.Model, Cmd FizzBuzz.Msg ) -> ( Model, Cmd Msg )
toFizzBuzz model ( fizzBuzzModel, cmd ) =
    let
        newFizz =
            Page.FizzBuzz fizzBuzzModel

        persist =
            model.persistance

        newPersist =
            { persist | fizzbuzz = newFizz }
    in
    ( { model | page = newFizz, persistance = newPersist }
    , Cmd.map GotFizzBuzzMsg cmd
    )



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
        , pageViewer model
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


pageViewer : Model -> Element Msg
pageViewer model =
    let
        pageView =
            case model.page of
                Page.Landing ->
                    landing

                Page.FizzBuzz fizzBuzzModel ->
                    FizzBuzz.view fizzBuzzModel |> Element.map GotFizzBuzzMsg

                Page.RomanNumerals numeralModel ->
                    Numeral.view numeralModel |> Element.map GotNumeralMsg

                Page.Diary ->
                    Debug.todo "Implement Diary"

                Page.NotFound_404 ->
                    notFound model.url

                Page.Visuals visModel ->
                    Visuals.view visModel |> Element.map GotVisualsMsg

                Page.PrimeFactorization num ->
                    Prime.view num |> Element.map GotPrimeMsg
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


notFound : Url.Url -> Element Msg
notFound url =
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
            , text <| "Full Url" ++ Url.toString url
            ]
        ]



--   https://example.com:8042/over/there?name=ferret#nose
--   \___/   \______________/\_________/ \_________/ \__/
--     |            |            |            |        |
--   scheme     authority       path        query   fragment
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
