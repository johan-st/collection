port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (search)
import Json.Decode as D
import Json.Encode as E exposing (encode)
import Page.FizzBuzz as FizzBuzz exposing (Msg(..), Soda(..))
import Page.Page as Page exposing (Page(..), fizzbuzzDecoder, numeralsDecoder, pageEncoder, primeDecoder)
import Page.PrimeFactorization as Prime
import Page.RomanNumerals as Numeral exposing (Numeral(..))
import Page.Search as Search
import Page.Timer as Timer
import Page.Visuals as Visuals
import Task
import Time
import Url
import Url.Builder
import Utils.Color as C



---- MODEL ----


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , page : Page
    , persistance : Persist20200929
    , time : Time.Posix
    , zone : Time.Zone
    }


init : String -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        persist =
            initialPersistance flags

        page =
            case url.path of
                "/" ->
                    Page.Landing

                "/fizzbuzz" ->
                    persist.fizzbuzz

                "/numerals" ->
                    persist.numerals

                "/visuals" ->
                    Page.Visuals Visuals.init

                "/primes" ->
                    persist.primeFactors

                "/search" ->
                    Page.Search <| Search.init url

                _ ->
                    Page.NotFound_404
    in
    ( { key = key
      , url = url
      , page = page
      , persistance = initialPersistance flags
      , time = Time.millisToPosix 0
      , zone = Time.utc
      }
    , Cmd.batch [ Task.perform AdjustTimeZone Time.here, Task.perform Tick Time.now, log ("flags on init:\n" ++ flags) ]
    )


initialPersistance : String -> Persist20200929
initialPersistance flags =
    case D.decodeString persistDecoder flags of
        Result.Err _ ->
            { numerals = RomanNumerals (Numeral.Model "" [])
            , fizzbuzz = Page.FizzBuzz <| FizzBuzz.init 7
            , primeFactors = Page.PrimeFactorization ""
            }

        Ok data ->
            data


type alias Persist20200929 =
    { fizzbuzz : Page
    , numerals : Page
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
    | GotSearchMsg Search.Msg
    | GotTimerMsg Timer.Msg
    | UpdateLocalStorage


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
                            Page.Visuals ""

                        "/primes" ->
                            model.persistance.primeFactors

                        "/search" ->
                            Page.Search (Search.init model.url)

                        "/timer" ->
                            Page.Timer Timer.init

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

        GotSearchMsg searchMsg ->
            case model.page of
                Page.Search searchModel ->
                    case searchMsg of
                        Search.SearchClicked ->
                            let
                                newUrl =
                                    Url.Builder.relative []
                                        [ Url.Builder.string "q" (Search.toString searchModel.search) ]
                            in
                            toSearch model
                                (Nav.pushUrl model.key newUrl)
                                (Search.update searchMsg searchModel)

                        Search.InputChanged _ ->
                            toSearch model
                                Cmd.none
                                (Search.update searchMsg searchModel)

                _ ->
                    ( model, Cmd.none )

        GotTimerMsg timerMsg ->
            case model.page of
                Page.Timer timerModel ->
                    toTimer model (Timer.update timerMsg timerModel)

                _ ->
                    ( model, Cmd.none )

        Tick time ->
            ( { model | time = time }, Cmd.none )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , log "AdjustTimeZone"
            )

        UpdateLocalStorage ->
            ( model, setPersist (E.encode 1 (persistEncoder model.persistance)) )


updateTime : Time.Posix -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateTime time ( model, cmd ) =
    ( { model | time = time }, cmd )


toPrime : Model -> ( Prime.Model, Cmd Prime.Msg ) -> ( Model, Cmd Msg )
toPrime model ( primeModel, cmd ) =
    let
        newPrime =
            Page.PrimeFactorization primeModel

        persist =
            model.persistance

        newPersist =
            { persist | primeFactors = newPrime }
    in
    ( { model | page = newPrime, persistance = newPersist }
    , Cmd.map GotPrimeMsg cmd
    )


toVisuals : Model -> ( Visuals.Model, Cmd Visuals.Msg ) -> ( Model, Cmd Msg )
toVisuals model ( visModel, cmd ) =
    ( { model | page = Page.Visuals visModel }
    , Cmd.map GotVisualsMsg cmd
    )


toNumeral : Model -> ( Numeral.Model, Cmd Numeral.Msg ) -> ( Model, Cmd Msg )
toNumeral model ( numModel, cmd ) =
    let
        newNumeral =
            Page.RomanNumerals numModel

        persist =
            model.persistance

        newPersist =
            { persist | numerals = newNumeral }
    in
    ( { model | page = newNumeral, persistance = newPersist }
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


toSearch : Model -> Cmd Msg -> ( Search.Model, Cmd Search.Msg ) -> ( Model, Cmd Msg )
toSearch model mainCmd ( searchModel, cmd ) =
    ( { model | page = Page.Search searchModel }
    , Cmd.batch [ Cmd.map GotSearchMsg cmd, mainCmd ]
    )


toTimer : Model -> ( Timer.Model, Cmd Timer.Msg ) -> ( Model, Cmd Msg )
toTimer model ( searchModel, cmd ) =
    ( { model | page = Page.Timer searchModel }
    , Cmd.map GotTimerMsg cmd
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
        , paddingXY 20 10
        ]
    <|
        [ link [ width fill ] { label = text "fizzBuzz", url = "/fizzbuzz" }
        , link [ width fill ] { label = text "Roman Numerals", url = "/numerals" }
        , link [ width fill ] { label = text "Prime Factorization", url = "/primes" }
        , link [ width fill ] { label = text "Visuals", url = "/visuals" }
        , link [ width fill ] { label = text "Timer", url = "/timer" }
        , link [ width fill ] { label = text "Search", url = "/search" }
        , Input.button [ Font.underline, Font.color C.accent2 ]
            { onPress = Just UpdateLocalStorage
            , label = text "save state"
            }
        , spacer 1
        , el [ Font.color C.accent3 ] <| text (toTime model.zone model.time)
        ]



-- VIEWS --


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
                    landing

                Page.NotFound_404 ->
                    notFound model.url

                Page.Visuals visModel ->
                    Visuals.view visModel |> Element.map GotVisualsMsg

                Page.PrimeFactorization num ->
                    Prime.view num |> Element.map GotPrimeMsg

                Page.Search pageModel ->
                    Search.view pageModel |> Element.map GotSearchMsg

                Page.Timer timerModel ->
                    Timer.view timerModel |> Element.map GotTimerMsg
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
--   https://example.com:8042/over/there?name=ferret#nose
--   \___/   \______________/\_________/ \_________/ \__/
--     |            |            |            |        |
--   scheme     authority       path        query   fragment


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


main : Program String Model Msg
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
    Time.every 1000 Tick



-- PORTS


port log : String -> Cmd msg


port setPersist : String -> Cmd msg



-- ENCODE / DECODE --


persistEncoder : Persist20200929 -> E.Value
persistEncoder p =
    E.object
        [ ( "storeVersion", E.string "v20200929" )
        , ( "fizzbuzz", pageEncoder p.fizzbuzz )
        , ( "numerals", pageEncoder p.numerals )
        , ( "prime", pageEncoder p.primeFactors )
        ]


persistDecoder : D.Decoder Persist20200929
persistDecoder =
    D.field "storeVersion" D.string
        |> D.andThen persistDecoderHelper


persistDecoderHelper : String -> D.Decoder Persist20200929
persistDecoderHelper versionString =
    case versionString of
        "v20200929" ->
            persist20200929Decoder

        _ ->
            D.fail "unhandled version"


persist20200929Decoder : D.Decoder Persist20200929
persist20200929Decoder =
    D.map3 Persist20200929
        (D.field "fizzbuzz" Page.fizzbuzzDecoder)
        (D.field "numerals" Page.numeralsDecoder)
        (D.field "prime" Page.primeDecoder)
