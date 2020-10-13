port module Main exposing (..)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Col
import Bootstrap.ListGroup as Listgroup
import Bootstrap.Modal as Modal
import Bootstrap.Navbar as Navbar
import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Html exposing (Html, div, h1, h2, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E exposing (encode)
import Page.FizzBuzz as FizzBuzz exposing (Msg(..), Soda(..))
import Page.Page as Page exposing (Page(..))
import Page.PrimeFactorization as Prime
import Page.RomanNumerals as Numeral exposing (Numeral(..))
import Page.Search as Search
import Page.Timer as Timer
import Page.Visuals as Visuals
import Task
import Time
import Url exposing (Url)
import Url.Builder
import Url.Parser as UrlParser exposing ((</>), Parser, s, top)
import Utils.Color as C
import Utils.Utils exposing (timeString)


type alias Flags =
    String



-- TODO: Build progressbar for salt course, week, day


type alias Model =
    { navKey : Navigation.Key
    , page : Page
    , navState : Navbar.State
    , modalVisibility : Modal.Visibility
    , url : Url.Url
    , persistance : PersistV2
    , time : Time.Posix
    , zone : Time.Zone
    }


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        persist =
            initialPersistance flags

        page =
            case url.path of
                "/" ->
                    Page.Home

                "/fizzbuzz" ->
                    Page.FizzBuzz persist.fizzbuzz

                "/numerals" ->
                    Page.RomanNumerals persist.numerals

                "/visuals" ->
                    Page.Visuals Visuals.init

                "/primes" ->
                    Page.PrimeFactorization persist.primeFactors

                "/search" ->
                    Page.Search <| Search.init url

                "/timer" ->
                    Page.Timer persist.timer

                _ ->
                    Page.NotFound_404

        ( navState, navCmd ) =
            Navbar.initialState NavMsg

        ( model, urlCmd ) =
            urlUpdate url
                { navKey = key
                , page = Home
                , navState = navState
                , modalVisibility = Modal.hidden
                , url = url
                , persistance = initialPersistance flags
                , time = Time.millisToPosix 0
                , zone = Time.utc
                }
    in
    ( model
    , Cmd.batch
        [ urlCmd
        , navCmd
        , Task.perform AdjustTimeZone Time.here
        , Task.perform Tick Time.now
        , log ("flags on init:\n" ++ flags)
        ]
    )


initialPersistance : String -> PersistV2
initialPersistance flags =
    case D.decodeString persistDecoder flags of
        Result.Err _ ->
            PersistV2
                FizzBuzz.init
                Numeral.init
                Prime.init
                Timer.init

        Ok data ->
            data


type Msg
    = ClickedLink UrlRequest
    | UrlChange Url
    | NavMsg Navbar.State
    | CloseModal
    | ShowModal
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
        ClickedLink req ->
            case req of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.navKey <| Url.toString url )

                Browser.External href ->
                    ( model, Navigation.load href )

        UrlChange url ->
            urlUpdate url model

        NavMsg state ->
            ( { model | navState = state }
            , Cmd.none
            )

        CloseModal ->
            ( { model | modalVisibility = Modal.hidden }
            , Cmd.none
            )

        ShowModal ->
            ( { model | modalVisibility = Modal.shown }
            , Cmd.none
            )

        Tick time ->
            ( { model | time = time }, setPersist (E.encode 1 (persistEncoder model.persistance)) )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }, Cmd.none )

        _ ->
            Debug.todo "update"


urlUpdate : Url -> Model -> ( Model, Cmd Msg )
urlUpdate url model =
    let
        page =
            case url.path of
                "/" ->
                    Page.Home

                "/katas" ->
                    Page.Katas

                "/katas/fizzbuzz" ->
                    Page.fromModel <| Page.FB model.persistance.fizzbuzz

                "/katas/numerals" ->
                    Page.fromModel <| Page.N model.persistance.numerals

                "/visuals" ->
                    Page.fromModel <| Page.V ""

                "/katas/primes" ->
                    Page.fromModel <| Page.P model.persistance.primeFactors

                "/search" ->
                    Page.Search (Search.init model.url)

                "/timer" ->
                    Page.fromModel <| Page.T model.persistance.timer

                _ ->
                    Page.NotFound_404
    in
    ( { model | url = url, page = page }
    , Cmd.none
    )


view : Model -> Browser.Document Msg
view model =
    { title = "Elm Bootstrap"
    , body =
        [ div []
            [ menu model
            , mainContent model
            , modal model
            ]
        ]
    }


menu : Model -> Html Msg
menu model =
    Navbar.config NavMsg
        |> Navbar.withAnimation
        |> Navbar.container
        |> Navbar.brand [ href "/" ] [ text "</salty noodles>" ]
        |> Navbar.items
            [ Navbar.dropdown
                { id = "mydropdown"
                , toggle = Navbar.dropdownToggle [] [ text "Katas" ]
                , items =
                    [ Navbar.dropdownItem
                        [ href "/katas" ]
                        [ text "overview" ]
                    , Navbar.dropdownDivider
                    , Navbar.dropdownItem
                        [ href "/katas/fizzbuzz" ]
                        [ text "fizzbuzz" ]
                    , Navbar.dropdownItem
                        [ href "/katas/roman_numerals" ]
                        [ text "roman Numerals" ]
                    , Navbar.dropdownItem
                        [ href "/katas/prime_factorization" ]
                        [ text "prime factorization" ]
                    ]
                }
            , Navbar.itemLink [ href "/404" ] [ text "Get Lost" ]
            ]
        |> Navbar.customItems
            [ Navbar.textItem []
                [ text (timeString model.zone model.time) ]
            ]
        |> Navbar.view model.navState


mainContent : Model -> Html Msg
mainContent model =
    -- let
    --     persist =
    --         model.persistance
    -- in
    Grid.container [] <|
        case model.page of
            Home ->
                pageHome model

            Katas ->
                pageKatasOverview model

            FizzBuzz _ ->
                pageGettingStarted model

            NotFound_404 ->
                pageNotFound_404

            _ ->
                Debug.todo "mainContent"


pageKatasOverview : Model -> List (Html Msg)
pageKatasOverview model =
    [ h1 [] [ text "Katas" ]
    , Grid.row []
        [ Grid.col []
            [ Card.config [ Card.outlinePrimary ]
                |> Card.headerH4 [] [ text "Fizzbuzz" ]
                |> Card.block []
                    [ Block.text [] [ text "Fizz buzz is a group word game for children to teach them about division. Count incrementally, replacing any number divisible by three with the word \" fizz \", and any number divisible by five with the word \" buzz \". " ]
                    , Block.custom <|
                        Button.linkButton
                            [ Button.primary, Button.attrs [ href "/katas/fizzbuzz" ] ]
                            [ text "fizz me!" ]
                    , Block.custom <|
                        Button.linkButton
                            [ Button.secondary, Button.attrs [ href "https://en.wikipedia.org/wiki/Fizz_buzz" ] ]
                            [ text "wikipedia" ]
                    ]
                |> Card.view
            ]
        , Grid.col []
            [ Card.config [ Card.outlineDanger ]
                |> Card.headerH4 [] [ text "Modules" ]
                |> Card.block []
                    [ Block.text [] [ text "Check out the modules overview" ]
                    , Block.custom <|
                        Button.linkButton
                            [ Button.primary, Button.attrs [ href "#modules" ] ]
                            [ text "Module" ]
                    ]
                |> Card.view
            ]
        ]
    ]


pageHome : Model -> List (Html Msg)
pageHome model =
    [ h1 [] [ text "Home" ]
    , Grid.row []
        [ Grid.col []
            [ Card.config [ Card.outlinePrimary ]
                |> Card.headerH4 [] [ text "Getting started" ]
                |> Card.block []
                    [ Block.text [] [ text "Getting started is real easy. Just click the start button." ]
                    , Block.custom <|
                        Button.linkButton
                            [ Button.primary, Button.attrs [ href "#getting-started" ] ]
                            [ text "Start" ]
                    ]
                |> Card.view
            ]
        , Grid.col []
            [ Card.config [ Card.outlineDanger ]
                |> Card.headerH4 [] [ text "Modules" ]
                |> Card.block []
                    [ Block.text [] [ text "Check out the modules overview" ]
                    , Block.custom <|
                        Button.linkButton
                            [ Button.primary, Button.attrs [ href "#modules" ] ]
                            [ text "Module" ]
                    ]
                |> Card.view
            ]
        ]
    ]


pageGettingStarted : Model -> List (Html Msg)
pageGettingStarted model =
    [ h2 [] [ text "Getting started" ]
    , Button.button
        [ Button.success
        , Button.large
        , Button.block
        , Button.attrs [ onClick ShowModal ]
        ]
        [ text "Click me" ]
    ]


pageModules : Model -> List (Html Msg)
pageModules model =
    [ h1 [] [ text "Modules" ]
    , Listgroup.ul
        [ Listgroup.li [] [ text "Alert" ]
        , Listgroup.li [] [ text "Badge" ]
        , Listgroup.li [] [ text "Card" ]
        ]
    ]


pageNotFound_404 : List (Html Msg)
pageNotFound_404 =
    [ div []
        [ Alert.simplePrimary []
            [ h1 [] [ text "Not found" ]
            , text "Sorry couldn't find that page"
            ]
        ]
    ]


modal : Model -> Html Msg
modal model =
    Modal.config CloseModal
        |> Modal.small
        |> Modal.h4 [] [ text "Getting started ?" ]
        |> Modal.body []
            [ Grid.containerFluid []
                [ Grid.row []
                    [ Grid.col
                        [ Col.xs6 ]
                        [ text "Col 1" ]
                    , Grid.col
                        [ Col.xs6 ]
                        [ text "Col 2" ]
                    ]
                ]
            ]
        |> Modal.view model.modalVisibility



-- ENCODE / DECODE --


persistDecoderHelper : String -> D.Decoder PersistV2
persistDecoderHelper versionString =
    case versionString of
        "v2" ->
            D.map4 PersistV2
                (D.field "fizzbuzz" FizzBuzz.modelDecoder)
                (D.field "numerals" Numeral.modelDecoder)
                (D.field "prime" Prime.modelDecoder)
                (D.field "timer" Timer.modelDecoder)

        _ ->
            D.fail "unhandled storage version"


persistDecoder : D.Decoder PersistV2
persistDecoder =
    D.field "storeVersion" D.string
        |> D.andThen persistDecoderHelper


type alias PersistV2 =
    { fizzbuzz : FizzBuzz.Model
    , numerals : Numeral.Model
    , primeFactors : Prime.Model
    , timer : Timer.Model
    }


persistEncoder : PersistV2 -> E.Value
persistEncoder p =
    E.object
        [ ( "storeVersion", E.string "v2" )
        , ( "fizzbuzz", FizzBuzz.modelEncoder p.fizzbuzz )
        , ( "numerals", Numeral.modelEncoder p.numerals )
        , ( "prime", Prime.modelEncoder p.primeFactors )
        , ( "timer", Timer.modelEncoder p.timer )
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Page.Timer timerModel ->
            Sub.batch [ Time.every 1000 Tick, Sub.map GotTimerMsg (Timer.subscriptions timerModel) ]

        _ ->
            Sub.batch [ Time.every 1000 Tick, Navbar.subscriptions model.navState NavMsg ]



-- PORTS


port log : String -> Cmd msg


port setPersist : String -> Cmd msg



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChange
        }
