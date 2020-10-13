port module Main exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, type_)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E exposing (encode)
import Page.FizzBuzz as FizzBuzz exposing (Msg(..), Soda(..))
import Page.Page as Page exposing (Page(..))
import Page.PrimeFactorization as Prime
import Page.RomanNumerals as Numeral exposing (Numeral(..))
import Page.Search as Search
import Page.Timer as Timer exposing (..)
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

        ( model, urlCmd ) =
            urlUpdate url
                { navKey = key
                , page = Home
                , url = url
                , persistance = initialPersistance flags
                , time = Time.millisToPosix 0
                , zone = Time.utc
                }
    in
    ( model
    , Cmd.batch
        [ urlCmd
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

                "/katas/primes" ->
                    Page.fromModel <| Page.P model.persistance.primeFactors

                "/visuals" ->
                    Page.fromModel <| Page.V ""

                "/resources" ->
                    Page.Resources

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
    { title = "Elm w/ pure-css"
    , body =
        [ div []
            [ menu model
            , mainContent model
            ]
        ]
    }


menu : Model -> Html Msg
menu model =
    div [ class "pure-menu pure-menu-horizontal" ]
        [ a [ href "/", class "pure-menu-heading pure-menu-link" ] [ text "</salty noodles>" ]
        , ul [ class "pure-menu-list" ]
            [ li [ class "pure-menu-item" ] [ a [ href "/katas", class "pure-menu-link" ] [ text "katas" ] ]
            , li [ class "pure-menu-item" ] [ a [ href "/timer", class "pure-menu-link" ] [ text "timer" ] ]
            , li [ class "pure-menu-item" ] [ a [ href "/resources", class "pure-menu-link" ] [ text "links" ] ]
            , li [ class "pure-menu-item" ] [ a [ href "/404", class "pure-menu-link" ] [ text "lost" ] ]
            ]
        , div [ class "pure-menu-item" ] [ text (timeString model.zone model.time) ]
        ]


mainContent : Model -> Html Msg
mainContent model =
    -- let
    --     persist =
    --         model.persistance
    -- in
    case model.page of
        Home ->
            pageHome model

        Katas ->
            pageKatasOverview model

        Resources ->
            pageResources model

        _ ->
            pageNotFound_404


pageResources : Model -> Html Msg
pageResources model =
    article [ class "pure-g" ]
        [ section [ class "pure-menu custom-restricted-width pure-u-1 pure-u-sm-1-3" ]
            [ span [ class "pure-menu-heading" ] [ text "elm" ]
            , ul [ class "pure-menu-list" ]
                [ li [ class "pure-menu-item" ] [ a [ class "pure-menu-link", href "https://package.elm-lang.org/" ] [ text "packages" ] ]
                , li [ class "pure-menu-item" ] [ a [ class "pure-menu-link", href "https://mbylstra.github.io/html-to-elm/" ] [ text "HTML to Elm" ] ]
                , li [ class "pure-menu-item" ] [ a [ class "pure-menu-link", href "https://guide.elm-lang.org/" ] [ text "official guide" ] ]
                ]
            ]
        , section [ class "pure-menu custom-restricted-width  pure-u-1 pure-u-sm-1-3" ]
            [ span [ class "pure-menu-heading" ] [ text "javascript" ]
            , ul [ class "pure-menu-list" ]
                [ li [ class "pure-menu-item" ] [ a [ class "pure-menu-link", href "https://package.elm-lang.org/" ] [ text "something" ] ]
                , li [ class "pure-menu-item" ] [ a [ class "pure-menu-link", href "https://mbylstra.github.io/html-to-elm/" ] [ text "some other thing" ] ]
                , li [ class "pure-menu-item" ] [ a [ class "pure-menu-link", href "https://guide.elm-lang.org/" ] [ text "bla bla" ] ]
                ]
            ]
        ]


pageKatasOverview : Model -> Html Msg
pageKatasOverview model =
    article [ class "pure-g" ]
        [ h1 [ class "pure-u-1" ] [ text "Katas" ]
        , section [ class "pure-u-sm-1-3 pure-u-1" ] [ h4 [] [ a [ href "/katas/fizzbuzz" ] [ text "Fizz Buzz" ] ], text "..is a group word game for children to teach them about division.[1] Players take turns to count incrementally, replacing any number divisible by three with the word \"fizz\", and any number divisible by five with the word \"buzz\". " ]
        , section [ class "pure-u-sm-1-3 pure-u-1" ] [ h4 [] [ a [ href "/katas/numerals" ] [ text "Roman Numerals" ] ], text "Roman numerals are a numeral system that originated in ancient Rome and remained the usual way of writing numbers throughout Europe well into the Late Middle Ages. " ]
        , section [ class "pure-u-sm-1-3 pure-u-1" ] [ h4 [] [ a [ href "/katas/primes" ] [ text "Primes" ] ], text "In number theory, integer factorization is the decomposition of a composite number into a product of smaller integers. If these factors are further restricted to prime numbers, the process is called prime factorization. " ]
        ]



-- , Grid.row []
--     [ Grid.col []
--         [ Card.config [ Card.outlinePrimary ]
--             |> Card.headerH4 [] [ text "Fizzbuzz" ]
--             |> Card.block []
--                 [ Block.text [] [ text "Fizz buzz is a group word game for children to teach them about division. Count incrementally, replacing any number divisible by three with the word \" fizz \", and any number divisible by five with the word \" buzz \". " ]
--                 , Block.custom <|
--                     Button.linkButton
--                         [ Button.primary, Button.attrs [ href "/katas/fizzbuzz" ] ]
--                         [ text "fizz me!" ]
--                 , Block.custom <|
--                     Button.linkButton
--                         [ Button.secondary, Button.attrs [ href "https://en.wikipedia.org/wiki/Fizz_buzz" ] ]
--                         [ text "wikipedia" ]
--                 ]
--             |> Card.view
--         ]
--     , Grid.col []
--         [ Card.config [ Card.outlineDanger ]
--             |> Card.headerH4 [] [ text "Modules" ]
--             |> Card.block []
--                 [ Block.text [] [ text "Check out the modules overview" ]
--                 , Block.custom <|
--                     Button.linkButton
--                         [ Button.primary, Button.attrs [ href "#modules" ] ]
--                         [ text "Module" ]
--                 ]
--             |> Card.view
--         ]
--     ]


pageHome : Model -> Html Msg
pageHome model =
    h1 [] [ text "Home" ]



-- , Grid.row []
--     [ Grid.col []
--         [ Card.config [ Card.outlinePrimary ]
--             |> Card.headerH4 [] [ text "Getting started" ]
--             |> Card.block []
--                 [ Block.text [] [ text "Getting started is real easy. Just click the start button." ]
--                 , Block.custom <|
--                     Button.linkButton
--                         [ Button.primary, Button.attrs [ href "#getting-started" ] ]
--                         [ text "Start" ]
--                 ]
--             |> Card.view
--         ]
--     , Grid.col []
--         [ Card.config [ Card.outlineDanger ]
--             |> Card.headerH4 [] [ text "Modules" ]
--             |> Card.block []
--                 [ Block.text [] [ text "Check out the modules overview" ]
--                 , Block.custom <|
--                     Button.linkButton
--                         [ Button.primary, Button.attrs [ href "#modules" ] ]
--                         [ text "Module" ]
--                 ]
--             |> Card.view
--         ]
--     ]


pageGettingStarted : Model -> Html Msg
pageGettingStarted model =
    h2 [] [ text "Getting started" ]



-- , Button.button
--     [ Button.success
--     , Button.large
--     , Button.block
--     , Button.attrs [ onClick ShowModal ]
--     ]
--     [ text "Click me" ]


pageModules : Model -> List (Html Msg)
pageModules model =
    [ h1 [] [ text "Modules" ]

    -- , Listgroup.ul
    --     [ Listgroup.li [] [ text "Alert" ]
    --     , Listgroup.li [] [ text "Badge" ]
    --     , Listgroup.li [] [ text "Card" ]
    --     ]
    ]


pageNotFound_404 : Html Msg
pageNotFound_404 =
    div [] [ text "Sorry couldn't find that page" ]



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
            Sub.batch [ Time.every 1000 Tick ]



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
