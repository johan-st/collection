port module Main exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, src, type_)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E exposing (encode)
import Page.FizzBuzz as FizzBuzz exposing (Msg(..), Soda(..))
import Page.Gallery as Gallery
import Page.Page as Page exposing (Page(..))
import Page.PrimeFactorization as Prime
import Page.Resources as Resources
import Page.RomanNumerals as Numeral exposing (Numeral(..))
import Page.Search as Search
import Page.Stack as Stack
import Page.Timer as Timer exposing (..)
import Page.Visuals as Visuals
import Task
import Time
import Url exposing (Url)
import Url.Builder exposing (relative)
import Url.Parser as UrlParser exposing ((</>), Parser, s, top)
import Utils.Color as C
import Utils.Utils exposing (timeString)


type alias Flags =
    String



-- TODO: ! Build progressbar for salt course, week, day


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

                "/gallery" ->
                    Page.Gallery persist.gallery

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
                Stack.init
                Gallery.init

        Ok data ->
            data


type Msg
    = ClickedLink UrlRequest
    | UrlChange Url
    | Redirect String
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | GotFizzBuzzMsg FizzBuzz.Msg
    | GotNumeralMsg Numeral.Msg
    | GotPrimeMsg Prime.Msg
    | GotVisualsMsg Visuals.Msg
    | GotSearchMsg Search.Msg
    | GotTimerMsg Timer.Msg
    | GotResourceMsg Resources.Msg
    | GotStackMsg Stack.Msg
    | GotGalleryMsg Gallery.Msg
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

        Redirect url ->
            ( model, Navigation.pushUrl model.navKey url )

        Tick time ->
            ( { model | time = time }, setPersist (E.encode 1 (persistEncoder model.persistance)) )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }, Cmd.none )

        GotFizzBuzzMsg fizzBuzzMsg ->
            case model.page of
                Page.FizzBuzz fizzBuzzModel ->
                    toFizzBuzz model (FizzBuzz.update fizzBuzzMsg fizzBuzzModel)

                _ ->
                    ( model, Cmd.none )

        GotNumeralMsg fizzBuzzMsg ->
            case model.page of
                RomanNumerals fizzBuzzModel ->
                    toNumeral model (Numeral.update fizzBuzzMsg fizzBuzzModel)

                _ ->
                    ( model, Cmd.none )

        GotPrimeMsg primeMsg ->
            case model.page of
                PrimeFactorization primeModel ->
                    toPrime model (Prime.update primeMsg primeModel)

                _ ->
                    ( model, Cmd.none )

        GotTimerMsg timerMsg ->
            case model.page of
                Page.Timer timerModel ->
                    toTimer model (Timer.update timerMsg timerModel)

                _ ->
                    ( model, Cmd.none )

        GotVisualsMsg _ ->
            ( model, Cmd.none )

        GotSearchMsg _ ->
            ( model, Cmd.none )

        GotResourceMsg _ ->
            ( model, Cmd.none )

        GotStackMsg _ ->
            ( model, Cmd.none )

        GotGalleryMsg galleryMsg ->
            case model.page of
                Page.Gallery galleryModel ->
                    toGallery model (Gallery.update galleryMsg galleryModel)

                _ ->
                    ( model, Cmd.none )

        UpdateLocalStorage ->
            ( model, Cmd.none )



-- _ ->
--     Debug.todo "main update function"


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

                "/stack" ->
                    Page.fromModel <| Page.ST model.persistance.stack

                "/gallery" ->
                    Page.fromModel <| Page.G model.persistance.gallery

                _ ->
                    Page.NotFound_404
    in
    ( { model | url = url, page = page }
    , Cmd.none
    )


view : Model -> Browser.Document Msg
view model =
    { title = timeString model.zone model.time ++ " - </salty noodles>"
    , body =
        [ menu model
        , mainContent model
        , footer model
        ]
    }


menu : Model -> Html Msg
menu model =
    nav [ class "main-nav" ]
        [ a [ href "/", class "main-nav__brand  main-nav__link" ] [ text "</salty noodles>" ]
        , ul [ class "main-nav__list " ]
            [ li [ class "main-nav__list-item" ] [ a [ class "main-nav__link", href "/katas" ] [ text "katas" ] ]
            , li [ class "main-nav__list-item" ] [ a [ class "main-nav__link", href "/resources" ] [ text "links" ] ]
            , li [ class "main-nav__list-item" ] [ a [ class "main-nav__link", href "/gallery" ] [ text "gallery" ] ]
            , li [ class "main-nav__list-item" ] [ a [ class "main-nav__link", href "/stack" ] [ text "stack" ] ]
            ]
        , button [ class "main-nav__clock" ] [ a [ class "main-nav__link main-nav__clock", href "/timer" ] [ text (timeString model.zone model.time) ] ]
        ]


mainContent : Model -> Html Msg
mainContent model =
    case model.page of
        Home ->
            section [ class "main" ] [ pageHome model ]

        Katas ->
            section [ class "main" ] [ pageKatasOverview model ]

        FizzBuzz fizzBuzzModel ->
            section [ class "main" ] [ FizzBuzz.view model.persistance.fizzbuzz |> Html.map GotFizzBuzzMsg ]

        RomanNumerals numeralModel ->
            section [ class "main" ] [ Numeral.view model.persistance.numerals |> Html.map GotNumeralMsg ]

        PrimeFactorization primeModel ->
            section [ class "main" ] [ Prime.view model.persistance.primeFactors |> Html.map GotPrimeMsg ]

        Page.Timer timerModel ->
            section [ class "main" ] [ Timer.view model.persistance.timer |> Html.map GotTimerMsg ]

        Resources ->
            section [ class "main" ] [ Resources.view |> Html.map GotResourceMsg ]

        Visuals visualsModel ->
            section [ class "main" ] [ Visuals.view visualsModel |> Html.map GotVisualsMsg ]

        Stack stackModel ->
            section [ class "main" ] [ Stack.view stackModel |> Html.map GotStackMsg ]

        Gallery galleryModel ->
            section [ class "main" ] [ Gallery.view galleryModel |> Html.map GotGalleryMsg ]

        _ ->
            section [ class "main" ] [ pageNotFound_404 ]


pageKatasOverview : Model -> Html Msg
pageKatasOverview model =
    article [ class "kata-links" ]
        [ h1 [ class "kata-links__heading" ] [ text "Katas" ]
        , section [ class "kata-links__card", onClick (Redirect "/katas/fizzbuzz") ]
            [ h4 [] [ a [ class "kata-links__link", href "/katas/fizzbuzz" ] [ text "Fizz Buzz" ] ]
            , p [ class "kata-links__description" ] [ text "..is a group word game for children to teach them about division. Players take turns to count incrementally, replacing any number divisible by three with the word \"fizz\", and any number divisible by five with the word \"buzz\". " ]
            ]
        , section [ class "kata-links__card", onClick (Redirect "/katas/numerals") ]
            [ h4 [] [ a [ class "kata-links__link", href "/katas/numerals" ] [ text "Roman Numerals" ] ]
            , p [ class "kata-links__description" ] [ text "Roman numerals are a numeral system that originated in ancient Rome and remained the usual way of writing numbers throughout Europe well into the Late Middle Ages. " ]
            ]
        , section [ class "kata-links__card", onClick (Redirect "/katas/primes") ]
            [ h4 [] [ a [ class "kata-links__link", href "/katas/primes" ] [ text "Primes" ] ]
            , p [ class "kata-links__description" ] [ text "In number theory, integer factorization is the decomposition of a composite number into a product of smaller integers. If these factors are further restricted to prime numbers, the process is called prime factorization. " ]
            ]
        ]


pageHome : Model -> Html Msg
pageHome model =
    section [ class "home" ]
        [ img [ class "home__illustration", src "https://media.giphy.com/media/RIpevxkTjCXW46Osr5/giphy.gif" ] []
        , h1 [ class "home__heading" ] [ text "Welcome noodle!" ]
        , p [ class "home__text" ] [ text "to a place of links and playthings.. (mostly links for now)" ]
        , h2 [ class "home__subheading" ] [ text "here's a few links.." ]
        , p [ class "home__text" ] [ text "more availible through the navbar up top" ]
        , ul [ class "home__link-list" ]
            [ li [ class "home__list-item" ] [ a [ class "home__link", href "https://appliedtechnology.github.io/protips/" ] [ text "Pro Tips" ] ]
            , li [ class "home__list-item" ] [ a [ class "home__link", href "https://expressjs.com/" ] [ text "express.js" ] ]
            , li [ class "home__list-item" ] [ a [ class "home__link", href "https://fonts.google.com/" ] [ text "Google Fonts" ] ]
            , li [ class "home__list-item" ] [ a [ class "home__link", href "https://www.pexels.com/" ] [ text "pexels (photo, video)" ] ]
            ]
        ]


pageNotFound_404 : Html Msg
pageNotFound_404 =
    section [ class "404__heading" ]
        [ h1 [] [ text "404: not found" ]
        , img [ class "404__illustration", src "https://media.giphy.com/media/QWFz3HbZpk4aiu9vZC/giphy.gif" ] []
        ]


footer : model -> Html Msg
footer model =
    Html.footer [ class "footer" ]
        [ blockquote [] [ text "Slow is smooth. Smooth is fast." ]
        , cite [] [ text "/ John Sensei" ]
        , p [ class "footer__version-info" ] [ text "build: 20201020__1254 " ]
        ]



-- ENCODE / DECODE --


persistDecoderHelper : String -> D.Decoder PersistV2
persistDecoderHelper versionString =
    case versionString of
        "v2" ->
            D.map6 PersistV2
                (D.field "fizzbuzz" FizzBuzz.modelDecoder)
                (D.field "numerals" Numeral.modelDecoder)
                (D.field "prime" Prime.modelDecoder)
                (D.field "timer" Timer.modelDecoder)
                (D.field "stack" Stack.modelDecoder)
                (D.field "gallery" Gallery.modelDecoder)

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
    , stack : Stack.Model
    , gallery : Gallery.Model
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



-- MODEL MAPPING


toPrime : Model -> ( Prime.Model, Cmd Prime.Msg ) -> ( Model, Cmd Msg )
toPrime model ( primeModel, cmd ) =
    let
        newPrime =
            Page.PrimeFactorization primeModel

        persist =
            model.persistance

        newPersist =
            { persist | primeFactors = primeModel }
    in
    ( { model | page = newPrime, persistance = newPersist }
    , Cmd.map GotPrimeMsg cmd
    )


toNumeral : Model -> ( Numeral.Model, Cmd Numeral.Msg ) -> ( Model, Cmd Msg )
toNumeral model ( numModel, cmd ) =
    let
        newNumeral =
            Page.RomanNumerals numModel

        persist =
            model.persistance

        newPersist =
            { persist | numerals = numModel }
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
            { persist | fizzbuzz = fizzBuzzModel }
    in
    ( { model | page = newFizz, persistance = newPersist }
    , Cmd.map GotFizzBuzzMsg cmd
    )


toTimer : Model -> ( Timer.Model, Cmd Timer.Msg ) -> ( Model, Cmd Msg )
toTimer model ( timerModel, cmd ) =
    let
        newTimer =
            Page.Timer timerModel

        persist =
            model.persistance

        newPersist =
            { persist | timer = timerModel }
    in
    ( { model | page = Page.Timer timerModel, persistance = newPersist }
    , Cmd.map GotTimerMsg cmd
    )


toGallery : Model -> ( Gallery.Model, Cmd Gallery.Msg ) -> ( Model, Cmd Msg )
toGallery model ( galleryModel, cmd ) =
    let
        newGallery =
            Page.Gallery galleryModel

        persist =
            model.persistance

        newPersist =
            { persist | gallery = galleryModel }
    in
    ( { model | page = Page.Gallery galleryModel, persistance = newPersist }
    , Cmd.map GotGalleryMsg cmd
    )



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
