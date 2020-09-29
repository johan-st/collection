module Page.Page exposing (..)

import Element exposing (..)
import Json.Decode as D
import Json.Encode as E
import Page.FizzBuzz as FizzBuzz
import Page.PrimeFactorization as Prime
import Page.RomanNumerals as Numerals exposing (numeralEncoder)
import Page.Visuals as Visuals


type Page
    = Landing
    | FizzBuzz FizzBuzz.Model
    | RomanNumerals Numerals.Model
    | PrimeFactorization Prime.Model
    | Visuals Visuals.Model
    | Diary
    | NotFound_404



-- ENCODE / DECODE --


pageEncoder : Page -> E.Value
pageEncoder p =
    case p of
        FizzBuzz model ->
            FizzBuzz.modelEncoder model

        RomanNumerals model ->
            Numerals.modelEncoder model

        PrimeFactorization model ->
            Prime.modelEncoder model

        _ ->
            E.null


fizzbuzzDecoder : D.Decoder Page
fizzbuzzDecoder =
    D.map FizzBuzz FizzBuzz.modelDecoder


numeralsDecoder : D.Decoder Page
numeralsDecoder =
    D.map RomanNumerals Numerals.modelDecoder


primeDecoder : D.Decoder Page
primeDecoder =
    D.map PrimeFactorization Prime.modelDecoder
