module Page.Page exposing (..)

import Element exposing (..)
import Json.Decode as D
import Json.Encode as E
import Page.FizzBuzz as FizzBuzz
import Page.PrimeFactorization as Prime
import Page.RomanNumerals as Numerals
import Page.Search as Search
import Page.Timer as Timer
import Page.Visuals as Visuals
import Url exposing (Url)


type Model
    = FB FizzBuzz.Model
    | N Numerals.Model
    | P Prime.Model
    | V Visuals.Model
    | T Timer.Model
    | S Search.Model


type Page
    = Home
    | Katas
    | FizzBuzz FizzBuzz.Model
    | RomanNumerals Numerals.Model
    | PrimeFactorization Prime.Model
    | Visuals Visuals.Model
    | Resources
    | Timer Timer.Model
    | Search Search.Model
    | NotFound_404


fromModel : Model -> Page
fromModel m =
    case m of
        FB model ->
            FizzBuzz model

        N model ->
            RomanNumerals model

        P model ->
            PrimeFactorization model

        V model ->
            Visuals model

        T model ->
            Timer model

        S model ->
            Search model
