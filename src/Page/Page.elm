module Page.Page exposing (..)

import Element exposing (..)
import Json.Decode as D
import Json.Encode as E
import Page.FizzBuzz as FizzBuzz
import Page.Gallery as Gallery
import Page.Login as Login
import Page.PrimeFactorization as Prime
import Page.RomanNumerals as Numerals
import Page.Search as Search
import Page.Stack as Stack
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
    | ST Stack.Model
    | G Gallery.Model
    | L Login.Model


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
    | Stack Stack.Model
    | Gallery Gallery.Model
    | NotFound_404
    | Login Login.Model


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

        ST model ->
            Stack model

        G model ->
            Gallery model

        L model ->
            Login model
