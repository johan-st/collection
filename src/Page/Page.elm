module Page.Page exposing (..)

import Element exposing (..)
import Page.FizzBuzz as FizzBuzz
import Page.PrimeFactorization as Prime
import Page.RomanNumerals as Numerals exposing (Numeral)
import Page.Visuals as Visuals


type Page
    = Landing
    | FizzBuzz FizzBuzz.Model
    | RomanNumerals Numerals.Model
    | PrimeFactorization Prime.Model
    | Diary
    | NotFound_404
    | Visuals Visuals.Model
