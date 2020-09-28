module Page.Page exposing (..)

import Element exposing (..)
import Page.PrimeFactorization as Prime
import Page.RomanNumerals as Numerals exposing (Numeral)
import Page.Visuals as Visuals


type Page
    = Landing
    | FizzBuzz Float
    | RomanNumerals String (List Numeral)
    | PrimeFactorization Prime.Model
    | Diary
    | NotFound_404
    | Visuals Visuals.Model
