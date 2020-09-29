module Page.Page exposing (..)

import Element exposing (..)
import Page.FizzBuzz as FizzBuzz
import Page.PrimeFactorization as Prime
import Page.RomanNumerals as Numerals
import Page.Visuals as Visuals


type Page
    = Landing
    | FizzBuzz FizzBuzz.Model
    | RomanNumerals Numerals.Model
    | PrimeFactorization Prime.Model
    | Visuals Visuals.Model
    | Diary
    | NotFound_404


-- toFizzBuzzModel : Page -> FizzBuzz.Model
-- toFizzBuzzModel page =
--     case page of
--         FizzBuzz model ->
--             model

--         _ ->
--             Debug.todo "toFizzBuzzModel called with non-fizbuzz page"
