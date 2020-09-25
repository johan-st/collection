module Page.Page exposing (..)

import Page.RomanNumerals exposing (Numeral)


type Page
    = Landing
    | FizzBuzz
    | RomanNumerals String (List Numeral)
    | Diary
    | NotFound_404
    | Visuals
