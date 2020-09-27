module Page.Page exposing (..)

import Element exposing (..)
import Page.RomanNumerals exposing (Numeral)


type Page
    = Landing
    | FizzBuzz Float
    | RomanNumerals String (List Numeral)
    | PrimeFact String
    | Diary
    | NotFound_404
    | Visuals
