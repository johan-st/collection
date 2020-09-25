module Tests exposing (..)

import Expect
import Fuzz exposing (int, intRange)
import Page.FizzBuzz exposing (Soda(..), carbonate)
import Page.RomanNumerals exposing (Numeral(..), toRoman)
import Test exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "Logic"
        [ describe "FizzBuzz"
            [ fuzz (intRange 1 9999) "Positive Numbers" <|
                \int ->
                    let
                        expected =
                            if modBy 15 int == 0 then
                                FizzyBuzzy

                            else if modBy 3 int == 0 then
                                Fizzy

                            else if modBy 5 int == 0 then
                                Buzzy

                            else
                                Uncarbonated int
                    in
                    carbonate int
                        |> Expect.equal expected
            , test "zero" <|
                \_ ->
                    carbonate 0
                        |> Expect.equal (Uncarbonated 0)
            , fuzz (intRange -9999 -1) "Negative numbers" <|
                \int ->
                    carbonate int
                        |> Expect.equal (Uncarbonated int)
            ]
        , describe "roman numerals"
            [ test "1 -> [I]" <| \_ -> toRoman 1 |> Expect.equal [ I ]
            , test "3 -> [III]" <| \_ -> toRoman 3 |> Expect.equal [ I, I, I ]
            , test "4 -> [IV]" <| \_ -> toRoman 4 |> Expect.equal [ I, V ]
            , test "5 -> [V]" <| \_ -> toRoman 5 |> Expect.equal [ V ]
            , test "8 -> [VIII]" <| \_ -> toRoman 8 |> Expect.equal [ V, I, I, I ]
            , test "9 -> [IX]" <| \_ -> toRoman 9 |> Expect.equal [ I, X ]
            , test "10 -> [X]" <| \_ -> toRoman 10 |> Expect.equal [ X ]
            ]
        ]
