module Tests exposing (..)

import Expect
import Fuzz exposing (int, intRange)
import Page.FizzBuzz exposing (Soda(..), carbonate)
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
        ]
