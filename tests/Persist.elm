module Persist exposing (..)

import Expect exposing (equal)
import Fuzz exposing (int, intRange)
import Json.Decode as D
import Page.FizzBuzz as FizzBuzz exposing (modelDecoder, modelEncoder)
import Page.Page exposing (Page(..))
import Page.PrimeFactorization as Prime exposing (modelDecoder, modelEncoder)
import Page.RomanNumerals as Numerals exposing (modelDecoder, modelEncoder)
import Page.Timer as Timer exposing (modelDecoder, modelEncoder)
import Test exposing (..)


persistance : Test
persistance =
    describe "Encode / Decode"
        [ test "FizzBuzz" <|
            \_ ->
                let
                    initial : FizzBuzz.Model
                    initial =
                        FizzBuzz.init
                in
                initial
                    |> FizzBuzz.modelEncoder
                    |> D.decodeValue FizzBuzz.modelDecoder
                    |> Expect.equal (Ok initial)
        , test "Prime" <|
            \_ ->
                let
                    initial : Prime.Model
                    initial =
                        { input = "0" }
                in
                initial
                    |> Prime.modelEncoder
                    |> D.decodeValue Prime.modelDecoder
                    |> Expect.equal (Ok initial)
        , test "Roman Numerals" <|
            \_ ->
                let
                    initial : Numerals.Model
                    initial =
                        Numerals.init
                in
                initial
                    |> Numerals.modelEncoder
                    |> D.decodeValue Numerals.modelDecoder
                    |> Expect.equal (Ok initial)
        , test "Timer" <|
            \_ ->
                let
                    initial : Timer.Model
                    initial =
                        Timer.init
                in
                initial
                    |> Timer.modelEncoder
                    |> D.decodeValue Timer.modelDecoder
                    |> Expect.equal (Ok initial)
        ]
