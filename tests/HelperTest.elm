module HelperTest exposing (..)

import Array
import Expect
import Page.Gallery exposing (Card, toggleCard)
import Test exposing (..)


helpers : Test
helpers =
    let
        image =
            { id = "id"
            , description = Nothing
            , alt = Nothing
            , url = "url"
            , likes = 42
            , user =
                { name = "user.name"
                , location = Nothing
                , bio = Nothing
                }
            }
    in
    describe "Gallery"
        [ test "card selected toggle" <|
            \_ ->
                let
                    cards =
                        Array.fromList [ Card image False, Card image False, Card image False ]

                    expectedResult =
                        Array.fromList [ Card image True, Card image False, Card image False ]
                in
                cards
                    |> toggleCard 1
                    |> toggleCard 2
                    |> toggleCard 2
                    |> toggleCard 0
                    |> Expect.equal expectedResult
        ]
