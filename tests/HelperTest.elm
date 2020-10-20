module HelperTest exposing (..)

import Expect exposing (equal)
import Fuzz exposing (int, intRange)
import Page.Gallery exposing (Card , Image(..) ,toggleCard)
import Test exposing (..)
import Array 

helpers:Test
helpers = 
   describe "Gallery"
   [test "card selected toggle" <|
      \_->
         let
             cards = Array.fromList [Card None False, Card None False, Card None True]
             expectedResult = Array.fromList [Card None True, Card None True, Card None False]
         in
           cards
           |> toggleCard 1
           |> toggleCard 2
           |> toggleCard 0
                |> Expect.equal expectedResult
         
   ]