module Session exposing (..)


type Session
    = Guest
    | Authenticated String
