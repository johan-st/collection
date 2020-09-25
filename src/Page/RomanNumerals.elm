module Page.RomanNumerals exposing (..)


type Numeral
    = I
    | V
    | X
    | L
    | C
    | D
    | M


toRoman : Int -> List Numeral
toRoman num =
    fn num []


fn : Int -> List Numeral -> List Numeral
fn x r =
    case x of
        0 ->
            r

        _ ->
            if x // 5 > 0 then
                fn (x - 5) (V :: r)
            else if x // 4 > 0 then
                fn (x - 4) (I :: V :: r)
            else
                fn (x - 1) (I :: r)
