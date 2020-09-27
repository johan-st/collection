module Page.PrimeFactorization exposing (factorize)


factorize : Int -> Maybe (List Int)
factorize int =
    if int < 2 then
        Nothing

    else
        Just <| fact ( int, [] )


fact : ( Int, List Int ) -> List Int
fact ( int, list ) =
    if int == 0 then
        List.reverse list

    else if modBy 2 int == 0 then
        fact ( int // 2, 2 :: list )

    else if modBy 3 int == 0 then
        fact ( int // 3, 3 :: list )

    else if modBy 5 int == 0 then
        fact ( int // 5, 5 :: list )

    else if modBy 7 int == 0 then
        fact ( int // 7, 7 :: list )

    else if modBy 11 int == 0 then
        fact ( int // 11, 11 :: list )

    else if modBy 13 int == 0 then
        fact ( int // 13, 13 :: list )

    else if modBy 17 int == 0 then
        fact ( int // 17, 17 :: list )

    else if modBy 19 int == 0 then
        fact ( int // 19, 19 :: list )

    else if modBy 21 int == 0 then
        fact ( int // 21, 21 :: list )

    else if modBy 23 int == 0 then
        fact ( int // 23, 23 :: list )

    else if modBy 29 int == 0 then
        fact ( int // 29, 29 :: list )

    else if modBy 31 int == 0 then
        fact ( int // 31, 31 :: list )

    else if modBy 37 int == 0 then
        fact ( int // 37, 37 :: list )

    else if modBy 41 int == 0 then
        fact ( int // 41, 41 :: list )

    else
        List.reverse list


primes : List Int
primes =
    [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31 ]
