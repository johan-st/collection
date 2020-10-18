module Utils.Utils exposing (timeString)

import Time as Time


timeString : Time.Zone -> Time.Posix -> String
timeString zone posix =
    let
        hourInt =
            Time.toHour zone posix

        hour =
            if hourInt < 10 then
                "0" ++ String.fromInt hourInt

            else
                String.fromInt hourInt

        minInt =
            Time.toMinute zone posix

        min =
            if minInt < 10 then
                "0" ++ String.fromInt minInt

            else
                String.fromInt minInt

        secInt =
            Time.toSecond zone posix

        sec =
            if secInt < 10 then
                "0" ++ String.fromInt secInt

            else
                String.fromInt secInt
    in
    hour ++ ":" ++ min ++ ":" ++ sec
