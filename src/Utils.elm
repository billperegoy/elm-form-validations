module Utils exposing (..)

import String


capitalize : String -> String
capitalize name =
    let
        first =
            String.left 1 name |> String.toUpper

        rest =
            String.dropLeft 1 name
    in
        first ++ rest
