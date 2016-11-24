module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Utils


all : Test
all =
    describe "A Test Suite"
        [ test "Capitalizes a lower case word" <|
            \() ->
                Expect.equal (Utils.capitalize "john") "John"
        , test "Does not change an already capitalized word" <|
            \() ->
                Expect.equal (Utils.capitalize "John") "John"
        ]
