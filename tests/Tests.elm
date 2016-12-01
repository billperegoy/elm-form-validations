module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Forms


validateNumericRangeTests : List Test
validateNumericRangeTests =
    [ test "validateNumericRange success" <|
        \() ->
            let
                result =
                    Forms.validateNumericRange 50 100 "95"

                expected =
                    Nothing
            in
                Expect.equal expected result
    , test "validateNumericRange fail below" <|
        \() ->
            let
                result =
                    Forms.validateNumericRange 50 100 "45"

                expected =
                    Just "must be >= 50"
            in
                Expect.equal expected result
    , test "validateNumericRange fail above" <|
        \() ->
            let
                result =
                    Forms.validateNumericRange 50 100 "105"

                expected =
                    Just "must be <= 100"
            in
                Expect.equal expected result
    , test "validateNumericRange fail bad value" <|
        \() ->
            let
                result =
                    Forms.validateNumericRange 50 100 "bad"

                expected =
                    Just "must be between 50 and 100"
            in
                Expect.equal expected result
    ]


validateLessThanTests : List Test
validateLessThanTests =
    [ test "validateLessThan success" <|
        \() ->
            let
                result =
                    Forms.validateLessThan 100 "95"

                expected =
                    Nothing
            in
                Expect.equal expected result
    , test "validateLessThan failure" <|
        \() ->
            let
                result =
                    Forms.validateLessThan 100 "105"

                expected =
                    Just "must be < 100"
            in
                Expect.equal expected result
    , test "validateLessThan bad input" <|
        \() ->
            let
                result =
                    Forms.validateLessThan 100 "bad"

                expected =
                    Just "must be < 100"
            in
                Expect.equal expected result
    ]


validateGreaterThanTests : List Test
validateGreaterThanTests =
    [ test "validateGreaterThan success" <|
        \() ->
            let
                result =
                    Forms.validateGreaterThan 100 "105"

                expected =
                    Nothing
            in
                Expect.equal expected result
    , test "validateGreaterThan failure" <|
        \() ->
            let
                result =
                    Forms.validateGreaterThan 100 "95"

                expected =
                    Just "must be > 100"
            in
                Expect.equal expected result
    , test "validateGreaterThan bad input" <|
        \() ->
            let
                result =
                    Forms.validateGreaterThan 100 "bad"

                expected =
                    Just "must be > 100"
            in
                Expect.equal expected result
    ]


validateIsOneOfTests : List Test
validateIsOneOfTests =
    [ test "validateIsOneOf success" <|
        \() ->
            let
                result =
                    Forms.validateIsOneOf [ "cat", "bat", "rat" ] "bat"

                expected =
                    Nothing
            in
                Expect.equal expected result
    , test "validateIsOneOf failure" <|
        \() ->
            let
                result =
                    Forms.validateIsOneOf [ "cat", "bat", "rat" ] "drat"

                expected =
                    Just "must match one of (cat, bat, rat)"
            in
                Expect.equal expected result
    ]


all : Test
all =
    describe "A Test Suite"
        (List.concat
            [ validateNumericRangeTests
            , validateLessThanTests
            , validateGreaterThanTests
            , validateIsOneOfTests
            ]
        )
