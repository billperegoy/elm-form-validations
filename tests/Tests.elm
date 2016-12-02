module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Dict
import Forms


formsTests : List Test
formsTests =
    [ test "initForm sets initial validate status as False" <|
        \() ->
            let
                form =
                    Forms.initForm
                        [ ( "field1", [ Forms.validateExistence, Forms.validateMaxLength 10 ] )
                        , ( "field2", [ Forms.validateExistence ] )
                        ]
            in
                Expect.equal form.validateStatus False
    , test "initForm stores correct number of form elements" <|
        \() ->
            let
                form =
                    Forms.initForm
                        [ ( "field1", [ Forms.validateExistence, Forms.validateMaxLength 10 ] )
                        , ( "field2", [ Forms.validateExistence ] )
                        ]
            in
                Expect.equal (form.elements |> Dict.keys |> List.length) 2
    ]


validateExistenceTests : List Test
validateExistenceTests =
    [ test "validateExistence success" <|
        \() ->
            let
                result =
                    Forms.validateExistence "abc"

                expected =
                    Nothing
            in
                Expect.equal expected result
    , test "validateExistence fail" <|
        \() ->
            let
                result =
                    Forms.validateExistence ""

                expected =
                    Just "must be present"
            in
                Expect.equal expected result
    ]


validateMinLengthTests : List Test
validateMinLengthTests =
    [ test "validateMinLength success" <|
        \() ->
            let
                result =
                    Forms.validateMinLength 5 "12345"

                expected =
                    Nothing
            in
                Expect.equal expected result
    , test "validateMinLength fail" <|
        \() ->
            let
                result =
                    Forms.validateMinLength 5 "1234"

                expected =
                    Just "must be at least 5 characters"
            in
                Expect.equal expected result
    ]


validateMaxLengthTests : List Test
validateMaxLengthTests =
    [ test "validateMaxLength success" <|
        \() ->
            let
                result =
                    Forms.validateMaxLength 5 "12345"

                expected =
                    Nothing
            in
                Expect.equal expected result
    , test "validateMaxLength fail" <|
        \() ->
            let
                result =
                    Forms.validateMaxLength 5 "123456"

                expected =
                    Just "must be 5 characters or fewer"
            in
                Expect.equal expected result
    ]


validateRegexTests : List Test
validateRegexTests =
    [ test "validateRegex success" <|
        \() ->
            let
                result =
                    Forms.validateRegex "^[a-h]+$" "abcdefgh"

                expected =
                    Nothing
            in
                Expect.equal expected result
    , test "validateRegex fail" <|
        \() ->
            let
                result =
                    Forms.validateRegex "^[a-h]+$" "abcdefghij"

                expected =
                    Just "invalid string format"
            in
                Expect.equal expected result
    ]


validateNumericalityTests : List Test
validateNumericalityTests =
    [ test "validateNumericality success" <|
        \() ->
            let
                result =
                    Forms.validateNumericality "95"

                expected =
                    Nothing
            in
                Expect.equal expected result
    , test "validateNumericality fail" <|
        \() ->
            let
                result =
                    Forms.validateNumericality "bad"

                expected =
                    Just "must be numeric"
            in
                Expect.equal expected result
    ]


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
            [ formsTests
            , validateExistenceTests
            , validateMinLengthTests
            , validateMaxLengthTests
            , validateRegexTests
            , validateNumericalityTests
            , validateNumericRangeTests
            , validateLessThanTests
            , validateGreaterThanTests
            , validateIsOneOfTests
            ]
        )
