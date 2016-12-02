module Forms
    exposing
        ( FieldValidator
        , Form
        , ValidationError
        , errorList
        , errorString
        , formValue
        , initForm
        , updateFormInput
        , validateGreaterThan
        , validateIsOneOf
        , validateMinLength
        , validateMaxLength
        , validateLessThan
        , validateExistence
        , validateNumericality
        , validateNumericRange
        , validateRegex
        , validateStatus
        )

{-| # Forms
This library performs form validation for HTML forms. A user can define
a list of form elements and define validations for each element. Each time the
formis updated, validations are automatically run and validation results are updated
for each form element as well as the overall form.

API calls can be used to retrieve validation information that can be used to
display validation errors on the form itself.

# Definition

# Types
@docs FieldValidator, Form, ValidationError

# Helper Functions
@docs errorList, errorString, formValue, updateFormInput, validateStatus

# Form Initializer
@docs initForm


# Primitive Validators
@docs validateGreaterThan, validateIsOneOf, validateMinLength
@docs validateMaxLength, validateLessThan, validateExistence
@docs validateNumericality, validateNumericRange, validateRegex

-}

import Dict
import Regex


{-| A validation result is simply a Maybe String.
    Nothing indicates no error. Otherwise it's Just "error message"
-}
type alias ValidationError =
    Maybe String


{-| A field validator is a function that takes a string to be validated and
    returns a ValidationError (Maybe String)
-}
type alias FieldValidator =
    String -> ValidationError


{-| A form is a set of form elements and a boolean indicating the validate
    status. Details of the FormElements type are not exposed to the user.
    Instead an API is provided so those details can be fluid.
-}
type alias Form =
    { elements : FormElements
    , validateStatus : Bool
    }


type alias FormElements =
    Dict.Dict String FormElement


type alias FormElement =
    { input : String
    , errors : List ValidationError
    , validator : List FieldValidator
    }


initFormElement : ( String, List FieldValidator ) -> FormElement
initFormElement field =
    { input = ""
    , errors = validateField "" (Tuple.second field)
    , validator = Tuple.second field
    }


{-| TBD
-}
initForm : List ( String, List FieldValidator ) -> Form
initForm fields =
    let
        elements =
            List.foldl
                (\e -> Dict.insert (Tuple.first e) (initFormElement e))
                Dict.empty
                fields
    in
        Form elements False


{-| TBD
-}
updateFormInput : Form -> String -> String -> Form
updateFormInput form name value =
    let
        formElement =
            Dict.get name form.elements
    in
        case formElement of
            Just element ->
                let
                    newFormElement =
                        { element
                            | input = value
                            , errors = validateField value element.validator
                        }

                    newFormElements =
                        Dict.insert name
                            newFormElement
                            form.elements
                in
                    { form
                        | elements = newFormElements
                        , validateStatus =
                            validateForm { form | elements = newFormElements }
                    }

            Nothing ->
                form


{-| Returns a formatted string representing all current validation errors.

    ErrorSring form "user" == "must be present, must be 10 characters or fewer"
-}
errorString : Form -> String -> String
errorString form name =
    lookupErrorValue form name |> errorsToString


{-| Returns raw list of all current validation errors."

    ErrorList form "user" == [ Just "must be present", Just "must be 10 characters or fewer" ]
-}
errorList : Form -> String -> List ValidationError
errorList form name =
    lookupErrorValue form name


lookupErrorValue : Form -> String -> List ValidationError
lookupErrorValue form name =
    let
        lookupValue =
            Dict.get name form.elements
    in
        case lookupValue of
            Just value ->
                value.errors

            Nothing ->
                []


errorsToString : List ValidationError -> String
errorsToString errors =
    let
        errorList =
            List.filter (\e -> e /= Nothing) errors
                |> List.map (\e -> Maybe.withDefault "" e)
    in
        if List.length errorList == 0 then
            "no errors"
        else
            String.join ", " errorList


{-| TBD
-}
formValue : Form -> String -> String
formValue form name =
    let
        lookupValue =
            Dict.get name form.elements
    in
        case lookupValue of
            Just value ->
                value.input

            Nothing ->
                ""



-- Validation


{-| Determine if a string passed in from a form has any data.
    Returns Nothing if value is present and an error string
    otherwise.

    validateExistence "" == Just "must be present"
    validateExistence "form value" == Nothing
-}
validateSingle : FormElement -> List ValidationError
validateSingle formElement =
    validateField formElement.input formElement.validator


validateForm : Form -> Bool
validateForm form =
    let
        errorElements =
            List.concatMap
                (\elem -> validateSingle (Tuple.second elem))
                (Dict.toList form.elements)
                |> List.filter (\e -> e /= Nothing)
    in
        (errorElements |> List.length) == 0


validateField : String -> List (String -> Maybe String) -> List ValidationError
validateField data validations =
    List.map (\e -> e data) validations


{-| TBD
-}
validateStatus : Form -> Bool
validateStatus form =
    form.validateStatus



-- Primitive validators


{-| TBD
-}
validateExistence : String -> Maybe String
validateExistence string =
    if String.length string > 0 then
        Nothing
    else
        Just "must be present"


{-| TBD
-}
validateMinLength : Int -> String -> Maybe String
validateMinLength minLength string =
    if String.length string >= minLength then
        Nothing
    else
        Just ("must be at least " ++ toString minLength ++ " characters")


{-| TBD
-}
validateMaxLength : Int -> String -> Maybe String
validateMaxLength maxLength string =
    if String.length string <= maxLength then
        Nothing
    else
        Just ("must be " ++ toString maxLength ++ " characters or fewer")


{-| TBD
-}
validateRegex : String -> String -> Maybe String
validateRegex regex string =
    if Regex.contains (Regex.regex regex) string then
        Nothing
    else
        Just "invalid string format"


{-| TBD
-}
validateNumericality : String -> Maybe String
validateNumericality string =
    let
        intValue =
            String.toInt string
    in
        case intValue of
            Ok value ->
                Nothing

            Err _ ->
                Just "must be numeric"


{-| TBD
-}
validateNumericRange : Int -> Int -> String -> Maybe String
validateNumericRange min max string =
    let
        intValue =
            String.toInt string
    in
        case intValue of
            Ok value ->
                if value < min then
                    Just ("must be >= " ++ toString min)
                else if value > max then
                    Just ("must be <= " ++ toString max)
                else
                    Nothing

            Err _ ->
                Just ("must be between " ++ toString min ++ " and " ++ toString max)


{-| Validates that a string converts to an integer and is less than the max
supplied

    validateLessThan 100 "95" == Nothing
    validateLessThan 100 "105" == Just "must be < 100"
    validateLessThan 100 "bad" == Just "must be < 100"
-}
validateLessThan : Int -> String -> Maybe String
validateLessThan num string =
    let
        intValue =
            String.toInt string
    in
        case intValue of
            Ok value ->
                if value >= num then
                    Just ("must be < " ++ toString num)
                else
                    Nothing

            Err _ ->
                Just ("must be < " ++ toString num)


{-| Validates that a string converts to an integer and is greater than the min
supplied

    validateGreaterThan 100 "105" == Nothing
    validateGreaterThan 100 "95" == Just "must be > 100"
    validateGreaterThan 100 "bad" == Just "must be > 100"
-}
validateGreaterThan : Int -> String -> Maybe String
validateGreaterThan num string =
    let
        intValue =
            String.toInt string
    in
        case intValue of
            Ok value ->
                if value <= num then
                    Just ("must be > " ++ toString num)
                else
                    Nothing

            Err _ ->
                Just ("must be > " ++ toString num)


{-| Validates that a string matches one of several potential matches

    validateIsOneOf [ "cat", "bat", "rat" ] "bat" == Nothing
    validateIsOneOf [ "cat", "bat", "rat" ] "brat" == Just "must match one of (cat, bat, rat)"
-}
validateIsOneOf : List String -> String -> Maybe String
validateIsOneOf matches string =
    if (List.any (\e -> e == string) matches) then
        Nothing
    else
        Just ("must match one of (" ++ String.join ", " matches ++ ")")
