module Forms
    exposing
        ( FieldValidator
        , Form
        , ValidationError
        , errorList
        , errorString
        , formValue
        , initForm
        , validateField
        , updateFormInput
        , validateGreaterThan
        , validateIsOneOf
        , validateLength
        , validateLessThan
        , validateExistence
        , validateNumericality
        , validateNumericRange
        , validateRegex
        , validateStatus
        )

import Regex
import Dict


type alias ValidationError =
    Maybe String


type alias ValidationErrors =
    List ValidationError


type alias FormElements =
    Dict.Dict String FormElement


type alias FieldValidator =
    String -> ValidationError


type alias Form =
    { elements : FormElements
    , validateStatus : Bool
    }


type alias FormElement =
    { input : String
    , errors : ValidationErrors
    , validator : List FieldValidator
    }


initFormElement : ( String, List FieldValidator ) -> FormElement
initFormElement field =
    { input = ""
    , errors = validateField "" (Tuple.second field)
    , validator = Tuple.second field
    }


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


errorString : Form -> String -> String
errorString form name =
    lookupErrorValue form name |> errorsToString


errorList : Form -> String -> ValidationErrors
errorList form name =
    lookupErrorValue form name


lookupErrorValue : Form -> String -> ValidationErrors
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


errorsToString : ValidationErrors -> String
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


validateSingle : FormElement -> ValidationErrors
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


validateField : String -> List (String -> Maybe String) -> ValidationErrors
validateField data validations =
    List.map (\e -> e data) validations


validateStatus : Form -> Bool
validateStatus form =
    form.validateStatus



-- Primitive validators


validateExistence : String -> Maybe String
validateExistence string =
    if String.length string > 0 then
        Nothing
    else
        Just "must be present"


validateLength : Int -> String -> Maybe String
validateLength minLength string =
    if String.length string >= minLength then
        Nothing
    else
        Just ("must be at least " ++ toString minLength ++ " characters")


validateRegex : String -> String -> Maybe String
validateRegex regex string =
    if Regex.contains (Regex.regex regex) string then
        Nothing
    else
        Just "invalid string format"


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


validateIsOneOf : List String -> String -> Maybe String
validateIsOneOf matches string =
    if (List.any (\e -> e == string) matches) then
        Nothing
    else
        Just ("must match one of (" ++ String.join ", " matches ++ ")")
