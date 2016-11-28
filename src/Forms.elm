module Forms
    exposing
        ( FieldValidator
        , Form
        , ValidationError
        , errors
        , formValue
        , initForm
        , validateField
        , updateFormInput
        , validateLength
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


errors : Form -> String -> String
errors form name =
    lookupErrorValue form name |> errorString


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


errorString : ValidationErrors -> String
errorString errors =
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



{--
enhancements:

allow custom error messages
acceptance -checkbox is checked  (not nil)`
confirmation - two fields the same
exclusion - not within a set of values
inclusion -in a set of values
length - min and max
numeriality - ony integer or float
other compariosns vs.range (odd even, lt, gt, etc.)
absence - does this make sense in my scenario?
uniquemness - do I need a backend?
conditional validation? - form field dependencies?
ability to get raw messages as well as string join
 --}
