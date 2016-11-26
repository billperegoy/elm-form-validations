module FormValidation exposing (..)

import Regex
import Dict exposing (..)


type alias ValidationErrors =
    List (Maybe String)


type alias FormElements =
    Dict String FormElement


type alias Form =
    { elements : FormElements
    , validateStatus : Bool
    }


type alias FormElement =
    { input : String
    , errors : ValidationErrors
    , validator : String -> ValidationErrors
    }


updateFormInput : Form -> String -> String -> Form
updateFormInput form name value =
    let
        formElement =
            Dict.get name form.elements
    in
        case formElement of
            Just f ->
                let
                    newFormElement =
                        { f
                            | input = value
                            , errors = f.validator value
                        }

                    newFormElements =
                        Dict.insert name
                            newFormElement
                            form.elements
                in
                    { form
                        | elements = newFormElements
                        , validateStatus = validateForm { form | elements = newFormElements }
                    }

            Nothing ->
                form


lookupInputValue : Form -> String -> String
lookupInputValue form name =
    let
        lookupValue =
            Dict.get name form.elements
    in
        case lookupValue of
            Just value ->
                value.input

            Nothing ->
                ""


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



-- Primitive validators


validateSingle : FormElement -> ValidationErrors
validateSingle formElement =
    formElement.input |> formElement.validator


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


runPrimitiveValidations : String -> List (String -> Maybe String) -> ValidationErrors
runPrimitiveValidations data validations =
    List.map (\e -> e data) validations


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
