module FormValidation exposing (..)

import Regex


type alias ValidationErrors =
    List (Maybe String)


type alias FormElement =
    { input : String
    , errors : ValidationErrors
    , validator : String -> ValidationErrors
    }



-- Primitive validators


runFormValidations : form -> List ( String -> ValidationErrors, form -> String ) -> Bool
runFormValidations form validations =
    let
        allFormElements =
            List.concatMap
                (\e -> Tuple.second e form |> Tuple.first e)
                validations

        errorElements =
            List.filter (\e -> e /= Nothing) allFormElements
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
