module Forms
    exposing
        ( FieldValidator
        , Form
        , DependencyValidator
        , DependencyError(..)
        , FormElement
        , ValidationError(..)
        , errorList
        , errorString
        , formValue
        , initForm
        , initFormElement
        , updateFormInput
        , validateExistence
        , validateField
        , validateGreaterThan
        , validateIsOneOf
        , validateLessThan
        , validateMaxLength
        , validateMinLength
        , validateNumericRange
        , validateNumericality
        , validatePob
        , validateRegex
        , validateStatus
        , updateFormInputs
        , stringToInt
        , stringToFloat
          -- dependency validator
        , haveSameValue
        )

{-|


# Forms

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
type ValidationError
    = NotGreaterThan Int
    | DoesNotExist
    | MinLength Int
    | MaxLength Int
    | NotRegex String
    | NotNumeric
    | NotInNumericRange Int Int
    | NotLessThan Int
    | NotOneOf (List String)
    | InvalidPob
    | CustomError String


type DependencyError
    = DependencyError String String ValidationError


{-| A field validator is a function that takes a string to be validated and
returns a ValidationError (Maybe String)
-}
type alias FieldValidator =
    String -> Maybe ValidationError


type alias DependencyValidator =
    List ( String, FormElement ) -> Maybe DependencyError


{-| A form is a set of form elements and a boolean indicating the validate
status. Details of the FormElements type are not exposed to the user.
Instead an API is provided so those details can be fluid.
-}
type alias Form =
    { elements : FormElements
    , validateStatus : Bool
    , dependencyValidators : List DependencyValidator
    }


type alias FormElements =
    Dict.Dict String FormElement


type alias FormElement =
    { input : String
    , errors : List ValidationError
    , dependencyErrors : List DependencyError
    , validator : List FieldValidator
    }


initFormElement : ( String, List FieldValidator ) -> FormElement
initFormElement field =
    { input = ""
    , errors = validateField "" (Tuple.second field)
    , dependencyErrors = [] -- @TODO do we really want to initialize this as empty?
    , validator = Tuple.second field
    }



--changeFormInputByKeysIfEmpty : Form -> List ( String, String ) -> Form
--changeFormInputByKeysIfEmpty form list =
--    let
--        a =
--            List.map
--                (\( k, v ) ->
--                    (if (formValue form k == "") then
--                        ( k, changeFormElementInput form ( k, v ) )
--                     else
--                        ( k, changeFormElementInput form ( k, formValue form k ) )
--                    )
--                )
--                list
--        b =
--            List.concat [ Dict.toList form.elements, a ]
--    in
--        { elements = Dict.fromList b
--        , validateStatus = form.validateStatus
--        }
--changeFormElementInput : Form -> ( String, String ) -> FormElement
--changeFormElementInput form ( key, newValue ) =
--    let
--        a =
--            List.filter (\( k, v ) -> k == key) (Dict.toList form.elements)
--        b =
--            List.head a
--    in
--        case b of
--            Nothing ->
--                { input = ""
--                , errors = []
--                , validator = []
--                }
--            Just b ->
--                { input = newValue
--                , errors = (Tuple.second b).errors
--                , validator = (Tuple.second b).validator
--                }


updateFormInputs : Form -> List ( String, String ) -> Form
updateFormInputs f elems =
    List.foldl
        (\( key, value ) form ->
            if (formValue f key == "") then
                updateFormInput form key value
            else
                f
        )
        f
        elems



--changeShippingInfoForm2 : List ( String, FormElement ) -> ( String, String ) -> ( String, FormElement )
--changeShippingInfoForm2 elems ( key, value ) =
--    let
--        a =
--            List.map
--                (\( k, v ) ->
--                    if key == k then
--                        ( k, value )
--                    else
--                        ( k, v )
--                )
--                elems
--    in
--        case List.head (List.filter (\( c, d ) -> c == key) a) of
--            Nothing ->
--                ( key, value )
--            Just a ->
--                a


{-| Initializes a form.Accepts a list of (name, validator) tuples
and returns a Form.
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
        Form elements False []


{-| Given a Form, a field name and a feild value and returns
a new form containing the update. This is genrally called
from the application's update function.
-}
updateFormInput : Form -> String -> String -> Form
updateFormInput form name value =
    let
        formElement =
            Dict.get name form.elements

        newForm =
            case formElement of
                Just element ->
                    let
                        newFormElement =
                            { element
                                | input = value
                                , errors = validateField value element.validator
                                , dependencyErrors = []
                            }

                        tmpFormElements =
                            Dict.insert name
                                newFormElement
                                form.elements

                        dependencyErrors =
                            validateDependencies (Dict.toList tmpFormElements) form.dependencyValidators

                        newFormElements =
                            addDependencyErrorsToErrorList dependencyErrors (removeDependencyErrors tmpFormElements)
                    in
                        { form
                            | elements = newFormElements
                            , validateStatus =
                                validateForm { form | elements = newFormElements }
                        }

                Nothing ->
                    form
    in
        newForm


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


catMaybes : List (Maybe a) -> List a
catMaybes xs =
    List.foldl
        (\mElem newList ->
            case mElem of
                Nothing ->
                    newList

                Just elem ->
                    elem :: newList
        )
        []
        xs


lookupErrorValue : Form -> String -> List ValidationError
lookupErrorValue form name =
    let
        lookupValue =
            Dict.get name form.elements
    in
        case lookupValue of
            Just value ->
                List.concat
                    [ value.errors
                    , catMaybes
                        (List.map
                            (\(DependencyError from to validationError) ->
                                if name == from || name == to then
                                    Just validationError
                                else
                                    Nothing
                            )
                            value.dependencyErrors
                        )
                    ]

            Nothing ->
                []


validationErrorToString : ValidationError -> String
validationErrorToString err =
    case err of
        NotGreaterThan n ->
            "must be < " ++ toString n

        DoesNotExist ->
            "must be present"

        MinLength n ->
            "must be at least " ++ toString n ++ " characters"

        MaxLength n ->
            "must be " ++ toString n ++ " characters of fewer"

        NotRegex s ->
            "invalid string format"

        NotNumeric ->
            "must be numeric"

        NotInNumericRange from to ->
            "must be between " ++ toString from ++ " and " ++ toString to

        NotLessThan n ->
            "must be > " ++ toString n

        NotOneOf matches ->
            "must match one of (" ++ String.join ", " matches ++ ")"

        CustomError err ->
            err

        InvalidPob ->
            "Not valid pob"


errorsToString : List ValidationError -> String
errorsToString errors =
    let
        errorList =
            List.map validationErrorToString errors
    in
        if List.length errorList == 0 then
            "no errors"
        else
            String.join ", " errorList


{-| Returns the current value of any form field.
Returns Nothing if an invalid form name is given.
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


validateSingle : FormElement -> List ValidationError
validateSingle formElement =
    validateField formElement.input formElement.validator


{-| Returns a Bool representing the current validation status of a form.
-}
validateStatus : Form -> Bool
validateStatus form =
    form.validateStatus


validateForm : Form -> Bool
validateForm form =
    let
        errorElements =
            List.concatMap
                (\elem -> (Tuple.second elem).errors)
                (Dict.toList form.elements)

        dependencyErrors =
            List.concatMap
                (\elem -> (Tuple.second elem).dependencyErrors)
                (Dict.toList form.elements)
    in
        (errorElements |> List.length) == 0 && (dependencyErrors |> List.length) == 0


stringToInt str =
    String.toInt str |> Result.toMaybe |> Maybe.withDefault 100


stringToFloat str =
    String.toFloat str |> Result.toMaybe |> Maybe.withDefault 100


validateField : String -> List (String -> Maybe ValidationError) -> List ValidationError
validateField data validations =
    List.map (\e -> e data) validations
        |> List.filterMap identity


validateDependencies : List ( String, FormElement ) -> List DependencyValidator -> List DependencyError
validateDependencies allFormElems validations =
    List.map (\e -> e allFormElems) validations
        |> List.filterMap identity


removeDependencyErrors : Dict.Dict String FormElement -> Dict.Dict String FormElement
removeDependencyErrors =
    Dict.map (\key v -> { v | dependencyErrors = [] })


addDependencyErrorsToErrorList : List DependencyError -> Dict.Dict String FormElement -> Dict.Dict String FormElement
addDependencyErrorsToErrorList errors elems =
    List.foldl
        (\(DependencyError from to validationError) elems_ ->
            Dict.update to
                (\maybeE ->
                    case maybeE of
                        Nothing ->
                            Nothing

                        Just e ->
                            Just
                                { e
                                    | dependencyErrors =
                                        (DependencyError from to validationError)
                                            :: e.dependencyErrors
                                }
                )
                elems_
        )
        elems
        errors



-- Primitive validators


{-| Validates existance of a form vale.

    validateExistence "" == Just "must be present"
    validateExistence "valid" == Nothing

-}
validateExistence : String -> Maybe ValidationError
validateExistence string =
    if String.length string > 0 then
        Nothing
    else
        Just DoesNotExist


{-| Validates that a form element is of a minimum length.

    validateMinLength 2 "123" == Nothing
    validateMinLength 4 "123" == Just "must be at least 4 characters"

-}
validateMinLength : Int -> String -> Maybe ValidationError
validateMinLength minLength string =
    if String.length string >= minLength then
        Nothing
    else
        Just (MinLength minLength)


{-| Validates that a form element is less than a maximum length.

    validateMaxLength 4 "123" == Nothing
    validateMaxLength 2 "123" == Just "must be at least 2 characters or fewer"

-}
validateMaxLength : Int -> String -> Maybe ValidationError
validateMaxLength maxLength string =
    if String.length string <= maxLength then
        Nothing
    else
        Just (MaxLength maxLength)


{-| Validates that a form element matches a regular expression.
-}
validateRegex : String -> String -> Maybe ValidationError
validateRegex regex string =
    if Regex.contains (Regex.regex regex) string then
        Nothing
    else
        Just (NotRegex regex)


{-| Validates that a form element is a valid integer.
-}
validateNumericality : String -> Maybe ValidationError
validateNumericality string =
    let
        intValue =
            String.toInt string
    in
        case intValue of
            Ok value ->
                Nothing

            Err _ ->
                Just NotNumeric


validatePob : String -> Maybe ValidationError
validatePob string =
    case validateNumericRange 0 999999 string of
        Nothing ->
            Nothing

        Just err ->
            Just InvalidPob


{-| iValidates that an integer field is within a specified numeric range.
-}
validateNumericRange : Int -> Int -> String -> Maybe ValidationError
validateNumericRange min max string =
    let
        intValue =
            String.toInt string
    in
        case intValue of
            Ok value ->
                if value < min then
                    Just (NotGreaterThan min)
                else if value > max then
                    Just (NotLessThan max)
                else
                    Nothing

            Err _ ->
                Just (NotInNumericRange min max)


{-| Validates that a string converts to an integer and is less than the max
supplied

    validateLessThan 100 "95" == Nothing
    validateLessThan 100 "105" == Just "must be < 100"
    validateLessThan 100 "bad" == Just "must be < 100"

-}
validateLessThan : Int -> String -> Maybe ValidationError
validateLessThan num string =
    let
        intValue =
            String.toInt string
    in
        case intValue of
            Ok value ->
                if value >= num then
                    Just (NotLessThan num)
                else
                    Nothing

            Err _ ->
                Just (NotLessThan num)


{-| Validates that a string converts to an integer and is greater than the min
supplied

    validateGreaterThan 100 "105" == Nothing
    validateGreaterThan 100 "95" == Just "must be > 100"
    validateGreaterThan 100 "bad" == Just "must be > 100"

-}
validateGreaterThan : Int -> String -> Maybe ValidationError
validateGreaterThan num string =
    let
        intValue =
            String.toInt string
    in
        case intValue of
            Ok value ->
                if value <= num then
                    Just (NotGreaterThan num)
                else
                    Nothing

            Err _ ->
                Just (NotGreaterThan num)


{-| Validates that a string matches one of several potential matches

    validateIsOneOf [ "cat", "bat", "rat" ] "bat" == Nothing
    validateIsOneOf [ "cat", "bat", "rat" ] "brat" == Just "must match one of (cat, bat, rat)"

-}
validateIsOneOf : List String -> String -> Maybe ValidationError
validateIsOneOf matches string =
    if List.any (\e -> e == string) matches then
        Nothing
    else
        Just (NotOneOf matches)


{-| Validates that two form fields have equal values
-}
haveSameValue : String -> String -> List ( String, FormElement ) -> Maybe DependencyError
haveSameValue name sameAsName elems =
    let
        firstElem =
            dGet elems name

        secondElem =
            dGet elems sameAsName
    in
        case firstElem of
            Just ( _, elem1 ) ->
                case secondElem of
                    Just ( _, elem2 ) ->
                        if elem1.input == elem2.input then
                            Nothing
                        else
                            Just (DependencyError name sameAsName (CustomError "Lozinke moraju biti jednake"))

                    Nothing ->
                        Nothing

            Nothing ->
                Nothing


isCheckedThenValidate : String -> String -> List FieldValidator -> List ( String, FormElement ) -> Maybe DependencyError
isCheckedThenValidate checkboxName inputName validators elems =
    let
        mCheckbox =
            dGet elems checkboxName

        mInput =
            dGet elems inputName
    in
        case mCheckbox of
            Just ( _, checkbox ) ->
                case mInput of
                    Just ( _, input ) ->
                        if checkbox.input == "true" then
                            case List.head (validateSingle { input | validator = validators }) of
                                Nothing ->
                                    Nothing

                                Just e ->
                                    Just (DependencyError checkboxName inputName e)
                        else
                            Nothing

                    Nothing ->
                        Nothing

            Nothing ->
                Nothing


dGet : List ( a, b ) -> a -> Maybe ( a, b )
dGet l elem =
    List.head (List.filter (\( k, _ ) -> k == elem) l)
