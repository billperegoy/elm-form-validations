module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Regex


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias ValidationErrors =
    List (Maybe String)


type alias SignupForm =
    { email : FormElement
    , password : FormElement
    , validateStatus : Bool
    }


type alias FormElement =
    { input : String
    , errors : ValidationErrors
    , validator : String -> ValidationErrors
    }


type alias Model =
    { signupForm : SignupForm
    }


init : ( Model, Cmd Msg )
init =
    { signupForm =
        { email = initEmail
        , password = initPassword
        , validateStatus = False
        }
    }
        ! []


initEmail : FormElement
initEmail =
    { input = ""
    , errors = (validateEmail "")
    , validator = validateEmail
    }


initPassword : FormElement
initPassword =
    { input = ""
    , errors = (validatePassword "")
    , validator = validatePassword
    }



-- Update


type Msg
    = UpdateEmailText String
    | UpdatePasswordText String


updateFormElement : FormElement -> String -> FormElement
updateFormElement formElement text =
    { formElement
        | input = text
        , errors = formElement.validator text
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateEmailText text ->
            let
                signupForm =
                    model.signupForm

                newFormElement =
                    updateFormElement signupForm.email text

                newSignupForm =
                    { signupForm
                        | email = newFormElement
                        , validateStatus =
                            validateForm { signupForm | email = newFormElement }
                    }
            in
                { model | signupForm = newSignupForm } ! []

        UpdatePasswordText text ->
            let
                signupForm =
                    model.signupForm

                newFormElement =
                    updateFormElement signupForm.password text

                newSignupForm =
                    { signupForm
                        | password = newFormElement
                        , validateStatus =
                            validateForm { signupForm | password = newFormElement }
                    }
            in
                { model | signupForm = newSignupForm } ! []



-- Form Validator


validateForm : SignupForm -> Bool
validateForm form =
    runFormValidations form
        [ ( validateEmail, (\elem -> elem.email.input) )
        , ( validatePassword, (\elem -> elem.password.input) )
        ]



-- Form Element Validators


validateEmail : String -> ValidationErrors
validateEmail string =
    let
        emailRegex =
            "^\\w+@\\w+\\.\\w+$"
    in
        runPrimitiveValidations
            string
            [ validateExistence, validateRegex emailRegex ]


validatePassword : String -> ValidationErrors
validatePassword string =
    runPrimitiveValidations
        string
        [ validateExistence, validateLength 10 ]



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



-- View


errorString : ValidationErrors -> String
errorString errors =
    let
        errorList =
            List.filter (\e -> e /= Nothing) errors
                |> List.map (\e -> Maybe.withDefault "" e)
    in
        String.join ", " errorList


view : Model -> Html Msg
view model =
    div [ class "container", style [ ( "width", "300px" ) ] ]
        [ p [] [ text ("Form validate status: " ++ toString model.signupForm.validateStatus) ]
        , Html.form []
            [ div [ class "form-group" ]
                [ label [ for "exampleInputEmail1" ] [ text "Email address" ]
                , input
                    [ class "form-control"
                    , id "exampleInputEmail1"
                    , placeholder "Enter email"
                    , type_ "email"
                    , onInput UpdateEmailText
                    ]
                    []
                , small [ class "form-text text-muted" ]
                    [ text (errorString model.signupForm.email.errors) ]
                ]
            , div [ class "form-group" ]
                [ label [ for "exampleInputPassword1" ] [ text "Password" ]
                , input
                    [ class "form-control"
                    , id "exampleInputPassword1"
                    , placeholder "Password"
                    , type_ "password"
                    , onInput UpdatePasswordText
                    ]
                    []
                , small [ class "form-text text-muted" ]
                    [ text (errorString model.signupForm.password.errors) ]
                ]
            , div [ class "form-group" ]
                [ label [ for "exampleSelect1" ]
                    [ text "Example select" ]
                , select [ class "form-control", id "exampleSelect1" ]
                    [ option []
                        [ text "1" ]
                    , option []
                        [ text "2" ]
                    , option []
                        [ text "3" ]
                    , option []
                        [ text "4" ]
                    , option []
                        [ text "5" ]
                    ]
                ]
            , div [ class "form-group" ]
                [ label [ for "exampleSelect2" ]
                    [ text "Example multiple select" ]
                , select [ class "form-control", id "exampleSelect2", attribute "multiple" "" ]
                    [ option []
                        [ text "1" ]
                    , option []
                        [ text "2" ]
                    , option []
                        [ text "3" ]
                    , option []
                        [ text "4" ]
                    , option []
                        [ text "5" ]
                    ]
                ]
            , div [ class "form-group" ]
                [ label [ for "exampleTextarea" ]
                    [ text "Example textarea" ]
                , textarea [ class "form-control", id "exampleTextarea", attribute "rows" "3" ]
                    []
                ]
            , div [ class "form-group" ]
                [ label [ for "exampleInputFile" ]
                    [ text "File input" ]
                , input [ attribute "aria-describedby" "fileHelp", class "form-control-file", id "exampleInputFile", type_ "file" ]
                    []
                , small [ class "form-text text-muted", id "fileHelp" ]
                    [ text "This is some placeholder block-level help text for the above input. It's a bit lighter and easily wraps to a new line." ]
                ]
            , fieldset [ class "form-group" ]
                [ legend []
                    [ text "Radio buttons" ]
                , div [ class "form-check" ]
                    [ label [ class "form-check-label" ]
                        [ input [ attribute "checked" "", class "form-check-input", id "optionsRadios1", name "optionsRadios", type_ "radio", value "option1" ]
                            []
                        , text "Option one is this and that&mdash;be sure to include why it's great      "
                        ]
                    ]
                , div [ class "form-check" ]
                    [ label [ class "form-check-label" ]
                        [ input [ class "form-check-input", id "optionsRadios2", name "optionsRadios", type_ "radio", value "option2" ]
                            []
                        , text "Option two can be something else and selecting it will deselect option one      "
                        ]
                    ]
                , div [ class "form-check disabled" ]
                    [ label [ class "form-check-label" ]
                        [ input [ class "form-check-input", attribute "disabled" "", id "optionsRadios3", name "optionsRadios", type_ "radio", value "option3" ]
                            []
                        , text "Option three is disabled      "
                        ]
                    ]
                ]
            , div [ class "form-check" ]
                [ label [ class "form-check-label" ]
                    [ input [ class "form-check-input", type_ "checkbox" ]
                        []
                    , text "Check me out    "
                    ]
                ]
            , button [ class "btn btn-primary", type_ "submit" ]
                [ text "Submit" ]
            ]
        ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
