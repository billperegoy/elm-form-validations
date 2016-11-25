module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import FormValidation exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { signupForm : SignupForm
    }


type alias SignupForm =
    { email : FormElement
    , password : FormElement
    , validateStatus : Bool
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


signupFormWithNewEmail : Model -> String -> SignupForm
signupFormWithNewEmail model text =
    let
        signupForm : SignupForm
        signupForm =
            model.signupForm

        newFormElement : FormElement
        newFormElement =
            updateFormElement signupForm.email text
    in
        { signupForm
            | email = newFormElement
            , validateStatus =
                validateForm { signupForm | email = newFormElement }
        }


signupFormWithNewPassword : Model -> String -> SignupForm
signupFormWithNewPassword model text =
    let
        signupForm : SignupForm
        signupForm =
            model.signupForm

        newFormElement : FormElement
        newFormElement =
            updateFormElement signupForm.password text
    in
        { signupForm
            | password = newFormElement
            , validateStatus =
                validateForm { signupForm | password = newFormElement }
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateEmailText text ->
            { model | signupForm = signupFormWithNewEmail model text } ! []

        UpdatePasswordText text ->
            { model | signupForm = signupFormWithNewPassword model text } ! []



-- Form Validator


validateForm : SignupForm -> Bool
validateForm form =
    runFormValidations form
        [ ( validateEmail, (\formElement -> formElement.email.input) )
        , ( validatePassword, (\formElement -> formElement.password.input) )
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



-- View


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


submitButtonAttributes : Bool -> List (Html.Attribute Msg)
submitButtonAttributes validateStatus =
    if validateStatus then
        [ class "btn btn-primary", type_ "submit" ]
    else
        [ class "btn", type_ "submit" ]


view : Model -> Html Msg
view model =
    div [ class "container", style [ ( "width", "300px" ) ] ]
        [ Html.form []
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
            , button
                (submitButtonAttributes model.signupForm.validateStatus)
                [ text "Submit" ]
            ]
        ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
