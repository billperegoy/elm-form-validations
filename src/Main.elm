module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Forms


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
    { signupForm : Forms.Form
    }


signupFormFields : List ( String, List Forms.FieldValidator )
signupFormFields =
    [ ( "email", emailValidations )
    , ( "password", passwordValidations )
    , ( "age", ageValidations )
    , ( "stooge", stoogeValidations )
    ]


init : ( Model, Cmd Msg )
init =
    { signupForm = Forms.initForm signupFormFields
    }
        ! []



-- Field Validators


emailValidations : List Forms.FieldValidator
emailValidations =
    let
        emailRegex =
            "^\\w+@\\w+\\.\\w+$"
    in
        [ Forms.validateExistence
        , Forms.validateRegex emailRegex
        ]


passwordValidations : List Forms.FieldValidator
passwordValidations =
    [ Forms.validateExistence
    , Forms.validateMinLength 10
    , Forms.validateMaxLength 15
    ]


ageValidations : List Forms.FieldValidator
ageValidations =
    [ Forms.validateExistence
    , Forms.validateNumericality
    , Forms.validateNumericRange 21 88
    ]


stoogeValidations : List Forms.FieldValidator
stoogeValidations =
    [ Forms.validateExistence
    , Forms.validateIsOneOf [ "larry", "curly", "moe" ]
    ]



-- Update


type Msg
    = UpdateEmailText String
    | UpdatePasswordText String
    | UpdateAgeText String
    | UpdateStoogeText String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateEmailText text ->
            { model
                | signupForm =
                    Forms.updateFormInput model.signupForm "email" text
            }
                ! []

        UpdatePasswordText text ->
            { model
                | signupForm =
                    Forms.updateFormInput model.signupForm "password" text
            }
                ! []

        UpdateAgeText text ->
            { model
                | signupForm =
                    Forms.updateFormInput model.signupForm "age" text
            }
                ! []

        UpdateStoogeText text ->
            { model
                | signupForm =
                    Forms.updateFormInput model.signupForm "stooge" text
            }
                ! []



-- View


emailFormElement : Forms.Form -> Html Msg
emailFormElement form =
    div [ class "form-group" ]
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
            [ text (Forms.errorString form "email") ]
        ]


passwordFormElement : Forms.Form -> Html Msg
passwordFormElement form =
    div [ class "form-group" ]
        [ label [ for "exampleInputPassword" ] [ text "Password" ]
        , input
            [ class "form-control"
            , id "exampleInputPassword"
            , placeholder "Enter password"
            , type_ "password"
            , onInput UpdatePasswordText
            ]
            []
        , small [ class "form-text text-muted" ]
            [ text (Forms.errorString form "password") ]
        ]


ageFormElement : Forms.Form -> Html Msg
ageFormElement form =
    div [ class "form-group" ]
        [ label [ for "exampleInputAge" ] [ text "Age" ]
        , input
            [ class "form-control"
            , id "exampleInputAge"
            , placeholder "Age"
            , onInput UpdateAgeText
            ]
            []
        , small [ class "form-text text-muted" ]
            [ text (Forms.errorString form "age") ]
        ]


stoogeFormElement : Forms.Form -> Html Msg
stoogeFormElement form =
    div [ class "form-group" ]
        [ label [ for "exampleInputSge" ] [ text "Stooge" ]
        , input
            [ class "form-control"
            , id "exampleInputStooge"
            , placeholder "Stooge"
            , onInput UpdateStoogeText
            ]
            []
        , small [ class "form-text text-muted" ]
            [ text (Forms.errorString form "stooge") ]
        ]


submitButtonAttributes : Bool -> List (Html.Attribute Msg)
submitButtonAttributes validateStatus =
    if validateStatus then
        [ class "btn btn-primary", type_ "submit" ]
    else
        [ class "btn", type_ "submit" ]


signupFormSubmitButton : Forms.Form -> Html Msg
signupFormSubmitButton form =
    button
        (submitButtonAttributes (Forms.validateStatus form))
        [ text "Submit" ]


view : Model -> Html Msg
view model =
    div [ class "container", style [ ( "width", "300px" ) ] ]
        [ Html.form []
            [ emailFormElement model.signupForm
            , passwordFormElement model.signupForm
            , ageFormElement model.signupForm
            , stoogeFormElement model.signupForm
            , signupFormSubmitButton model.signupForm
            ]
        ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
