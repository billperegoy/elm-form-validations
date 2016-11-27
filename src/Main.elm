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


signupFormFields : List ( String, Forms.FormValidator )
signupFormFields =
    [ ( "email", validateEmail )
    , ( "password", validatePassword )
    , ( "age", validateAge )
    ]


init : ( Model, Cmd Msg )
init =
    { signupForm = Forms.initForm signupFormFields
    }
        ! []



-- Field Validators


validateEmail : String -> Forms.ValidationErrors
validateEmail string =
    let
        emailRegex =
            "^\\w+@\\w+\\.\\w+$"
    in
        Forms.runPrimitiveValidations
            string
            [ Forms.validateExistence, Forms.validateRegex emailRegex ]


validatePassword : String -> Forms.ValidationErrors
validatePassword string =
    Forms.runPrimitiveValidations
        string
        [ Forms.validateExistence, Forms.validateLength 10 ]


validateAge : String -> Forms.ValidationErrors
validateAge string =
    Forms.runPrimitiveValidations
        string
        [ Forms.validateExistence, Forms.validateNumericality ]



-- Update


type Msg
    = UpdateEmailText String
    | UpdatePasswordText String
    | UpdateAgeText String


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



-- View


submitButtonAttributes : Bool -> List (Html.Attribute Msg)
submitButtonAttributes validateStatus =
    if validateStatus then
        [ class "btn btn-primary", type_ "submit" ]
    else
        [ class "btn", type_ "submit" ]


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
            [ text (Forms.errors form "email") ]
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
            [ text (Forms.errors form "password") ]
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
            [ text (Forms.errors form "age") ]
        ]


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
            , signupFormSubmitButton model.signupForm
            ]
        ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
