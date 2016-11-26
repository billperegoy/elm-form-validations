module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Forms
import Dict exposing (..)


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


fields : List ( String, Forms.FormValidator )
fields =
    [ ( "email", validateEmail )
    , ( "password", validatePassword )
    ]


init : ( Model, Cmd Msg )
init =
    { signupForm = Forms.initForm fields
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



-- Update


type Msg
    = UpdateEmailText String
    | UpdatePasswordText String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateEmailText text ->
            { model
                | signupForm = Forms.updateFormInput model.signupForm "email" text
            }
                ! []

        UpdatePasswordText text ->
            { model
                | signupForm = Forms.updateFormInput model.signupForm "password" text
            }
                ! []



-- View


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
                    [ text (Forms.lookupErrorValue model.signupForm "email" |> Forms.errorString) ]
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
                    [ text (Forms.lookupErrorValue model.signupForm "password" |> Forms.errorString) ]
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
