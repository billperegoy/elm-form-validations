# Elm Form Validation library

## Installation
To install the library, use the normal approach with elm-package.

```
elm-package install billperegoy/elm-form-validations
```

## Model and Init
The form data is represented by a single sub-model instantiated within your user’s model. For instance.

```
import Forms
type alias model =
    { loginForm : Forms.Form
    }
```

You can (and will) of course add other data to your model, but in order to acquire and validate form data, you just need this one entry.
In order to define what fields are available in your form, you need to define an init function that describes this.

```
init =
    { loginForm = Form.initForm loginFormFields } ! []
```

The loginFormFields function is where all the field definition occurs. 

```
loginFormFields : List ( String, List Forms.FieldValidator )
loginFormFields =
    [ ("name", nameValidations
    , ( "email", emailValidations )
    , ( "password", passwordValidations )
    ]
```

This function returns a list of tuples with each tuple representing the field name and a list of validations for that field. The field validation lists are contained in separate functions. Here is an example of one of them.

```
passwordValidations : List Forms.FieldValidator
passwordValidations =
    [ Forms.validateExistence
    , Forms.validateMinLength 10
    , Forms.validateMaxLength 15
    ]
```

This example adds three validations to the password field. Each of the validation functions is a partially applied function defined in the library.
Upon initialization, all data structures to support the input and validation of the defined fields is set up. While under the hood, an Elm Dict is used to hold this data, there is no need to know those details. You can just use the supplied functions to update and query this structure.
## Update Function
You only need to add one message type and update action to update and access any form field defined in the init function.

```
type Msg
    = UpdateFormText String String
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateFormText name val ->
            { model
                | signupForm =
                    Forms.updateFormInput model.signupForm name val
            }
                ! []
```

That’s really all there is to it. You define this single message type and update case and the library handles the rest. The updateFormContent function does the work of storing the value and running validations. All that’s left now is to define our view so that we can trigger updates and display the form values and validate status.

## View Function

```
view : Model -> Html Msg
view model =
    div []
        [ Html.form []
            [ input
                  [ placeholder "Enter email"
                  , onInput (UpdateFormText "email")
                  ]
                  []
            , small []
                  [ text (Forms.errorString form "email") ]
            ]
        ]
```

## Full Example
For a full working example of this librray in use, please visit.

[https://github.com/billperegoy/elm-forms-example](https://github.com/billperegoy/elm-forms-example)


