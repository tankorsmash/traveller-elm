module StarPage exposing (Model, Msg(..), init, update, view)

import Element exposing (column, text)
import Element.Font as Font


type alias Model =
    {}


view model =
    column [ Font.color <| Element.rgb 0.5 1 0.5 ]
        [ text "Hello StarPage"
        , text "This will be rendering a star"
        ]


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )


update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


type Msg
    = NoOp
