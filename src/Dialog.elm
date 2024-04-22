port module Dialog exposing (toggleDialog, view)

import Html exposing (Html, text)
import Html.Attributes
import Html.Events


port toggleDialog : String -> Cmd msg


dialogNode : String -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
dialogNode elementId attr content =
    Html.node "dialog" (Html.Attributes.id elementId :: attr) content


view : String -> msg -> Html msg -> Html msg
view elementId closeMsg content =
    dialogNode
        elementId
        [-- any bootstrap classes in here seem to force visibliy and breaks HTML Dialog
        ]
        [ content
        , Html.button
            [ Html.Attributes.class "btn btn-primary m-2"
            , Html.Events.onClick closeMsg
            ]
            [ text "Close" ]
        ]
