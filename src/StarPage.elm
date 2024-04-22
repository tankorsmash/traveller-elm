module StarPage exposing (Model, Msg(..), init, update, view)

import Element exposing (column, text)
import Element.Font as Font
import Svg.Styled as Svg
import Svg.Styled.Attributes as SvgAttrs


type alias Model =
    {}


view model =
    let
        ( width, stringWidth ) =
            500 |> (\x -> ( x, String.fromFloat x ))

        ( height, stringHeight ) =
            500 |> (\x -> ( x, String.fromFloat x ))

        ( horizOffset, vertOffset ) =
            ( 0.0, 0.0 )

        xOffset =
            -- view horizontal offset
            String.fromFloat (width * horizOffset)

        yOffset =
            -- view vertical offset
            String.fromFloat (height * vertOffset)

        drawStar ( starX, starY ) radius =
            Svg.circle
                [ SvgAttrs.cx <| String.fromFloat <| starX
                , SvgAttrs.cy <| String.fromFloat <| starY
                , SvgAttrs.r <| String.fromFloat <| radius
                , SvgAttrs.fill <| "red"
                ]
                []
    in
    column [ Font.color <| Element.rgb 0.5 1 0.5 ]
        [ text "Hello StarPage"
        , text "This will be rendering a star"
        , Element.html <|
            Svg.toUnstyled <|
                Svg.svg
                    [ SvgAttrs.width <| stringWidth
                    , SvgAttrs.height <| stringHeight
                    , SvgAttrs.id "start_svg"
                    , SvgAttrs.style "border: 1px solid black; background-color: #f0f0f0;"
                    , SvgAttrs.viewBox <|
                        xOffset
                            ++ " "
                            ++ yOffset
                            ++ " "
                            ++ stringWidth
                            ++ " "
                            ++ stringHeight
                    ]
                    [ drawStar ( 250, 250 ) 20
                    ]
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
