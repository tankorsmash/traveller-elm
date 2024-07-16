module CanvasMap exposing (view)

import Canvas exposing (rect, shapes)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Advanced exposing (rotate, transform, translate)
import Color
import Html exposing (div)
import Html.Attributes exposing (style)
import Http exposing (Error(..))
import RemoteData exposing (RemoteData(..))


view : Html.Html msg
view =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ let
            width =
                800

            height =
                400

            ( centerX, centerY ) =
                ( width / 2, height / 2 )

            clearScreen =
                shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]

            render count =
                let
                    size =
                        width / 3

                    x =
                        -(size / 2)

                    y =
                        -(size / 2)

                    rotation =
                        degrees (count * 3)

                    hue =
                        toFloat (count / 4 |> floor |> modBy 100) / 100
                in
                shapes
                    [ transform
                        [ translate centerX centerY
                        , rotate rotation
                        ]
                    , fill (Color.hsl hue 0.3 0.7)
                    ]
                    [ rect ( x, y ) size size ]
          in
          Canvas.toHtml
            ( width, height )
            [ style "border" "10px solid rgba(0,0,0,0.1)" ]
            [ clearScreen
            , render 106
            ]
        ]
