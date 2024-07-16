module CanvasMap exposing (view)

import Canvas exposing (rect, shapes)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Advanced exposing (rotate, transform, translate)
import Color
import Dict
import Html exposing (div)
import Html.Attributes exposing (style)
import Http exposing (Error(..))
import RemoteData exposing (RemoteData(..))
import Traveller.SectorData exposing (SectorData)
import Traveller.SolarSystem exposing (SolarSystem)


type alias RenderConfig =
    { width : Float, height : Float, centerX : Float, centerY : Float }


defaultHexBg =
    "#f5f5f5"


defaultHexSize =
    40


numHexCols =
    32 * 1


numHexRows =
    40 * 1


numHexes =
    numHexCols * numHexRows


hexWidth : Int
hexWidth =
    100


hexHeight : Int
hexHeight =
    floor <| toFloat hexWidth * 1.2


render : RenderConfig -> Float -> Canvas.Renderable
render { width, height, centerX, centerY } count =
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


indexToCoords : Int -> ( Int, Int )
indexToCoords index =
    let
        row =
            index // numHexCols

        col =
            modBy numHexCols index
    in
    ( row, col )


renderHex : RenderConfig -> Int -> SolarSystem -> Canvas.Renderable
renderHex { width, height, centerX, centerY } index solarSystem =
    let
        iSize =
            floor size

        ( row, col ) =
            indexToCoords index

        fIndex =
            toFloat index

        x =
            centerX + toFloat col * 10

        y =
            centerY + toFloat row * 20

        size =
            10

        halfSize =
            size / 2

        rotation =
            degrees (toFloat index * 10)

        hue =
            toFloat (toFloat index / 2 |> floor |> modBy 100) / 100
    in
    shapes
        [ transform
            [ translate (x + halfSize) (y + halfSize)
            , rotate rotation
            ]

        -- , fill (Color.rgb 255 0 0)
        , fill (Color.hsl hue 0.3 0.7)
        ]
        [ rect ( -halfSize, -halfSize ) size size ]


view : ( SectorData, Dict.Dict Int SolarSystem ) -> Html.Html msg
view ( sectorData, solarSystemDict ) =
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

            renderConfig =
                { width = width, height = height, centerX = centerX, centerY = centerY }
          in
          Canvas.toHtml
            ( width, height )
            [ style "border" "10px solid rgba(0,0,0,0.1)" ]
            ([ clearScreen
             , render renderConfig 106
             ]
                ++ List.indexedMap
                    (\index solarSystem ->
                        renderHex renderConfig index solarSystem
                    )
                    (Dict.values solarSystemDict)
            )
        ]
