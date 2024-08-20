module CanvasMap exposing (view)

import Browser.Dom
import Canvas exposing (rect, shapes)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Advanced exposing (rotate, transform, translate)
import Canvas.Settings.Text exposing (TextAlign(..), TextBaseLine(..))
import Color
import Color.Convert exposing (hexToColor)
import Dict
import Html exposing (div)
import Html.Attributes exposing (style)
import Http exposing (Error(..))
import RemoteData exposing (RemoteData(..))
import Traveller.SectorData exposing (SectorData)
import Traveller.SolarSystem exposing (SolarSystem)
import Traveller.Star as Star exposing (Star, StarColour(..))


type alias RenderConfig =
    { width : Float
    , height : Float
    , centerX : Float
    , centerY : Float
    , hexScale : Float
    , xOffset : Float
    , yOffset : Float
    }


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


hexColOffset row =
    if remainderBy 2 row == 0 then
        0

    else
        1


calcOrigin : Float -> Int -> Int -> ( Int, Int )
calcOrigin hexSize row col =
    let
        a =
            2 * pi / 6

        x =
            hexSize + toFloat col * (hexSize + hexSize * cos a)

        y =
            hexSize + toFloat row * 2 * hexSize * sin a + hexSize * hexColOffset col * sin a
    in
    ( floor x, floor y )


starColourRGB : Star.StarColour -> Color.Color
starColourRGB colour =
    let
        colorString : String
        colorString =
            case colour of
                Blue ->
                    "#000077"

                BlueWhite ->
                    "#87cefa"

                White ->
                    "#FFFFFF"

                YellowWhite ->
                    "#ffffe0"

                Yellow ->
                    "#ffff00"

                LightOrange ->
                    "#ffbf00"

                OrangeRed ->
                    "#ff4500"

                Red ->
                    "#ff0000"

                Brown ->
                    "#f4a460"

                DeepDimRed ->
                    "#800000"
    in
    forceHexToColor colorString


{-| returns mageenta if the hex string is invalid

if we are worried about invalid colors, we can use `hexToColor` directly

-}
forceHexToColor : String -> Color.Color
forceHexToColor colorString =
    hexToColor colorString |> Result.withDefault (Color.rgb 255 0 255)


drawStar : ( Float, Float ) -> Float -> { a | colour : Maybe StarColour } -> Canvas.Renderable
drawStar ( starX, starY ) radius star =
    shapes
        [ fill <|
            case star.colour of
                Just starColor ->
                    starColourRGB starColor

                Nothing ->
                    Color.rgb 255 0 255
        ]
        [ Canvas.circle ( starX, starY ) radius ]



-- [ Canvas.circle ( 0, 0 ) radius ]


renderHex : RenderConfig -> ( Int, Int ) -> Int -> Int -> Maybe SolarSystem -> Canvas.Renderable
renderHex { width, height, centerX, centerY, hexScale, xOffset, yOffset } hexOrigin hexIndex index maybeSolarSystem =
    let
        ( x, y ) =
            hexOrigin
                |> Tuple.mapBoth toFloat toFloat
                |> Tuple.mapBoth ((+) xOffset) ((+) yOffset)

        size =
            hexScale

        hue =
            toFloat (toFloat index / 2 |> floor |> modBy 100)
                / 100
                |> (\h ->
                        if hasStar then
                            255

                        else
                            h
                   )

        hasStar =
            case maybeSolarSystem of
                Just _ ->
                    True

                Nothing ->
                    False
    in
    Canvas.group [ transform [ translate x y ] ]
        [ shapes [ fill (Color.hsl hue 0.3 0.7) ]
            [ case hexagonPoints ( 0, 0 ) (size * 1) of
                p1 :: ps ->
                    Canvas.path p1 (List.map Canvas.lineTo ps)

                _ ->
                    rect ( 0, 0 ) 10 10
            ]
        , case maybeSolarSystem of
            Just solarSystem ->
                case solarSystem.stars of
                    [] ->
                        shapes [] []

                    primaryStar :: stars ->
                        let
                            primaryPos =
                                ( x / 100, y / 100 )
                        in
                        Canvas.group
                            []
                            ((case primaryStar.companion of
                                Just companionStar ->
                                    let
                                        companionStarPos =
                                            primaryPos
                                                |> Tuple.mapFirst (\x_ -> x_ - 5)
                                    in
                                    Canvas.group []
                                        [ drawStar primaryPos 5 primaryStar
                                        , drawStar companionStarPos 2 companionStar
                                        ]

                                Nothing ->
                                    drawStar primaryPos 5 primaryStar
                             )
                                :: List.indexedMap
                                    (\_ secondaryStar ->
                                        let
                                            secondaryStarPos =
                                                -- rotatePoint idx primaryPos 60 20
                                                ( 10, 10 )
                                        in
                                        case secondaryStar.companion of
                                            Just companionStar ->
                                                let
                                                    companionStarPos =
                                                        secondaryStarPos |> Tuple.mapFirst (\x_ -> x_ - 5)
                                                in
                                                Canvas.group []
                                                    [ drawStar secondaryStarPos 7 secondaryStar
                                                    , drawStar companionStarPos 3 companionStar
                                                    ]

                                            Nothing ->
                                                drawStar secondaryStarPos 7 secondaryStar
                                    )
                                    stars
                            )

            Nothing ->
                shapes [] []
        , -- Hex ID display
          Canvas.text
            [ fill Color.red
            , Canvas.Settings.Text.align Center
            , Canvas.Settings.Text.baseLine Middle
            ]
            ( 0, -hexScale * 0.65 )
            (case maybeSolarSystem of
                Just solarSystem ->
                    solarSystem.coordinates.raw

                Nothing ->
                    ""
            )
        , -- giants/planets/belts display
          case maybeSolarSystem of
            Just solarSystem ->
                Canvas.group
                    [ Canvas.Settings.Text.align Center
                    , Canvas.Settings.Text.baseLine Middle
                    , Canvas.Settings.Text.font { size = 14, family = "Arial" }
                    ]
                    [ Canvas.text
                        [ fill <| forceHexToColor "#109076" ]
                        ( hexScale * -0.3, hexScale * 0.65 )
                        (solarSystem.gasGiants |> String.fromInt)
                    , Canvas.text
                        [ fill <| forceHexToColor "#809076" ]
                        ( 0, hexScale * 0.65 )
                        (solarSystem.terrestrialPlanets |> String.fromInt)
                    , Canvas.text
                        [ fill <| forceHexToColor "#68B976" ]
                        ( hexScale * 0.3, hexScale * 0.65 )
                        (solarSystem.planetoidBelts |> String.fromInt)
                    ]

            Nothing ->
                Canvas.group [] []
        ]


hexagonPoints : ( Float, Float ) -> Float -> List ( Float, Float )
hexagonPoints ( xOrigin, yOrigin ) size =
    let
        a =
            2 * pi / 6

        -- angle deg =
        --     (deg + 90) * pi / 180
        x n =
            xOrigin
                + (size * cos (a * n))

        y n =
            yOrigin
                + (size * sin (a * n))

        buildPoint n =
            ( x n, y n )
    in
    List.range 0 5
        |> List.map (toFloat >> buildPoint)


view : { screenVp : Browser.Dom.Viewport } -> ( SectorData, Dict.Dict Int SolarSystem ) -> Float -> ( Float, Float ) -> Html.Html msg
view { screenVp } ( sectorData, solarSystemDict ) hexScale ( horizOffset, vertOffset ) =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ let
            ( centerX, centerY ) =
                ( canvasWidthish / 2, canvasHeightish / 2 )

            clearScreen =
                shapes [ fill Color.white ] [ rect ( 0, 0 ) canvasWidthish canvasHeightish ]

            renderConfig =
                { width = canvasWidthish
                , height = canvasHeightish
                , centerX = centerX
                , centerY = centerY
                , hexScale = hexScale
                , xOffset = xOffset
                , yOffset = yOffset
                }

            canvasWidthish =
                min (screenVp.viewport.width * 0.9)
                    (screenVp.viewport.width - 500.0)

            canvasHeightish =
                screenVp.viewport.height * 0.9

            xOffset =
                -- view horizontal offset
                (canvasWidthish - (numHexCols * hexScale * 1.6)) * horizOffset

            yOffset =
                -- view vertical offset
                (canvasHeightish - (numHexRows * hexScale * 1.6)) * vertOffset

            -- viewHexRow : Int -> List ( Maybe (Svg Msg), Int )
            viewHexRow rowIdx =
                List.range 0 numHexCols
                    |> List.map (calcOrigin hexScale rowIdx)
                    |> List.indexedMap
                        (\colIdx (( ox, oy ) as hexOrigin) ->
                            let
                                -- hexIdx is the '0145' for the x 1, y 45 position
                                hexIdx =
                                    (rowIdx + 1) + (colIdx + 1) * 100

                                index =
                                    rowIdx * numHexCols + colIdx

                                ( fox, foy ) =
                                    ( toFloat ox, toFloat oy )

                                widestViewport =
                                    screenVp

                                outsideX =
                                    let
                                        plus =
                                            fox + hexScale - horizOffset

                                        minus =
                                            fox - hexScale - horizOffset
                                    in
                                    (plus < 0) || (minus > widestViewport.viewport.width)

                                outsideY =
                                    let
                                        plus =
                                            foy + hexScale - (canvasHeightish * vertOffset)

                                        minus =
                                            foy - hexScale - (canvasHeightish * vertOffset)
                                    in
                                    (plus < 0) || (minus > widestViewport.viewport.height)
                            in
                            if outsideX || outsideY then
                                Nothing

                            else
                                Just <|
                                    renderHex renderConfig hexOrigin hexIdx index (Dict.get hexIdx solarSystemDict)
                        )

            renderedHexes =
                List.range 0 numHexRows
                    |> List.map viewHexRow
                    |> List.concat
                    |> List.filterMap identity
          in
          Canvas.toHtml
            ( floor canvasWidthish, floor canvasHeightish )
            [ style "border" "10px solid rgba(0,0,0,0.1)" ]
            ([ clearScreen
             , render renderConfig 106
             ]
                ++ renderedHexes
            )
        ]
