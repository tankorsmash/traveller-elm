module CanvasMap exposing (Msg(..), update, view)

import Array exposing (Array)
import AssocList as Dict exposing (Dict)
import Browser.Dom
import Canvas exposing (path, rect, shapes)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Advanced exposing (rotate, transform, translate)
import Canvas.Settings.Line exposing (LineJoin, lineWidth)
import Canvas.Settings.Text exposing (TextAlign(..), TextBaseLine(..))
import Color
import Color.Convert exposing (hexToColor)
import Dict as ElmDict
import Html exposing (div)
import Html.Attributes exposing (style)
import Html.Events.Extra.Mouse
import Http exposing (Error(..))
import RemoteData exposing (RemoteData(..))
import Traveller.HexId as HexId exposing (HexId)
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



-- defaultHexBg =
--     "#f5f5f5"
-- defaultHexSize =
--     40


numHexCols =
    32 * 1


numHexRows =
    40 * 1



-- numHexes =
--     numHexCols * numHexRows
--
--
-- hexWidth : Int
-- hexWidth =
--     100
--
--
-- hexHeight : Int
-- hexHeight =
--     floor <| toFloat hexWidth * 1.2


type Msg
    = MouseMovedOnCanvas Html.Events.Extra.Mouse.Event


{-| check if the mouse is over the canvas's specific hex
-}
update : Msg -> Maybe HexId
update msg =
    case msg of
        MouseMovedOnCanvas event ->
            let
                ( offsetX, offsetY ) =
                    event.offsetPos

                hexWidth =
                    hexSize * 1.6

                hexHeight =
                    hexSize * 1.6

                gridX =
                    offsetX / hexWidth

                gridY =
                    offsetY / hexHeight

                hexOffsetQ =
                    gridX * 2 / 3

                hexOffsetR =
                    (-gridX / 3) + (sqrt 3 / 3) * gridY

                _ =
                    ( round hexOffsetQ, round hexOffsetR )

                hexSize =
                    -- TODO: replace with actual size from Traveller.Model
                    40
            in
            HexId.createFromTwoInts (round hexOffsetR + 1) (round hexOffsetQ + 1)
                |> Just


render : RenderConfig -> Float -> Canvas.Renderable
render { width, height, centerX, centerY } bgRotateAndHue =
    let
        size =
            width / 3

        x =
            -(size / 2)

        y =
            -(size / 2)

        rotation =
            degrees (bgRotateAndHue * 3)

        hue =
            toFloat (bgRotateAndHue / 4 |> floor |> modBy 100) / 100
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


starColourDict : Dict Star.StarColour Color.Color
starColourDict =
    [ ( Star.Blue, "#000077" )
    , ( Star.BlueWhite, "#87cefa" )
    , ( Star.White, "#FFFFFF" )
    , ( Star.YellowWhite, "#ffffe0" )
    , ( Star.Yellow, "#ffff00" )
    , ( Star.LightOrange, "#ffbf00" )
    , ( Star.OrangeRed, "#ff4500" )
    , ( Star.Red, "#ff0000" )
    , ( Star.Brown, "#f4a460" )
    , ( Star.DeepDimRed, "#800000" )
    ]
        |> List.map (\( k, v ) -> ( k, forceHexToColor v ))
        |> Dict.fromList


starColourRGB : Star.StarColour -> Color.Color
starColourRGB colour =
    Dict.get colour starColourDict
        |> Maybe.withDefault (Color.rgb 255 0 255)


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


gasGiantColor =
    forceHexToColor "#109076"


terrestialPlanetColor =
    forceHexToColor "#809076"


planetoidBeltColor =
    forceHexToColor "#68B976"


colorGrey =
    forceHexToColor "#CCCCCC"


renderHex : RenderConfig -> ( Int, Int ) -> Int -> List Canvas.Shape -> Maybe SolarSystem -> Bool -> Canvas.Renderable
renderHex { width, height, centerX, centerY, hexScale, xOffset, yOffset } hexOrigin index hexPoints maybeSolarSystem isHovered =
    let
        ( x, y ) =
            hexOrigin
                |> Tuple.mapBoth toFloat toFloat
                |> Tuple.mapBoth ((+) xOffset) ((+) yOffset)

        hue =
            toFloat (toFloat index / 2 |> floor |> modBy 100)
                / 100
                |> (\h ->
                        if isHovered then
                            0

                        else if hasStar then
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
        [ shapes [ Canvas.Settings.stroke <| colorGrey, lineWidth 1.0, fill (Color.hsl hue 0.3 0.7) ]
            hexPoints
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
                        [ fill <| gasGiantColor ]
                        ( hexScale * -0.3, hexScale * 0.65 )
                        (solarSystem.gasGiants |> String.fromInt)
                    , Canvas.text
                        [ fill <| terrestialPlanetColor ]
                        ( 0, hexScale * 0.65 )
                        (solarSystem.terrestrialPlanets |> String.fromInt)
                    , Canvas.text
                        [ fill <| planetoidBeltColor ]
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


view : { screenVp : Browser.Dom.Viewport } -> ( SectorData, ElmDict.Dict Int SolarSystem ) -> Float -> ( Float, Float ) -> Maybe HexId -> Html.Html Msg
view { screenVp } ( sectorData, solarSystemDict ) hexScale ( horizOffset, vertOffset ) hoveredHexId =
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

            hexPoints =
                [ case hexagonPoints ( 0, 0 ) (renderConfig.hexScale * 1) of
                    p1 :: ps ->
                        Canvas.path p1 (List.map Canvas.lineTo ps)

                    _ ->
                        rect ( 0, 0 ) 10 10
                ]

            renderSingleHex rowIdx colIdx (( ox, oy ) as hexOrigin) =
                let
                    ( fox, foy ) =
                        ( toFloat ox + xOffset, toFloat oy + yOffset )

                    outsideX =
                        (fox + hexScale < 0) || (fox - hexScale > canvasWidthish)

                    outsideY =
                        (foy + hexScale < 0) || (foy - hexScale > canvasHeightish)

                    isOutOfView =
                        outsideX || outsideY
                in
                if isOutOfView then
                    Nothing

                else
                    let
                        isHovered =
                            case hoveredHexId of
                                Just hid ->
                                    (colIdx + rowIdx * 100) == hid.value

                                Nothing ->
                                    False

                        -- hexIdx is the '0145' for the x 1, y 45 position
                        hexIdx =
                            (rowIdx + 1) + (colIdx + 1) * 100
                    in
                    Just <|
                        renderHex
                            renderConfig
                            hexOrigin
                            hexIdx
                            hexPoints
                            (ElmDict.get hexIdx solarSystemDict)
                            isHovered

            viewHexRow rowIdx =
                List.range 0 numHexCols
                    |> Array.fromList
                    |> Array.map (calcOrigin hexScale rowIdx)
                    |> Array.indexedMap (renderSingleHex rowIdx)

            renderedHexes =
                List.range 0 numHexRows
                    |> Array.fromList
                    |> Array.map (Array.toList << viewHexRow)
                    |> Array.toList
                    |> List.concat
                    |> List.filterMap identity
          in
          Canvas.toHtml
            ( floor canvasWidthish, floor canvasHeightish )
            [ style "border" "10px solid rgba(0,0,0,0.1)"
            , Html.Events.Extra.Mouse.onMove MouseMovedOnCanvas
            ]
            ([ clearScreen
             , render renderConfig 105
             ]
                ++ renderedHexes
            )
        ]
