module CanvasMap exposing (Msg(..), update, view)

import Bitwise exposing (and)
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
import Css exposing (calc)
import Dict as ElmDict
import Hexagons.Hex
import Hexagons.Layout
import Hexagons.Map
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



-- Convert pixel coordinates to axial coordinates for flat-topped hexes


pixelToAxial : Float -> Float -> Float -> ( Float, Float )
pixelToAxial size x y =
    let
        q =
            ((2 / 3) * x) / size

        r =
            ((-1 / 3) * x + (sqrt 3 / 3) * y) / size
    in
    ( q, r )



-- Convert axial coordinates to cube coordinates


axialToCube : ( Float, Float ) -> ( Float, Float, Float )
axialToCube ( q, r ) =
    let
        x =
            q

        z =
            r

        y =
            -x - z
    in
    ( x, y, z )



-- Round cube coordinates to the nearest integer cube coordinates


cubeRound : ( Float, Float, Float ) -> ( Int, Int, Int )
cubeRound ( x, y, z ) =
    let
        rx =
            round x

        ry =
            round y

        rz =
            round z

        xDiff =
            abs (toFloat rx - x)

        yDiff =
            abs (toFloat ry - y)

        zDiff =
            abs (toFloat rz - z)
    in
    if xDiff > yDiff && xDiff > zDiff then
        ( -ry - rz, ry, rz )

    else if yDiff > zDiff then
        ( rx, -rx - rz, rz )

    else
        ( rx, ry, -rx - ry )



-- Convert cube coordinates back to axial coordinates


cubeToAxial : ( Int, Int, Int ) -> ( Int, Int )
cubeToAxial ( x, y, z ) =
    ( x, z )



-- calc : Float -> Float -> Float -> { q : Int, r : Int, s : Int }
-- calc hexSize offsetX offsetY =
--     let
--         layout : Hexagons.Layout.Layout
--         layout =
--             { orientation = Hexagons.Layout.orientationLayoutFlat
--             , size = ( hexSize, hexSize )
--
--             -- , origin = ( 0, -hexSize / 2 )
--             , origin = ( 0, 0 )
--
--             -- , origin = ( -hexSize / 2 , hexSize / 2 )
--             }
--
--         -- map =
--         --     Hexagons.Map.rectangularFlatTopMap 40 30
--         hex =
--             Hexagons.Layout.pointToHex layout ( offsetX, offsetY )
--                 |> --ints
--                    (\h -> { q = Hexagons.Hex.intQ h, r = Hexagons.Hex.intR h, s = Hexagons.Hex.intS h })
--
--         -- |> -- floats
--         --     (\h -> { q = Hexagons.Hex.q h, r = Hexagons.Hex.r h, s = Hexagons.Hex.s h })
--     in
--     hex


{-| check if the mouse is over the canvas's specific hex
-}



-- update : Float -> Msg -> Maybe HexId


update : Float -> Msg -> ( Float, Float )
update hexSize msg =
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
                    (gridX * 2 / 3) / hexSize

                hexOffsetR =
                    ((-1 / 3) * gridX) + ((sqrt 3 / 3) * gridY)

                ( q, r ) =
                    pixelToAxial hexSize offsetX offsetY

                -- Convert axial to cube coordinates
                ( cubeX, cubeY, cubeZ ) =
                    axialToCube ( q, r )

                -- Round cube coordinates to nearest integers
                ( rx, ry, rz ) =
                    cubeRound ( cubeX, cubeY, cubeZ )

                -- Convert cube coordinates back to axial coordinates
                ( finalQ, finalR ) =
                    cubeToAxial ( rx, ry, rz )

                -- Update the hovered hex ID
                newHoveredHexId =
                    HexId.createFromTwoInts (finalQ + 1) (finalR + 1)

                -- sqrt3 = sqrt 3
                --
                -- q = ((sqrt3 / 3) * adjustedX - (1 / 3) * adjustedY) / hexSize
                -- r = (2 / 3 * adjustedY) / hexSize
                -- _ =
                --     ( round q, round r )
            in
            -- HexId.createFromTwoInts (round hexOffsetQ + 1) (round hexOffsetR + 1)
            -- HexId.createFromTwoInts (round q ) (round r)
            -- HexId.createFromTwoInts (floor <| hex.q + 1) (floor <| hex.r + 1)
            -- HexId.createFromTwoInts ( hex.q + 1) ( hex.r + 1)
            -- newHoveredHexId
            -- |> Just
            -- Nothing
            ( offsetX, offsetY )


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
renderHex { width, height, centerX, centerY, hexScale, xOffset, yOffset } hexOrigin index hexPoints_ maybeSolarSystem isHovered =
    let
        ( x, y ) =
            hexOrigin
                |> Tuple.mapBoth toFloat toFloat
                -- |> Tuple.mapBoth ((+) xOffset) ((+) yOffset)

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

        -- hex = (calc hexScale x y)
        layout : Hexagons.Layout.Layout
        layout =
            { orientation = Hexagons.Layout.orientationLayoutFlat
            , size = ( hexScale, hexScale )

            -- , origin = ( 0, -hexScale / 2 )
            -- , origin = ( 0, 0 )

            , origin = ( hexScale / 2, hexScale / 2 )
            }

        -- map =
        --     Hexagons.Map.rectangularFlatTopMap 40 30
        hex =
            -- Hexagons.Layout.offsetToHex ( 4, 0 )
            Hexagons.Layout.offsetToHex ( Tuple.first hexOrigin, Tuple.second hexOrigin )

        ( pointX, pointY ) =
            Hexagons.Layout.hexToPoint layout hex

        hexPoints =
            [ case Hexagons.Layout.polygonCorners layout hex of
                p1 :: ps ->
                    Canvas.path p1 (List.map Canvas.lineTo ps)

                _ ->
                    rect ( 0, 0 ) 10 10
            ]
    in
    -- Canvas.group [ transform [ translate 0 0 ] ]
    Canvas.group []
        -- Canvas.group [ transform [ translate x y ] ]
        [ shapes [ Canvas.Settings.stroke <| colorGrey, lineWidth 1.0, fill (Color.hsl hue 0.3 0.7) ]
            hexPoints
        , -- Hex ID display
          Canvas.text
            [ fill Color.red
            , Canvas.Settings.Text.align Center
            , Canvas.Settings.Text.baseLine Middle
            , transform [ translate pointX pointY ]
            ]
            ( 0, -hexScale * 0.65 )
            (case maybeSolarSystem of
                Just solarSystem ->
                    Hexagons.Layout.hexToOffset hex
                        |> Debug.toString
                        |> (\s -> s ++ "\n" ++ solarSystem.coordinates.raw)
                    -- solarSystem.coordinates.raw

                Nothing ->
                    Hexagons.Layout.hexToOffset hex
                        |> Debug.toString
            )
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


view : { screenVp : Browser.Dom.Viewport } -> ( SectorData, ElmDict.Dict Int SolarSystem ) -> Float -> ( Float, Float ) -> ( Float, Float ) -> Maybe HexId -> Html.Html Msg
view { screenVp } ( sectorData, solarSystemDict ) hexScale ( horizOffset, vertOffset ) mousePos hoveredHexId =
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
                    isHovered =
                        case hoveredHexId of
                            Just hid ->
                                hexIdx == hid.value

                            Nothing ->
                                False

                    axialToCube2 : ( Int, Int ) -> ( Int, Int, Int )
                    axialToCube2 ( q, r ) =
                        ( q, -q - r, r )

                    -- /* 
                    -- function cube_to_oddq(cube):
                    --     col = cube.x
                    --     row = cube.z + (cube.x - (cube.x&1)) / 2
                    --     return Hex(col, row)
                    -- */
                    -- hexIdx = axialToCube2 (  rowIdx, colIdx) |> calcHexIdx
                    -- hexIdx = axialToCube2 (  colIdx, rowIdx) |> calcHexIdx

                    hexIdx =
                        let 
                            s = (-(q - r))
                            r = rowIdx - (q + (q |> and 1)) // 2
                            q = colIdx

                        -- in (q, r)
                        in ( (q +  1) * 100 ) + (r +  1)
                        -- in ( (r +  1) * 100 ) + (q +  1)

                    -- calcHexIdx : (Float, Float, Float) -> (Int, Int)
                    calcHexIdx (x,y,z) =
                        let
                            col =
                                -- Hexagons.Hex.intQ hex
                                x

                            row =
                                -- Hexagons.Hex.intS hex + (Hexagons.Hex.intQ hex - (Hexagons.Hex.intQ hex |> and 1 ) ) // 2
                                z + (x - (x |> and 1)) // 2
                        in
                        -- ( (col + (numHexCols//2) + 1) * 100 ) + (row + (numHexRows//2)+ 1)
                        -- ( (row +  1) * 100 ) + (col +  1)
                        ( (col +  1) * 100 ) + (row +  1)

                        -- ( (col + (numHexCols//2) + 1) * 100 ) + (row + (numHexRows//2)+ 1)
                        -- ( (col + (numHexCols//2) + 1) ) + (row + (numHexRows//2)+ 1)* 100 
                        -- ( (col +  1) ) + (row +  1)* 100
                        -- (colIdx + 1) + (rowIdx + 1) * 100


                    -- hexIdx is the '0145' for the x 1, y 45 position
                    -- hexIdx =
                    --     (colIdx + 1) + (rowIdx + 1) * 100

                    -- hex = (calc hexScale x y)
                    layout : Hexagons.Layout.Layout
                    layout =
                        { orientation = Hexagons.Layout.orientationLayoutFlat
                        , size = ( hexScale, hexScale )

                        -- , origin = ( 0, -hexScale / 2 )
                        -- , origin = ( 0, 0 )

                        , origin = ( hexScale / 2, hexScale / 2 )
                        }

                    -- -- map =
                    -- --     Hexagons.Map.rectangularFlatTopMap 40 30
                    -- hex =
                    --     -- Hexagons.Layout.offsetToHex ( 4, 0 )
                    --     Hexagons.Layout.offsetToHex ( Tuple.first hexOrigin, Tuple.second hexOrigin )

                    ( pointX, pointY ) =
                        Hexagons.Layout.hexToPoint layout hex

                    hex = Hexagons.Layout.offsetToHex (colIdx, rowIdx)

                in
                Just <|
                    renderHex
                        renderConfig
                        hexOrigin
                        (Debug.log "hexidx" hexIdx)
                        hexPoints
                        (ElmDict.get hexIdx solarSystemDict)
                        -- hexIdx
                        isHovered

            viewHexRow rowIdx =
                List.range  (negate (floor <| numHexCols/2)) (floor <| numHexCols/2)
                    |> Array.fromList
                    -- |> Array.map (calcOrigin hexScale rowIdx)
                    |> Array.map (\col -> Tuple.pair rowIdx col)
                    |> Array.map (\(r, c)-> renderSingleHex r c (r,c))



            renderedHexes =
                List.range (negate (floor <| numHexRows/2)) (floor <| numHexRows/2)
                    |> Array.fromList
                    |> Array.map (Array.toList << viewHexRow)
                    |> Array.toList
                    |> List.concat
                    |> List.filterMap identity

            -- renderDebugMouse =
            --     let
            --         ( mouseX, mouseY ) =
            --             mousePos
            --
            --         hex =
            --             calc hexScale mouseX mouseY
            --     in
            --     Canvas.group []
            --         [ -- shapes [] [Canvas.rect ( mouseX, mouseY ) 200 200]
            --           Canvas.text [ Canvas.Settings.fill Color.red, Canvas.Settings.stroke Color.green, Canvas.Settings.Text.font { size = 48, family = "Arial" } ] ( mouseX, mouseY ) (String.fromInt hex.q ++ ", " ++ String.fromInt hex.r ++ ", " ++ String.fromInt hex.s)
            --         ]
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
             -- ++ [ renderDebugMouse ]
            )
        ]
