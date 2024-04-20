module Traveller exposing (Model, Msg(..), init, update, view)

-- import Svg exposing (..)

import Codec
import Css exposing (hover)
import Css.Transitions exposing (offset)
import Dict
import Element
    exposing
        ( Element
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , px
        , rgb
        , row
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes
import Html.Styled.Events
import Http
import Json.Decode as JsDecode
import RemoteData exposing (RemoteData)
import Svg.Styled as Svg exposing (Svg)
import Svg.Styled.Attributes as SvgAttrs exposing (fill, points, viewBox)
import Svg.Styled.Events as SvgEvents
import Svg.Styled.Lazy
import Traveller.HexId as HexId exposing (HexId)
import Traveller.SectorData exposing (SectorData, SectorSolarSystem, codecSectorData)


{-| needs to be a plain int for hashing in a Dict, otherwise we'd use full HexIds
-}
type alias RawHexId =
    Int


type alias Model =
    { hexScale : Float
    , sectorData : RemoteData Http.Error ( SectorData, Dict.Dict RawHexId SectorSolarSystem )
    , offset : ( Float, Float )
    , playerHex : HexId
    , hoveringHex : Maybe HexId
    }


type OffsetDirection
    = Horizontal
    | Vertical


type Msg
    = NoOpMsg
    | ZoomScaleChanged Float
    | DownloadSectorJson
    | DownloadedSectorJson (Result Http.Error SectorData)
    | OffsetChanged OffsetDirection Float
    | HoveringHex HexId


type alias HexOrigin =
    ( Int, Int )


init : ( Model, Cmd Msg )
init =
    ( { hexScale = 35
      , sectorData = RemoteData.NotAsked
      , offset = ( 0.0, 0.0 )
      , playerHex = HexId.createFromInt 135
      , hoveringHex = Nothing
      }
    , sendSectorRequest
    )


hoverableStyle =
    Css.batch
        [ Css.hover
            [ Css.fill <| Css.hex "aaaaaa" ]
        ]


hexagonPoints : ( Int, Int ) -> Float -> String
hexagonPoints ( xOrigin, yOrigin ) size =
    let
        a =
            2 * pi / 6

        -- angle deg =
        --     (deg + 90) * pi / 180
        x n =
            toFloat xOrigin
                + (size * cos (a * n))
                |> String.fromFloat

        y n =
            toFloat yOrigin
                + (size * sin (a * n))
                |> String.fromFloat

        buildPoint n =
            x n ++ "," ++ y n
    in
    List.range 0 5
        |> List.map (toFloat >> buildPoint)
        |> String.join " "


viewHexDetailed : Maybe SectorSolarSystem -> HexId -> Int -> HexOrigin -> Float -> Svg Msg
viewHexDetailed maybeSolarSystem playerHexId hexIdx (( x, y ) as origin) size =
    let
        systemName =
            maybeSolarSystem
                |> Maybe.map (.coordinates >> .raw)
                |> Maybe.withDefault "Unknown"

        hexBg =
            "#4A90E2"

        travelZoneColor =
            defaultHexBg

        militaryBaseColor =
            if modBy 4 hexIdx == 0 then
                "red"

            else
                defaultHexBg

        navalBaseColor =
            if modBy 5 hexIdx == 0 then
                "white"

            else
                defaultHexBg

        hasStar =
            case maybeSolarSystem of
                Just solarSystem ->
                    True

                Nothing ->
                    False

        ifStarOrNot trueValue falseValue =
            if hasStar then
                trueValue

            else
                falseValue
    in
    Svg.g []
        [ -- background hex
          Svg.polygon
            [ points (hexagonPoints origin size)
            , SvgAttrs.fill defaultHexBg
            , SvgAttrs.stroke <|
                ifStarOrNot "#4A0000" defaultHexBg
            , SvgAttrs.strokeWidth <|
                ifStarOrNot "3" "2"
            , SvgAttrs.pointerEvents "visiblePainted"
            , Html.Styled.Events.onClick NoOpMsg
            , SvgAttrs.css [ hoverableStyle ]
            ]
            []
        , -- center star
          ifStarOrNot
            (Svg.circle
                [ SvgAttrs.cx <| String.fromInt <| x
                , SvgAttrs.cy <| String.fromInt <| y
                , SvgAttrs.r "15"
                , SvgAttrs.fill <|
                    (maybeSolarSystem
                        |> Maybe.map (always "white")
                        |> Maybe.withDefault "black"
                    )
                ]
                []
            )
            (Html.text "")
        , ifStarOrNot
            (Svg.g []
                [ -- travel zone ring
                  Svg.circle
                    [ SvgAttrs.cx <| String.fromInt <| x
                    , SvgAttrs.cy <| String.fromInt <| y
                    , SvgAttrs.r <| String.fromInt <| floor <| size * 0.7
                    , SvgAttrs.fill "none"
                    , SvgAttrs.stroke travelZoneColor
                    , SvgAttrs.strokeWidth "3"

                    -- hack dashes to get the circle. hope you can find a better combo of numbers
                    , SvgAttrs.strokeDasharray "170"
                    , SvgAttrs.strokeDashoffset "210"
                    ]
                    []
                , -- miliary base red star in the top left
                  Svg.circle
                    [ SvgAttrs.cx <| String.fromInt <| x - (floor <| size * 0.3)
                    , SvgAttrs.cy <| String.fromInt <| y - (floor <| size * 0.3)
                    , SvgAttrs.r "5"
                    , SvgAttrs.fill militaryBaseColor
                    ]
                    []
                , -- naval base  in the bottom left
                  Svg.circle
                    [ SvgAttrs.cx <| String.fromInt <| x - (floor <| size * 0.4)
                    , SvgAttrs.cy <| String.fromInt <| y + (floor <| size * 0.2)
                    , SvgAttrs.r "5"
                    , SvgAttrs.fill navalBaseColor
                    ]
                    []

                -- hex index
                , Svg.text_
                    [ SvgAttrs.x <| String.fromInt <| x
                    , SvgAttrs.y <| String.fromInt <| y - (floor <| size * 0.65)
                    , SvgAttrs.fontSize "12"
                    , SvgAttrs.textAnchor "middle"
                    ]
                    [ Svg.text <| String.fromInt hexIdx ]
                , -- starport
                  Svg.text_
                    [ SvgAttrs.x <| String.fromInt <| x
                    , SvgAttrs.y <| String.fromInt <| y - (floor <| size * 0.35)
                    , SvgAttrs.textAnchor "middle"
                    ]
                    [ Svg.text "A" ]

                -- , -- allegiance
                --   Svg.text_
                --     [ SvgAttrs.x <| String.fromInt <| x + (floor <| size * 0.55)
                --     , SvgAttrs.y <| String.fromInt <| y + (floor <| size * 0.15)
                --     , SvgAttrs.textAnchor "middle"
                --     , SvgAttrs.fontSize "14"
                --     ]
                --     [ Svg.text allegiance ]
                , -- UWP
                  Svg.text_
                    [ SvgAttrs.x <| String.fromInt <| x
                    , SvgAttrs.y <| String.fromInt <| y + (floor <| size * 0.5)
                    , SvgAttrs.fontFamily "Courier New"
                    , SvgAttrs.fontSize "14"
                    , SvgAttrs.textAnchor "middle"
                    ]
                    [ Svg.text "A7889C9–C" ]
                , -- system name
                  Svg.text_
                    [ SvgAttrs.x <| String.fromInt <| x
                    , SvgAttrs.y <| String.fromInt <| y + (floor <| size * 0.75)
                    , SvgAttrs.textAnchor "middle"
                    ]
                    [ Svg.text systemName ]
                ]
            )
            (Svg.text "")
        ]


defaultHexBg =
    "#e3e3e3"


viewHexSimple : Maybe SectorSolarSystem -> HexId -> Int -> HexOrigin -> Float -> Svg Msg
viewHexSimple maybeSolarSystem playerHexId hexIdx (( x, y ) as origin) size =
    let
        systemName =
            maybeSolarSystem
                |> Maybe.map (.coordinates >> .raw)
                |> Maybe.withDefault "Unknown"

        hexBg =
            if playerHexId.value == hexIdx then
                "#FF30E2"

            else
                defaultHexBg

        travelZoneColor =
            if modBy 4 hexIdx == 0 then
                "red"

            else if modBy 2 hexIdx == 0 then
                "yellow"

            else
                defaultHexBg

        hasStar =
            case maybeSolarSystem of
                Just solarSystem ->
                    True

                Nothing ->
                    False

        ifStarOrNot trueValue falseValue =
            if hasStar then
                trueValue

            else
                falseValue
    in
    Svg.g [ SvgEvents.onMouseOver (HoveringHex (HexId.createFromInt hexIdx)) ]
        [ -- background hex
          Svg.polygon
            [ points (hexagonPoints origin size)
            , SvgAttrs.fill hexBg
            , SvgAttrs.stroke <|
                ifStarOrNot "#4A0000" defaultHexBg
            , SvgAttrs.strokeWidth <|
                ifStarOrNot "3" "2"
            , SvgAttrs.pointerEvents "visiblePainted"
            , SvgEvents.onClick NoOpMsg
            ]
            []
        , -- x,y
          Svg.text_
            [ SvgAttrs.x <| String.fromInt <| x
            , SvgAttrs.y <| String.fromInt <| (y - 10)
            , SvgAttrs.fontSize "12"
            , SvgAttrs.textAnchor "middle"
            ]
            [ Svg.text (String.fromInt x ++ ", " ++ String.fromInt y)
            ]
        , -- hex id
          Svg.text_
            [ SvgAttrs.x <| String.fromInt <| x
            , SvgAttrs.y <| String.fromInt <| y
            , SvgAttrs.fontSize "12"
            , SvgAttrs.textAnchor "middle"
            ]
            [ Svg.text <| "h: " ++ String.fromInt hexIdx
            ]
        , -- center star
          ifStarOrNot
            (Svg.circle
                [ SvgAttrs.cx <| String.fromInt <| x
                , SvgAttrs.cy <| String.fromInt <| y
                , SvgAttrs.r "10"
                , SvgAttrs.fill <|
                    (maybeSolarSystem
                        |> Maybe.map (always "white")
                        |> Maybe.withDefault "black"
                    )
                ]
                []
            )
            (Html.text "")
        , ifStarOrNot
            (Svg.g []
                [ -- travel zone ring
                  Svg.circle
                    [ SvgAttrs.cx <| String.fromInt <| x
                    , SvgAttrs.cy <| String.fromInt <| y
                    , SvgAttrs.r <| String.fromInt <| floor <| size * 0.7
                    , SvgAttrs.fill "none"
                    , SvgAttrs.stroke travelZoneColor
                    , SvgAttrs.strokeWidth "3"

                    -- hack dashes to get the circle. hope you can find a better combo of numbers
                    , SvgAttrs.strokeDasharray "170"
                    , SvgAttrs.strokeDashoffset "210"
                    ]
                    []
                ]
            )
            (Svg.text "asd")
        ]


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


isEmptyHex : Maybe a -> Int
isEmptyHex maybeSolarSystem =
    case maybeSolarSystem of
        Just solarSystem ->
            1

        Nothing ->
            0


hexColOffset row =
    if remainderBy 2 row == 0 then
        0

    else
        1


{-| View all the hexes in the system
-}
viewHexes : ( SectorData, Dict.Dict Int SectorSolarSystem ) -> ( Float, Float ) -> HexId -> Float -> Html Msg
viewHexes ( sectorData, solarSystemDict ) ( horizOffset, vertOffset ) playerHexId hexSize =
    let
        calcOrigin : Int -> Int -> HexOrigin
        calcOrigin row col =
            let
                r =
                    hexSize / 1

                a =
                    2 * pi / 6

                -- x =
                --     r + toFloat col * (r + r * sin a)
                x =
                    r + toFloat col * (r + r * cos a)

                y =
                    r + toFloat row * 2 * r * sin a + r * hexColOffset col * sin a

                -- y =
                --     -- r + toFloat row * hexSize + r * hexColOffset col * sin a
                --     let
                --         base =
                --             r * sin a
                --     in
                --     hexSize
                --         + base
                --         + (base * toFloat (row - 0))
                --         + (-1 ^ toFloat (col - 0) * base)
                --r + ((-1 ^ toFloat col) * (r * toFloat col) * sin a)
            in
            ( floor x, floor y )

        viewHexRow rowIdx =
            List.range 0 numHexCols
                |> List.map (calcOrigin rowIdx)
                |> List.indexedMap
                    (\colIdx origin ->
                        let
                            idx =
                                rowIdx + (colIdx + 1) * 100 + 1

                            solarSystem =
                                Dict.get idx solarSystemDict

                            hexSVG =
                                if hexSize <= 35 then
                                    viewHexSimple solarSystem playerHexId idx origin hexSize

                                else
                                    viewHexDetailed solarSystem playerHexId idx origin hexSize
                        in
                        ( hexSVG, isEmptyHex solarSystem )
                    )
    in
    List.range 0 numHexRows
        |> List.map viewHexRow
        |> List.concat
        |> List.sortBy Tuple.second
        |> List.map Tuple.first
        |> Svg.svg
            [ SvgAttrs.width "600"
            , SvgAttrs.height "500"
            , let
                xOffset =
                    -- view horizontal offset
                    String.fromFloat (hexSize * numHexCols * horizOffset)

                yOffset =
                    -- view vertical offset
                    String.fromFloat (hexSize * numHexRows * vertOffset)
              in
              viewBox <|
                -- figure out a nicer way of dealing with this lol
                (xOffset
                    ++ " "
                    ++ yOffset
                    ++ " 600 500"
                )
            ]


hexToCoords : HexId -> ( Int, Int )
hexToCoords hexId =
    let
        row =
            hexId.value // numHexCols

        col =
            modBy numHexCols hexId.value
    in
    ( row, col )


calcDistance : HexId -> HexId -> Int
calcDistance hex1 hex2 =
    let
        ( x1, y1 ) =
            hexToCoords hex1

        ( x2, y2 ) =
            hexToCoords hex2

        colDiff =
            abs (x1 - x2)

        rowDiff =
            abs (y1 - y2)
    in
    -- wrong abs (x1 - x2) + abs (y1 - y2)
    -- this is still wrong but its closer lol, ty gpt
    max colDiff rowDiff + (min colDiff rowDiff // 2)


view : Model -> Element.Element Msg
view model =
    column [ Font.size 20, centerY ]
        [ text <|
            "Welcome to the Traveller app!"
        , text <|
            "Viewing "
                ++ String.fromInt numHexCols
                ++ " columns and "
                ++ String.fromInt numHexRows
                ++ " rows"
        , text <| "Total hexes: " ++ String.fromInt (numHexCols * numHexRows)
        , -- zoom slider
          Input.slider []
            { onChange = ZoomScaleChanged
            , label = Input.labelAbove [] (text <| "Zoom: " ++ String.fromFloat model.hexScale)
            , min = 0
            , max = 75
            , step = Just 5
            , value = model.hexScale
            , thumb =
                Input.defaultThumb
            }
        , -- horiz slider
          let
            horizOffset =
                Tuple.first model.offset
          in
          Input.slider []
            { onChange = OffsetChanged Horizontal
            , label =
                Input.labelAbove []
                    (text <| "Horiz: " ++ (String.fromFloat <| horizOffset))
            , min = 0
            , max = 1.0
            , step = Just 0.025
            , value = horizOffset
            , thumb =
                Input.defaultThumb
            }
        , -- vertical slider
          let
            vertOffset =
                Tuple.second model.offset
          in
          Input.slider []
            { onChange = OffsetChanged Vertical
            , label =
                Input.labelAbove []
                    (text <| "Vert: " ++ (String.fromFloat <| vertOffset))
            , min = 0
            , max = 1.0
            , step = Just 0.025
            , value = vertOffset
            , thumb =
                Input.defaultThumb
            }
        , column
            [ Font.size 14
            , Font.color <| Element.rgb 0.5 1.5 0.5
            ]
          <|
            [ row [ Element.spacing 15 ]
                [ text "Player HexId:"
                , text <| String.fromInt model.playerHex.value
                , case model.hoveringHex of
                    Just hoveringHex ->
                        column []
                            [ text "Hovering HexId:"
                            , text <| String.fromInt hoveringHex.value
                            , text "distance to player hex"
                            , text <| String.fromInt <| calcDistance model.playerHex hoveringHex
                            ]

                    Nothing ->
                        text <| "None yet"
                ]
            ]
        , Element.html <|
            -- Note: we use elm-css for type-safe CSS, so we need to use the Html.Styled.* dropins for Html.
            case model.sectorData of
                RemoteData.Success sectorData ->
                    Svg.Styled.Lazy.lazy4 viewHexes sectorData model.offset model.playerHex model.hexScale
                        |> Html.toUnstyled

                _ ->
                    Html.toUnstyled <| Html.text "Loading..."
        ]


sendSectorRequest : Cmd Msg
sendSectorRequest =
    let
        -- sectorParser : JsDecode.Decoder SectorData
        sectorParser =
            codecSectorData
                |> Codec.decoder
    in
    Http.get
        -- { url = "/Few Stars.json"
        { url = "/public/Deepnight.json"
        , expect = Http.expectJson DownloadedSectorJson sectorParser
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOpMsg ->
            ( model, Cmd.none )

        ZoomScaleChanged newScale ->
            ( { model | hexScale = newScale }, Cmd.none )

        DownloadSectorJson ->
            ( model
            , sendSectorRequest
            )

        DownloadedSectorJson (Ok sectorData) ->
            let
                sortedSolarSystems =
                    sectorData.solarSystems |> List.take 10 |> List.sortBy (.coordinates >> .value)

                solarSystemDict =
                    sortedSolarSystems
                        |> List.map (\system -> ( system.coordinates.value, system ))
                        |> Dict.fromList

                newSectorData =
                    { sectorData | solarSystems = sortedSolarSystems }
            in
            ( { model
                | sectorData =
                    ( newSectorData, solarSystemDict )
                        -- |> Debug.log "sector data"
                        |> RemoteData.Success
              }
            , Cmd.none
            )

        DownloadedSectorJson (Err err) ->
            case err of
                Http.BadUrl _ ->
                    Debug.todo "branch 'DownloadedSectorJson bad url' not implemented"

                Http.Timeout ->
                    Debug.todo "branch 'DownloadedSectorJson timeout' not implemented"

                Http.NetworkError ->
                    Debug.todo "branch 'DownloadedSectorJson NetworkError' not implemented"

                Http.BadStatus _ ->
                    Debug.todo "branch 'DownloadedSectorJson BadStatus' not implemented"

                Http.BadBody bodyErr ->
                    let
                        _ =
                            Debug.log "bodyErr" bodyErr
                    in
                    Debug.todo "branch 'DownloadedSectorJson BadBody' not implemented"

        OffsetChanged Horizontal horizOffset ->
            ( { model | offset = Tuple.mapFirst (always horizOffset) model.offset }, Cmd.none )

        OffsetChanged Vertical vertOffset ->
            ( { model | offset = Tuple.mapSecond (always vertOffset) model.offset }, Cmd.none )

        HoveringHex hoveringHex ->
            ( { model | hoveringHex = Just hoveringHex }, Cmd.none )
