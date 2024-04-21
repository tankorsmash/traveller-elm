module Traveller exposing (Model, Msg(..), init, subscriptions, update, view)

-- import Svg exposing (..)

import Browser.Dom
import Browser.Events
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
import Task
import Traveller.HexId as HexId exposing (HexId)
import Traveller.SectorData exposing (SectorData, codecSectorData)
import Traveller.SolarSystem exposing (SolarSystem)
import Traveller.Star as Star exposing (starColourRGB)


{-| needs to be a plain int for hashing in a Dict, otherwise we'd use full HexIds
-}
type alias RawHexId =
    Int


type alias Model =
    { hexScale : Float
    , sectorData : RemoteData Http.Error ( SectorData, Dict.Dict RawHexId SolarSystem )
    , offset : ( Float, Float )
    , playerHex : HexId
    , hoveringHex : Maybe HexId
    , viewingHexId : Maybe HexId
    , viewport : Maybe Browser.Dom.Viewport
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
    | ViewingHex HexId
    | GotViewport Browser.Dom.Viewport
    | GotResize Int Int


type alias HexOrigin =
    ( Int, Int )


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onResize GotResize


init : ( Model, Cmd Msg )
init =
    ( { hexScale = defaultHexSize
      , sectorData = RemoteData.NotAsked
      , offset = ( 0.0, 0.0 )
      , playerHex = HexId.createFromInt 135
      , hoveringHex = Nothing
      , viewingHexId = Nothing
      , viewport = Nothing
      }
    , Cmd.batch
        [ sendSectorRequest
        , Browser.Dom.getViewport
            |> Task.perform GotViewport
        ]
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


viewHexDetailed : Maybe SolarSystem -> HexId -> Int -> HexOrigin -> Float -> Svg Msg
viewHexDetailed maybeSolarSystem playerHexId hexIdx (( x, y ) as origin) size =
    let
        systemName =
            maybeSolarSystem
                |> Maybe.map (.coordinates >> .raw)
                |> Maybe.withDefault "Unknown"

        hexBg =
            "#ffffff"

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

        scaleAttr : Int -> Float
        scaleAttr default =
            toFloat default * min 1 (size / defaultHexSize)

        rotatePoint idx ( x_, y_ ) degrees_ distance =
            let
                rads =
                    (toFloat idx * degrees_)
                        - 30
                        |> degrees

                cosTheta =
                    cos rads

                sinTheta =
                    sin rads
            in
            ( x_ + (scaleAttr distance * cosTheta) - (0 * sinTheta)
            , y_ + (scaleAttr distance * sinTheta) + (0 * cosTheta)
            )
    in
    Svg.g []
        [ -- background hex
          Svg.polygon
            [ points (hexagonPoints origin size)
            , SvgAttrs.fill defaultHexBg
            , SvgAttrs.stroke "#CCCCCC"
            , SvgAttrs.strokeWidth "1"
            , SvgAttrs.pointerEvents "visiblePainted"
            , SvgAttrs.css [ hoverableStyle ]
            , SvgEvents.onMouseOver (HoveringHex (HexId.createFromInt hexIdx))
            , SvgEvents.onClick (ViewingHex (HexId.createFromInt hexIdx))
            ]
            []
        , -- center star
          let
            -- drawStar : Float -> Float -> Int -> Star.Star -> Svg Msg
            drawStar ( starX, starY ) radius star =
                Svg.circle
                    [ SvgAttrs.cx <| String.fromFloat <| starX
                    , SvgAttrs.cy <| String.fromFloat <| starY
                    , SvgAttrs.r <|
                        String.fromFloat <|
                            scaleAttr radius
                    , SvgAttrs.fill <|
                        case star.colour of
                            Just starColor ->
                                starColourRGB starColor

                            Nothing ->
                                "#FF00FF"
                    ]
                    []
          in
          case maybeSolarSystem of
            Just solarSystem ->
                case solarSystem.stars of
                    [] ->
                        Html.text ""

                    primaryStar :: stars ->
                        let
                            primaryPos =
                                ( toFloat x, toFloat y )
                        in
                        Svg.g
                            []
                            ((case primaryStar.companion of
                                Just compStar ->
                                    let
                                        compStarPos =
                                            Tuple.mapFirst (\x_ -> x_ - 5) primaryPos
                                    in
                                    Svg.g []
                                        [ drawStar primaryPos 12 primaryStar
                                        , drawStar compStarPos 6 compStar
                                        ]

                                Nothing ->
                                    drawStar primaryPos 12 primaryStar
                             )
                                :: List.indexedMap
                                    (\idx secondaryStar ->
                                        let
                                            secondaryStarPos =
                                                rotatePoint idx primaryPos 60 20
                                        in
                                        case secondaryStar.companion of
                                            Just compStar ->
                                                let
                                                    compStarPos =
                                                        Tuple.mapFirst (\x_ -> x_ - 5) secondaryStarPos
                                                in
                                                Svg.g []
                                                    [ drawStar secondaryStarPos 7 secondaryStar
                                                    , drawStar compStarPos 3 compStar
                                                    ]

                                            Nothing ->
                                                drawStar secondaryStarPos 7 secondaryStar
                                    )
                                    stars
                            )

            Nothing ->
                Html.text ""
        , ifStarOrNot
            (Svg.g []
                [ --   -- travel zone ring
                  -- Svg.circle
                  --   [ SvgAttrs.cx <| String.fromInt <| x
                  --   , SvgAttrs.cy <| String.fromInt <| y
                  --   , SvgAttrs.r <| String.fromInt <| floor <| size * 0.7
                  --   , SvgAttrs.fill "none"
                  --   , SvgAttrs.stroke travelZoneColor
                  --   , SvgAttrs.strokeWidth "1"
                  --
                  --   -- hack dashes to get the circle. hope you can find a better combo of numbers
                  --   , SvgAttrs.strokeDasharray "170"
                  --   , SvgAttrs.strokeDashoffset "210"
                  --   ]
                  --   []
                  --, -- miliary base red star in the top left
                  --  Svg.circle
                  --    [ SvgAttrs.cx <| String.fromInt <| x - (floor <| size * 0.3)
                  --    , SvgAttrs.cy <| String.fromInt <| y - (floor <| size * 0.3)
                  --    , SvgAttrs.r "5"
                  --    , SvgAttrs.fill militaryBaseColor
                  --    ]
                  --    []
                  --, -- naval base  in the bottom left
                  --  Svg.circle
                  --    [ SvgAttrs.cx <| String.fromInt <| x - (floor <| size * 0.4)
                  --    , SvgAttrs.cy <| String.fromInt <| y + (floor <| size * 0.2)
                  --    , SvgAttrs.r "5"
                  --    , SvgAttrs.fill navalBaseColor
                  --    ]
                  --    []
                  -- hex index
                  Svg.text_
                    [ SvgAttrs.x <| String.fromInt <| x
                    , SvgAttrs.y <| String.fromInt <| y - (floor <| size * 0.65)
                    , SvgAttrs.fontSize "12"
                    , SvgAttrs.textAnchor "middle"
                    ]
                    [ String.fromInt hexIdx
                        |> String.pad 4 '0'
                        |> Svg.text
                    ]

                -- , -- starport
                --   Svg.text_
                --     [ SvgAttrs.x <| String.fromInt <| x
                --     , SvgAttrs.y <| String.fromInt <| y - (floor <| size * 0.35)
                --     , SvgAttrs.textAnchor "middle"
                --     ]
                --     [ Svg.text "A" ]
                -- , -- allegiance
                --   Svg.text_
                --     [ SvgAttrs.x <| String.fromInt <| x + (floor <| size * 0.55)
                --     , SvgAttrs.y <| String.fromInt <| y + (floor <| size * 0.15)
                --     , SvgAttrs.textAnchor "middle"
                --     , SvgAttrs.fontSize "14"
                --     ]
                --     [ Svg.text allegiance ]
                -- , -- UWP
                --   Svg.text_
                --     [ SvgAttrs.x <| String.fromInt <| x
                --     , SvgAttrs.y <| String.fromInt <| y + (floor <| size * 0.5)
                --     , SvgAttrs.fontFamily "Courier New"
                --     , SvgAttrs.fontSize "14"
                --     , SvgAttrs.textAnchor "middle"
                --     ]
                --     [ Svg.text "A7889C9–C" ]
                -- , -- system name
                --   Svg.text_
                --     [ SvgAttrs.x <| String.fromInt <| x
                --     , SvgAttrs.y <| String.fromInt <| y + (floor <| size * 0.75)
                --     , SvgAttrs.textAnchor "middle"
                --     ]
                --     [ Svg.text systemName ]
                ]
            )
            (Svg.text "")
        ]


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


calcOrigin : Float -> Int -> Int -> HexOrigin
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


{-| View all the hexes in the system
-}
viewHexes : Browser.Dom.Viewport -> ( SectorData, Dict.Dict Int SolarSystem ) -> ( Float, Float ) -> HexId -> Float -> Html Msg
viewHexes viewport ( sectorData, solarSystemDict ) ( horizOffset, vertOffset ) playerHexId hexSize =
    let
        viewHexRow rowIdx =
            List.range 0 numHexCols
                |> List.map (calcOrigin hexSize rowIdx)
                |> List.indexedMap
                    (\colIdx origin ->
                        let
                            idx =
                                rowIdx + 1 + (colIdx + 1) * 100

                            solarSystem =
                                Dict.get idx solarSystemDict

                            hexSVG =
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
        |> (let
                width =
                    String.fromFloat <| viewport.viewport.width * 0.9

                height =
                    String.fromFloat <| viewport.viewport.height * 0.9
            in
            Svg.svg
                [ SvgAttrs.width <| width
                , SvgAttrs.height <| height
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
                        ++ " "
                        ++ width
                        ++ " "
                        ++ height
                    )
                ]
           )


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
    column [ Font.size 20, centerX, centerY, Font.color <| Element.rgb 0.5 1.5 0.5 ]
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
            , thumb = Input.defaultThumb
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
            , thumb = Input.defaultThumb
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
        , column
            []
            [ case model.sectorData of
                RemoteData.Success ( sectorData, solarSystemDict ) ->
                    case
                        model.viewingHexId
                            |> Maybe.andThen
                                (\hid ->
                                    Dict.get hid.value solarSystemDict
                                )
                    of
                        Just solarSystem ->
                            let
                                renderStar star =
                                    star.stellarType
                                        ++ (case star.subtype of
                                                Just num ->
                                                    "" ++ String.fromInt num

                                                Nothing ->
                                                    ""
                                           )
                                        ++ " "
                                        ++ star.stellarClass
                            in
                            solarSystem.stars
                                |> List.map
                                    (\star ->
                                        renderStar star
                                            ++ (star.companion
                                                    |> Maybe.map
                                                        (\compStar ->
                                                            "\n  \\----> " ++ renderStar compStar
                                                        )
                                                    |> Maybe.withDefault ""
                                               )
                                    )
                                |> List.map text
                                |> column []

                        Nothing ->
                            text "No solar system data yet"

                _ ->
                    text "No sector data yet"
            ]
        , Element.html <|
            -- Note: we use elm-css for type-safe CSS, so we need to use the Html.Styled.* dropins for Html.
            case ( model.sectorData, model.viewport ) of
                ( RemoteData.Success sectorData, Just viewport ) ->
                    Svg.Styled.Lazy.lazy5 viewHexes viewport sectorData model.offset model.playerHex model.hexScale
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
                    sectorData.solarSystems |> List.sortBy (.coordinates >> .value)

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
                            Debug.log bodyErr ""
                    in
                    Debug.todo "branch 'DownloadedSectorJson BadBody' not implemented"

        OffsetChanged Horizontal horizOffset ->
            ( { model | offset = Tuple.mapFirst (always horizOffset) model.offset }, Cmd.none )

        OffsetChanged Vertical vertOffset ->
            ( { model | offset = Tuple.mapSecond (always vertOffset) model.offset }, Cmd.none )

        HoveringHex hoveringHex ->
            ( { model | hoveringHex = Just hoveringHex }, Cmd.none )

        GotViewport viewport ->
            ( { model | viewport = Just viewport }, Cmd.none )

        GotResize width_ height_ ->
            ( model
            , Browser.Dom.getViewport
                |> Task.perform GotViewport
            )

        ViewingHex hexId ->
            let
                goodValX =
                    Debug.log "y" <| (hexId.value // 100) - 1

                goodValY =
                    Debug.log "y" <| modBy 100 hexId.value - 1

                origin =
                    Debug.log "orign" <|
                        calcOrigin model.hexScale goodValY goodValX
            in
            ( { model | viewingHexId = Just hexId }, Cmd.none )
