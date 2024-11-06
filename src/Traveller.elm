module Traveller exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser.Dom
import Browser.Events
import Browser.Navigation
import Codec
import Css
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
import Maybe.Extra as Maybe
import Parser
import RemoteData exposing (RemoteData(..))
import Round exposing (round)
import Svg.Attributes exposing (width)
import Svg.Styled as Svg exposing (Svg)
import Svg.Styled.Attributes as SvgAttrs exposing (fill, points, viewBox)
import Svg.Styled.Events as SvgEvents
import Svg.Styled.Lazy
import Task
import Traveller.HexId as HexId exposing (HexId, RawHexId)
import Traveller.Orbit exposing (StellarOrbit(..))
import Traveller.Parser as TravellerParser
import Traveller.SectorData exposing (SISector, SectorData, SurveyIndexData, codecSectorData, codecSurveyIndexData)
import Traveller.SolarSystem exposing (SolarSystem)
import Traveller.StellarObject exposing (GasGiantData, PlanetoidBeltData, PlanetoidData, StarData(..), StarDataConfig, StellarObjectX(..), TerrestrialData, getStarDataConfig, starColourRGB)


gasGiantSI =
    5


terrestrialSI =
    6


planetoidSI =
    6


type alias Model =
    { key : Browser.Navigation.Key
    , hexScale : Float
    , sectorData : RemoteData Http.Error ( SectorData, Dict.Dict RawHexId SolarSystem )
    , offset : ( Float, Float )
    , playerHex : HexId
    , hoveringHex : Maybe HexId
    , viewingHexId : Maybe ( HexId, Int )
    , viewingHexOrigin : Maybe ( Int, Int )
    , viewport : Maybe Browser.Dom.Viewport
    , hexmapViewport : Maybe (Result Browser.Dom.Error Browser.Dom.Viewport)
    , surveyIndexData : RemoteData Http.Error SurveyIndexData
    }


type OffsetDirection
    = Horizontal
    | Vertical


type Msg
    = NoOpMsg
    | ZoomScaleChanged Float
    | DownloadSurveyIndexJson
    | DownloadedSurveyIndexJson (Result Http.Error SurveyIndexData)
    | DownloadSectorJson
    | DownloadedSectorJson (Result Http.Error SectorData)
    | OffsetChanged OffsetDirection Float
    | HoveringHex HexId
    | ViewingHex ( HexId, Int )
    | GotViewport Browser.Dom.Viewport
    | GotHexMapViewport (Result Browser.Dom.Error Browser.Dom.Viewport)
    | GotResize Int Int
    | GoToSolarSystemPage HexId


type alias HexOrigin =
    ( Int, Int )


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onResize GotResize


init : Browser.Navigation.Key -> ( Model, Cmd Msg )
init key =
    ( { hexScale = defaultHexSize
      , sectorData = RemoteData.NotAsked
      , surveyIndexData = RemoteData.NotAsked
      , offset = ( 0.0, 0.0 )
      , playerHex = HexId.createFromInt 135
      , hoveringHex = Nothing
      , viewingHexId = Nothing
      , viewingHexOrigin = Nothing
      , viewport = Nothing
      , hexmapViewport = Nothing
      , key = key
      }
    , Cmd.batch
        [ sendSectorRequest
        , sendSurveyIndexRequest
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


viewHexDetailed : Maybe SolarSystem -> Int -> HexId -> Int -> HexOrigin -> Float -> Svg Msg
viewHexDetailed maybeSolarSystem si playerHexId hexIdx (( x, y ) as origin) size =
    let
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
    Svg.g
        [ SvgEvents.onMouseOver (HoveringHex (HexId.createFromInt hexIdx))
        , SvgEvents.onClick (ViewingHex ( HexId.createFromInt hexIdx, si ))
        ]
        [ -- background hex
          Svg.polygon
            [ points (hexagonPoints origin size)
            , SvgAttrs.fill defaultHexBg
            , SvgAttrs.stroke "#CCCCCC"
            , SvgAttrs.strokeWidth "1"
            , SvgAttrs.pointerEvents "visiblePainted"
            , SvgAttrs.css [ hoverableStyle ]
            ]
            []
        , -- center star
          let
            drawStar : ( Float, Float ) -> Int -> StarDataConfig -> Svg Msg
            drawStar ( starX, starY ) radius star =
                Svg.circle
                    [ SvgAttrs.cx <| String.fromFloat <| starX
                    , SvgAttrs.cy <| String.fromFloat <| starY
                    , SvgAttrs.r <|
                        String.fromFloat <|
                            scaleAttr radius
                    , SvgAttrs.fill <|
                        starColourRGB star.colour
                    ]
                    []
          in
          case maybeSolarSystem of
            Just solarSystem ->
                --(StarData primaryStar) :: stars ->
                let
                    primaryPos =
                        ( toFloat x, toFloat y )

                    primaryStar =
                        getStarDataConfig solarSystem.primaryStar
                in
                Svg.g
                    []
                    ((case primaryStar.companion of
                        Just (StarData compStarData) ->
                            let
                                compStarPos =
                                    Tuple.mapFirst (\x_ -> x_ - 5) primaryPos
                            in
                            Svg.g []
                                [ drawStar primaryPos 12 primaryStar
                                , drawStar compStarPos 6 compStarData
                                ]

                        Nothing ->
                            drawStar primaryPos 12 primaryStar
                     )
                        :: List.indexedMap
                            (\idx stellarObject ->
                                let
                                    secondaryStarPos =
                                        rotatePoint idx primaryPos 60 20
                                in
                                case stellarObject of
                                    Star (StarData star) ->
                                        case star.companion of
                                            Just (StarData compStarData) ->
                                                let
                                                    compStarPos =
                                                        Tuple.mapFirst (\x_ -> x_ - 5) secondaryStarPos
                                                in
                                                Svg.g []
                                                    [ drawStar secondaryStarPos 7 star
                                                    , drawStar compStarPos 3 compStarData
                                                    ]

                                            Nothing ->
                                                drawStar secondaryStarPos 7 star

                                    _ ->
                                        Html.text ""
                            )
                            primaryStar.stellarObjects
                    )

            Nothing ->
                Html.text ""
        , case maybeSolarSystem of
            Just solarSystem ->
                Svg.g []
                    [ -- hex index
                      Svg.text_
                        [ SvgAttrs.x <| String.fromInt <| x
                        , SvgAttrs.y <| String.fromInt <| y - (floor <| size * 0.65)
                        , SvgAttrs.fontSize "10"
                        , SvgAttrs.textAnchor "middle"
                        ]
                        [ String.fromInt hexIdx
                            |> String.pad 4 '0'
                            |> Svg.text
                        ]
                    , Svg.text_
                        [ SvgAttrs.x <| String.fromInt <| x
                        , SvgAttrs.y <| String.fromInt <| y + (floor <| size * 0.85)
                        , SvgAttrs.fontSize "12"
                        , SvgAttrs.textAnchor "middle"
                        ]
                        (let
                            showGasGiants =
                                if si >= gasGiantSI then
                                    String.fromInt <| solarSystem.gasGiants

                                else
                                    "?"

                            showTerrestrialPlanets =
                                if si >= terrestrialSI then
                                    String.fromInt <| solarSystem.terrestrialPlanets

                                else
                                    "?"

                            showplanetoidBelts =
                                if si >= planetoidSI then
                                    String.fromInt <| solarSystem.planetoidBelts

                                else
                                    "?"
                         in
                         [ Svg.tspan
                            [ SvgAttrs.fill "#109076" ]
                            [ showGasGiants |> Svg.text ]
                         , Svg.text " / "
                         , Svg.tspan
                            [ SvgAttrs.fill "#809076" ]
                            [ showTerrestrialPlanets |> Svg.text ]
                         , Svg.text " / "
                         , Svg.tspan
                            [ SvgAttrs.fill "#68B976" ]
                            [ showplanetoidBelts |> Svg.text ]
                         ]
                        )
                    ]

            Nothing ->
                Svg.text ""
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


viewHex :
    Browser.Dom.Viewport
    -> Float
    -> ( SectorData, Dict.Dict Int SolarSystem, Maybe SISector )
    -> ( Float, Float )
    -> ( Float, Float )
    -> ( Int, Int )
    -> HexOrigin
    -> HexId
    -> ( Maybe (Svg Msg), Int )
viewHex widestViewport hexSize ( sectorData, solarSystemDict, maybeSISector ) ( horizOffsetPct, vertOffsetPct ) ( viewportWidth, viewportHeight ) ( colIdx, rowIdx ) ( ox, oy ) playerHexId =
    let
        idx =
            (rowIdx + 1) + (colIdx + 1) * 100

        ( fox, foy ) =
            ( toFloat ox, toFloat oy )

        outsideX =
            let
                plus =
                    fox + hexSize - (viewportWidth * horizOffsetPct)

                minus =
                    fox - hexSize - (viewportWidth * horizOffsetPct)
            in
            (plus < 0) || (minus > widestViewport.viewport.width)

        outsideY =
            let
                plus =
                    foy + hexSize - (viewportHeight * vertOffsetPct)

                minus =
                    foy - hexSize - (viewportHeight * vertOffsetPct)
            in
            (plus < 0) || (minus > widestViewport.viewport.height)

        solarSystem =
            Dict.get idx solarSystemDict

        hexSVG =
            if not (outsideX || outsideY) then
                Just
                    (viewHexDetailed solarSystem
                        (case maybeSISector of
                            Just siSector ->
                                case Dict.get (String.padLeft 4 '0' <| String.fromInt idx) siSector.hexes of
                                    Just si2 ->
                                        si2

                                    Nothing ->
                                        0

                            Nothing ->
                                0
                        )
                        playerHexId
                        idx
                        ( ox, oy )
                        hexSize
                    )

            else
                Nothing
    in
    ( hexSVG, isEmptyHex solarSystem )


{-| View all the hexes in the system
-}
viewHexes : Maybe ( Int, Int ) -> { screenVp : Browser.Dom.Viewport, hexmapVp : Maybe Browser.Dom.Viewport } -> ( SectorData, Dict.Dict Int SolarSystem ) -> SurveyIndexData -> ( Float, Float ) -> HexId -> Float -> Html Msg
viewHexes viewingHexOrigin { screenVp, hexmapVp } ( sectorData, solarSystemDict ) surveyIndexData ( horizOffset, vertOffset ) playerHexId hexSize =
    let
        maybeSISector : Maybe SISector
        maybeSISector =
            surveyIndexData
                |> List.filter (\sector -> sector.x == sectorData.x && sector.y == sectorData.y)
                |> List.head

        viewportHeightIsh =
            screenVp.viewport.height * 0.9

        viewportWidthIsh =
            min (screenVp.viewport.width * 0.9)
                (screenVp.viewport.width - 500.0)

        xOffset =
            -- view horizontal offset
            String.fromFloat (viewportWidthIsh * horizOffset)

        yOffset =
            -- view vertical offset
            String.fromFloat (viewportHeightIsh * vertOffset)

        widestViewport =
            case hexmapVp of
                Nothing ->
                    screenVp

                Just hexmapViewport ->
                    hexmapViewport

        viewHexRow : Int -> List ( Maybe (Svg Msg), Int )
        viewHexRow rowIdx =
            List.range 0 numHexCols
                |> List.map (calcOrigin hexSize rowIdx)
                |> List.indexedMap
                    (\colIdx hexOrigin ->
                        viewHex
                            widestViewport
                            hexSize
                            ( sectorData, solarSystemDict, maybeSISector )
                            ( horizOffset, vertOffset )
                            ( viewportWidthIsh, viewportHeightIsh )
                            ( colIdx, rowIdx )
                            hexOrigin
                            playerHexId
                    )
    in
    List.range 0 numHexRows
        |> List.map viewHexRow
        |> List.concat
        |> List.filterMap
            (\( maybeHex, condition ) ->
                case maybeHex of
                    Just hex ->
                        Just ( hex, condition )

                    Nothing ->
                        Nothing
            )
        |> List.sortBy Tuple.second
        |> List.map Tuple.first
        |> (let
                stringWidth =
                    String.fromFloat <| viewportWidthIsh

                stringHeight =
                    String.fromFloat <| viewportHeightIsh

                bootstrapDark =
                    "#212529"

                midColor =
                    "#f5f5f5"
            in
            Svg.svg
                [ SvgAttrs.width <| stringWidth
                , SvgAttrs.height <| stringHeight
                , SvgAttrs.style <|
                    -- "background-color: "
                    --     ++ "blue"
                    --     ++ ";"
                    --
                    "background-image: radial-gradient("
                        ++ midColor
                        ++ ", "
                        ++ bootstrapDark
                        ++ ", "
                        ++ bootstrapDark
                        ++ " );"
                , SvgAttrs.css <|
                    [ Css.before
                        [ Css.boxShadowMany
                            [ { offsetX = Css.px 0
                              , offsetY = Css.px 0
                              , blurRadius =
                                    Just (Css.px 10)
                              , spreadRadius =
                                    Just (Css.px 10)
                              , color = Just (Css.hex "#FFFFFF")
                              , inset = True
                              }
                            ]
                        ]
                    ]
                , SvgAttrs.id "hexmap"
                , viewBox <|
                    xOffset
                        ++ " "
                        ++ yOffset
                        ++ " "
                        ++ stringWidth
                        ++ " "
                        ++ stringHeight
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


{-| Builds a monospace text element
-}
monospaceText : String -> Element.Element msg
monospaceText someString =
    text someString |> el [ Font.family [ Font.monospace ] ]


renderGasGiant : Int -> GasGiantData -> Element.Element msg
renderGasGiant newNestingLevel gasGiantData =
    row
        [ Element.spacing 8
        , Element.moveRight <| calcNestedOffset newNestingLevel
        , Font.size 14
        ]
        [ renderRawOrbit gasGiantData.orbit
        , text gasGiantData.orbitSequence
        , text (gasGiantData.code |> Maybe.withDefault "??")
        , text "🛢"
        , text <| "j: " ++ gasGiantData.safeJumpTime
        ]


renderTerrestrialPlanet : Int -> TerrestrialData -> Element.Element msg
renderTerrestrialPlanet newNestingLevel terrestrialData =
    row
        [ Element.spacing 8
        , Element.moveRight <| calcNestedOffset newNestingLevel
        , Font.size 14
        ]
        [ renderRawOrbit terrestrialData.orbit
        , let
            rawUwp =
                terrestrialData.uwp
          in
          case Parser.run TravellerParser.uwp rawUwp of
            Ok uwpData ->
                column []
                    [ monospaceText <| rawUwp
                    , -- temporarily using Debug.toString
                      monospaceText <| Debug.toString uwpData.size
                    ]

            Err _ ->
                monospaceText <| rawUwp
        , text terrestrialData.orbitSequence
        , text "🌍"
        , text <| "j: " ++ terrestrialData.safeJumpTime
        ]


calcNestedOffset : Int -> Float
calcNestedOffset newNestingLevel =
    toFloat <| newNestingLevel * 10


renderRawOrbit : Float -> Element.Element msg
renderRawOrbit orbit =
    row []
        [ monospaceText <| Round.round 2 orbit
        , text " ("
        , monospaceText <| Round.round 2 (orbit * 2.5)
        , text ")"
        ]


renderPlanetoidBelt : Int -> PlanetoidBeltData -> Element.Element msg
renderPlanetoidBelt newNestingLevel planetoidBeltData =
    row
        [ Element.spacing 8
        , Element.moveRight <| calcNestedOffset newNestingLevel
        , Font.size 14
        ]
        [ renderRawOrbit planetoidBeltData.orbit
        , let
            rawUwp =
                planetoidBeltData.uwp
          in
          case Parser.run TravellerParser.uwp rawUwp of
            Ok uwpData ->
                column []
                    [ monospaceText <| rawUwp

                    -- , -- temporarily using Debug.toString
                    --   monospaceText <| Debug.toString uwpData.size
                    ]

            Err _ ->
                monospaceText <| rawUwp
        , text planetoidBeltData.orbitSequence
        , text "🗿"
        , text <| "j: " ++ planetoidBeltData.safeJumpTime
        ]


renderPlanetoid : Int -> PlanetoidData -> Element.Element msg
renderPlanetoid newNestingLevel planetoidData =
    row
        [ Element.spacing 8
        , Element.moveRight <| calcNestedOffset newNestingLevel
        , Font.size 14
        ]
        [ renderRawOrbit planetoidData.orbit
        , let
            rawUwp =
                planetoidData.uwp
          in
          case Parser.run TravellerParser.uwp rawUwp of
            Ok uwpData ->
                column []
                    [ monospaceText <| planetoidData.uwp
                    , -- temporarily using Debug.toString
                      monospaceText <| Debug.toString uwpData.size
                    ]

            Err _ ->
                monospaceText <| rawUwp
        , text planetoidData.orbitSequence
        , text "🌎"
        , text <| "j: " ++ planetoidData.safeJumpTime
        ]


renderStellarObject : Int -> StellarObjectX -> Element.Element msg
renderStellarObject newNestingLevel stellarObject =
    row
        [ Element.spacing 8
        , Font.size 14
        ]
        [ case stellarObject of
            GasGiant gasGiantData ->
                renderGasGiant newNestingLevel gasGiantData

            TerrestrialPlanet terrestrialData ->
                renderTerrestrialPlanet newNestingLevel terrestrialData

            PlanetoidBelt planetoidBeltData ->
                renderPlanetoidBelt newNestingLevel planetoidBeltData

            Planetoid planetoidData ->
                renderPlanetoid newNestingLevel planetoidData

            Star starDataConfig ->
                renderStar starDataConfig (newNestingLevel + 1)
        ]


renderStar : StarData -> Int -> Element.Element msg
renderStar (StarData starData) nestingLevel =
    column [ Element.moveRight <| toFloat <| nestingLevel * 10 ]
        [ el [ Font.size 16, Font.bold ] <|
            text <|
                (List.repeat nestingLevel "↳" |> String.join "")
                    ++ starData.stellarType
                    ++ (case starData.subtype of
                            Just num ->
                                String.fromInt num

                            Nothing ->
                                ""
                       )
                    ++ " "
                    ++ starData.stellarClass
        , starData.companion
            |> Maybe.map
                (\compStarData ->
                    renderStar compStarData (nestingLevel + 1)
                )
            |> Maybe.withDefault Element.none
        , column [] <| List.map (renderStellarObject <| nestingLevel + 1) starData.stellarObjects
        ]


viewSystemDetailsSidebar : ( HexId, Int ) -> Maybe HexOrigin -> SolarSystem -> Element Msg
viewSystemDetailsSidebar ( viewingHexId, si ) maybeViewingHexOrigin solarSystem =
    column [ Element.spacing 10 ] <|
        [ -- render the nested chart of the system
          renderStar solarSystem.primaryStar 0
        , -- the button to load the solar system
          Input.button
            [ Background.color <| rgb 0.5 1.5 0.5
            , Border.rounded 5
            , Border.width 10
            , Border.color <| rgb 0.5 1.5 0.5
            , Font.size 14
            , Font.color <| rgb 0 0 0
            , Element.spacing 10
            , -- limited styling for mouse over
              Element.mouseOver [ Border.color <| rgb 0.9 0.9 0.9, Background.color <| rgb 0.9 0.9 0.9 ]
            ]
            { onPress = Just <| GoToSolarSystemPage viewingHexId
            , label = text <| "Visualize Solar System: " ++ viewingHexId.raw
            }
        ]


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
    let
        sidebarColumn =
            column [ centerX ]
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
                    , min = 1
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
                  Input.slider [ height <| px 50 ]
                    { onChange = OffsetChanged Horizontal
                    , label =
                        Input.labelAbove []
                            (text <| "Horiz: " ++ (Round.round 3 <| horizOffset))
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
                  Input.slider [ height <| px 50 ]
                    { onChange = OffsetChanged Vertical
                    , label =
                        Input.labelAbove []
                            (text <| "Vert: " ++ (Round.round 3 <| vertOffset))
                    , min = 0
                    , max = 1.0
                    , step = Just 0.025
                    , value = vertOffset
                    , thumb = Input.defaultThumb
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
                , case model.sectorData of
                    Success ( sectorData, solarSystemDict ) ->
                        case model.viewingHexId of
                            Just ( viewingHexId, si ) ->
                                case solarSystemDict |> Dict.get viewingHexId.value of
                                    Just solarSystem ->
                                        viewSystemDetailsSidebar
                                            ( viewingHexId, si )
                                            model.viewingHexOrigin
                                            solarSystem

                                    Nothing ->
                                        text "No solar system data found in dict"

                            Nothing ->
                                text "No viewing hex data yet"

                    _ ->
                        text "No loaded sector data yet"
                ]

        hexesColumn =
            column []
                [ Element.html <|
                    -- Note: we use elm-css for type-safe CSS, so we need to use the Html.Styled.* dropins for Html.
                    case ( model.sectorData, model.viewport, model.surveyIndexData ) of
                        ( RemoteData.Success sectorData, Just viewport, RemoteData.Success surveyIndexData ) ->
                            viewHexes
                                model.viewingHexOrigin
                                (case model.hexmapViewport of
                                    Nothing ->
                                        { screenVp = viewport, hexmapVp = Nothing }

                                    Just (Ok hexmapViewport) ->
                                        { screenVp = viewport
                                        , hexmapVp = Just hexmapViewport
                                        }

                                    Just (Err domError) ->
                                        let
                                            _ =
                                                Debug.log "cant use, domError" domError
                                        in
                                        { screenVp = viewport, hexmapVp = Nothing }
                                )
                                sectorData
                                surveyIndexData
                                model.offset
                                model.playerHex
                                model.hexScale
                                |> Html.toUnstyled

                        ( RemoteData.Failure _, _, _ ) ->
                            Html.toUnstyled <|
                                Html.div
                                    [ Html.Styled.Attributes.css
                                        [ Css.color (Css.hex "#ff0000") ]
                                    ]
                                    [ Html.text "Failed to decode SectorData. See console for details" ]

                        ( NotAsked, _, _ ) ->
                            Html.toUnstyled <|
                                Html.div
                                    [ Html.Styled.Attributes.css
                                        [ Css.color (Css.rgb 100 100 100) ]
                                    ]
                                    [ Html.text "Not asked" ]

                        ( Loading, _, _ ) ->
                            Html.toUnstyled <| Html.text "Loading..."

                        ( Success _, Nothing, _ ) ->
                            Html.toUnstyled <| Html.text "Have sector data but no viewport"

                        ( Success _, Just _, NotAsked ) ->
                            Html.toUnstyled <| Html.text "Have sector data and viewport, but not yet asked for survey index data"

                        ( Success _, Just _, Loading ) ->
                            Html.toUnstyled <| Html.text "Have sector data and viewport, but loading survey index data"

                        ( Success _, Just _, Failure _ ) ->
                            Html.toUnstyled <| Html.text "Have sector data and viewport, but failed to load survey index data"
                ]
    in
    column [ centerX, centerY ]
        [ row [ Font.size 20, Font.color <| Element.rgb 0.5 1.5 0.5 ]
            [ sidebarColumn
            , hexesColumn
            ]
        , -- displaying json errors for SectorData
          case model.sectorData of
            Failure (Http.BadBody error) ->
                (-- turn html into elm-ui
                 Element.html <|
                    -- convert from elm-css's HTML
                    Html.toUnstyled
                    <|
                        -- use <pre> to preserve whitespace
                        Html.pre [ Html.Styled.Attributes.css [ Css.overflow Css.hidden ] ] [ Html.text error ]
                )

            _ ->
                Element.none
        , -- displaying json errors for SurveyIndexData
          case model.surveyIndexData of
            Failure (Http.BadBody error) ->
                (-- turn html into elm-ui
                 Element.html
                    << -- convert from elm-css's HTML
                       Html.toUnstyled
                 <|
                    -- use <pre> to preserve whitespace
                    Html.pre [ Html.Styled.Attributes.css [ Css.overflow Css.hidden ] ] [ Html.text error ]
                )

            _ ->
                Element.none
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


sendSurveyIndexRequest : Cmd Msg
sendSurveyIndexRequest =
    let
        surveyIndexParser =
            codecSurveyIndexData
                |> Codec.decoder
    in
    Http.get
        { url = "https://radiofreewaba.net/deepnight/surveyIndexes.json"
        , expect = Http.expectJson DownloadedSurveyIndexJson surveyIndexParser
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
                        |> RemoteData.Success
              }
            , Cmd.batch
                [ Browser.Dom.getViewportOf "hexmap"
                    |> Task.attempt GotHexMapViewport
                ]
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
                            Debug.log bodyErr "__ END OF ERROR Traveller.elm __"
                    in
                    ( { model | sectorData = RemoteData.Failure err }, Cmd.none )

        DownloadSurveyIndexJson ->
            ( model
            , sendSurveyIndexRequest
            )

        DownloadedSurveyIndexJson (Ok surveyIndexData) ->
            ( { model
                | surveyIndexData =
                    surveyIndexData
                        |> RemoteData.Success
              }
            , Cmd.none
            )

        DownloadedSurveyIndexJson (Err err) ->
            case err of
                Http.BadUrl _ ->
                    Debug.todo "branch 'DownloadedSurveyIndexJson bad url' not implemented"

                Http.Timeout ->
                    Debug.todo "branch 'DownloadedSurveyIndexJson timeout' not implemented"

                Http.NetworkError ->
                    Debug.todo "branch 'DownloadedSurveyIndexJson NetworkError' not implemented"

                Http.BadStatus _ ->
                    Debug.todo "branch 'DownloadedSurveyIndexJson BadStatus' not implemented"

                Http.BadBody bodyErr ->
                    let
                        _ =
                            Debug.log bodyErr ""
                    in
                    Debug.todo "branch 'DownloadedSurveyIndexJson BadBody' not implemented"

        OffsetChanged Horizontal horizOffset ->
            ( { model | offset = Tuple.mapFirst (always horizOffset) model.offset }, Cmd.none )

        OffsetChanged Vertical vertOffset ->
            ( { model
                | offset =
                    Tuple.mapSecond (always vertOffset) model.offset
              }
            , Cmd.none
            )

        HoveringHex hoveringHex ->
            ( { model | hoveringHex = Just hoveringHex }, Cmd.none )

        GotViewport viewport ->
            ( { model | viewport = Just viewport }
            , Browser.Dom.getViewportOf "hexmap"
                |> Task.attempt GotHexMapViewport
            )

        GotHexMapViewport hexmapOrErr ->
            ( { model | hexmapViewport = Just hexmapOrErr }, Cmd.none )

        GotResize width_ height_ ->
            ( model
            , Cmd.batch
                [ Browser.Dom.getViewport
                    |> Task.perform GotViewport
                , Browser.Dom.getViewportOf "hexmap"
                    |> Task.attempt GotHexMapViewport
                ]
            )

        ViewingHex ( hexId, si ) ->
            let
                goodValX =
                    (hexId.value // 100) - 1

                goodValY =
                    modBy 100 hexId.value - 1

                ( ox, oy ) =
                    calcOrigin model.hexScale goodValY goodValX

                ( fox, foy ) =
                    ( toFloat ox, toFloat oy )

                newOffsetPct : ( Float, Float )
                newOffsetPct =
                    let
                        vpWidth =
                            case model.hexmapViewport of
                                Nothing ->
                                    Debug.todo "GotHexMapViewport doesnt exist yet"

                                Just (Ok hexmapViewport) ->
                                    Debug.log "width" hexmapViewport.viewport.width

                                Just (Err _) ->
                                    Debug.todo "GotHexMapViewport error"

                        vpHeight =
                            case model.hexmapViewport of
                                Nothing ->
                                    Debug.todo "GotHexMapViewport doesnt exist yet"

                                Just (Ok hexmapViewport) ->
                                    hexmapViewport.viewport.height

                                Just (Err _) ->
                                    Debug.todo "GotHexMapViewport error"
                    in
                    ( clamp 0 1 <| (fox - (vpWidth * 0.5)) / vpWidth
                    , clamp 0 1 <| (foy - (vpHeight * 0.5)) / vpHeight
                    )

                maybeSolarSystem =
                    case model.sectorData of
                        RemoteData.Success ( sectorData, solarSystemDict ) ->
                            Dict.get hexId.value solarSystemDict

                        _ ->
                            Nothing
            in
            ( { model
                | viewingHexId = Just ( hexId, si )
                , viewingHexOrigin = Just ( ox, oy )
                , offset = Debug.log "new offset" newOffsetPct
              }
            , Cmd.none
            )

        GoToSolarSystemPage hexId ->
            ( model
            , Browser.Navigation.pushUrl model.key <| "/view_system?hexid=" ++ hexId.raw
            )
