module Traveller exposing (Model, Msg(..), init, subscriptions, update, view)

--import Traveller.HexId as HexId exposing (HexId, RawHexId)

import Browser.Dom
import Browser.Events
import Browser.Navigation
import Codec
import Color
import Color.Manipulate
import Css
import Dict
import Element
    exposing
        ( Element
        , centerX
        , column
        , el
        , rgb
        , row
        , text
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Html as UnstyledHtml
import Html.Events.Extra.Mouse
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes
import Http
import Json.Decode as JsDecode
import Parser
import RemoteData exposing (RemoteData(..))
import Round
import Svg.Styled as Svg exposing (Svg)
import Svg.Styled.Attributes as SvgAttrs exposing (points, viewBox)
import Svg.Styled.Events as SvgEvents
import Task
import Traveller.HexAddress as HexAddress exposing (HexAddress)
import Traveller.Parser as TravellerParser
import Traveller.Point exposing (StellarPoint)
import Traveller.SolarSystem as SolarSystem exposing (SolarSystem, SolarSystemDict)
import Traveller.StellarObject exposing (GasGiantData, InnerStarData, PlanetoidBeltData, PlanetoidData, StarData(..), StellarObject(..), TerrestrialData, getInnerStarData, getStellarOrbit, starColourRGB)
import Url.Builder


gasGiantSI =
    5


terrestrialSI =
    6


planetoidSI =
    6


type DragMode
    = IsDragging ( Float, Float )
    | NoDragging


type alias Model =
    { key : Browser.Navigation.Key
    , hexScale : Float
    , solarSystems : RemoteData Http.Error SolarSystemDict
    , dragMode : DragMode
    , playerHex : HexAddress
    , hoveringHex : Maybe HexAddress
    , sidebarSystemAndSI : Maybe ( HexAddress, Int )
    , viewport : Maybe Browser.Dom.Viewport
    , hexmapViewport : Maybe (Result Browser.Dom.Error Browser.Dom.Viewport)
    , selectedStellarObject : Maybe StellarObject
    , upperLeftHex : HexAddress
    , lowerRightHex : HexAddress
    }


type Msg
    = NoOpMsg
    | ZoomScaleChanged Float
    | DownloadSolarSystems
    | DownloadedSolarSystems (Result Http.Error (List SolarSystem))
    | HoveringHex HexAddress
    | ViewingHex ( HexAddress, Int )
    | GotViewport Browser.Dom.Viewport
    | GotHexMapViewport (Result Browser.Dom.Error Browser.Dom.Viewport)
    | GotResize Int Int
    | GoToSolarSystemPage HexAddress
    | FocusInSidebar StellarObject
    | MapMouseDown ( Float, Float )
    | MapMouseUp
    | MapMouseMove ( Float, Float )


type alias HexOrigin =
    ( Int, Int )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize GotResize


init : Browser.Navigation.Key -> ( Model, Cmd Msg )
init key =
    let
        model : Model
        model =
            { hexScale = defaultHexSize
            , solarSystems =
                --Loading, because we're sending the solar system request
                RemoteData.Loading
            , dragMode = NoDragging
            , playerHex = { sectorX = 1, sectorY = 1, x = 1, y = 1 }
            , hoveringHex = Nothing
            , sidebarSystemAndSI = Nothing
            , viewport = Nothing
            , hexmapViewport = Nothing
            , key = key
            , selectedStellarObject = Nothing
            , upperLeftHex =
                { sectorX = -10
                , sectorY = -2
                , x = 22
                , y = 13
                }
            , lowerRightHex =
                { sectorX = -10
                , sectorY = -2
                , x = 32
                , y = 24
                }
            }
    in
    ( model
    , Cmd.batch
        [ sendSolarSystemRequest model.upperLeftHex model.lowerRightHex
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


isStarOrbit : StellarObject -> Bool
isStarOrbit obj =
    case obj of
        GasGiant gasGiantData ->
            gasGiantData.orbitType < 10

        TerrestrialPlanet terrestrialData ->
            terrestrialData.orbitType < 10

        PlanetoidBelt planetoidBeltData ->
            planetoidBeltData.orbitType < 10

        Planetoid planetoidData ->
            planetoidData.orbitType < 10

        Star (StarDataWrap starDataConfig) ->
            starDataConfig.orbitType < 10


viewHexDetailed : Maybe SolarSystem -> HexAddress -> HexAddress -> HexOrigin -> Float -> Svg Msg
viewHexDetailed maybeSolarSystem _ hexAddress (( x, y ) as origin) size =
    let
        si =
            case maybeSolarSystem of
                Just solarSystem ->
                    solarSystem.surveyIndex

                Nothing ->
                    0

        hasStar =
            case maybeSolarSystem of
                Just solarSystem ->
                    solarSystem.surveyIndex > 0

                Nothing ->
                    False

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

        -- a decoder that takes JSON and emits either a decode failure or a Msg
        downDecoder : JsDecode.Decoder Msg
        downDecoder =
            let
                -- takes a raw JS mouse event and turns it into a parsed Elm mouse event
                jsMouseEventDecoder =
                    Html.Events.Extra.Mouse.eventDecoder

                -- takes an Elm Mouse event and creates our Msg
                msgConstructor evt =
                    MapMouseDown <| evt.offsetPos
            in
            -- run the mouse event decoder
            jsMouseEventDecoder
                |> -- then if that succeeds, pass the event object into msgConstructor
                   JsDecode.map msgConstructor

        moveDecoder =
            -- equivalent to the `downDecoder`, only it returns `MapMouseMove` instead
            JsDecode.map (\evt -> MapMouseMove <| evt.offsetPos) Html.Events.Extra.Mouse.eventDecoder
    in
    Svg.g
        [ SvgEvents.onMouseOver (HoveringHex hexAddress)
        , SvgEvents.onClick (ViewingHex ( hexAddress, si ))
        , SvgEvents.onMouseUp MapMouseUp
        , -- listens for the JS 'mousedown' event and then runs the `downDecoder` on the JS Event, returning the Msg
          SvgEvents.on "mousedown" downDecoder
        , SvgEvents.on "mousemove" moveDecoder
        , SvgAttrs.style "cursor: pointer;"
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
            drawStar : ( Float, Float ) -> Int -> InnerStarData -> Svg Msg
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
          case ( maybeSolarSystem, hasStar ) of
            ( Just solarSystem, True ) ->
                --(StarData primaryStar) :: stars ->
                let
                    primaryPos =
                        ( toFloat x, toFloat y )

                    primaryStar =
                        getInnerStarData solarSystem.primaryStar

                    generateStar : Int -> StellarObject -> Svg Msg
                    generateStar idx stellarObject =
                        case stellarObject of
                            Star (StarDataWrap star) ->
                                let
                                    secondaryStarPos =
                                        rotatePoint idx primaryPos 60 20
                                in
                                case star.companion of
                                    Just (StarDataWrap compStarData) ->
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
                in
                Svg.g
                    []
                    ((case primaryStar.companion of
                        Just (StarDataWrap compStarData) ->
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
                        :: (primaryStar.stellarObjects
                                |> List.filter isStarOrbit
                                |> List.indexedMap generateStar
                           )
                    )

            _ ->
                Svg.text_
                    [ SvgAttrs.x <| String.fromInt <| x
                    , SvgAttrs.y <| String.fromInt <| y - (floor <| size * 0.65)
                    , SvgAttrs.fontSize "10"
                    , SvgAttrs.textAnchor "middle"
                    ]
                    [ HexAddress.hexLabel hexAddress |> Svg.text
                    ]
        , case ( maybeSolarSystem, hasStar ) of
            ( Just solarSystem, True ) ->
                Svg.g []
                    [ -- hex index
                      Svg.text_
                        [ SvgAttrs.x <| String.fromInt <| x
                        , SvgAttrs.y <| String.fromInt <| y - (floor <| size * 0.65)
                        , SvgAttrs.fontSize "10"
                        , SvgAttrs.textAnchor "middle"
                        ]
                        [ HexAddress.hexLabel hexAddress |> Svg.text
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

            ( Just solarSystem, False ) ->
                Svg.text ""

            ( Nothing, True ) ->
                Svg.text ""

            ( Nothing, False ) ->
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
    -> SolarSystemDict
    -> ( Float, Float )
    -> HexAddress
    -> HexOrigin
    -> HexAddress
    -> ( Maybe (Svg Msg), Int )
viewHex widestViewport hexSize solarSystemDict ( viewportWidth, viewportHeight ) hexAddress ( ox, oy ) playerHexId =
    let
        -- idx =
        --     rowIdx + colIdx * 100
        --
        ( fox, foy ) =
            ( toFloat ox, toFloat oy )

        outsideX =
            let
                plus =
                    fox + hexSize

                minus =
                    fox - hexSize
            in
            (plus < 0) || (minus > widestViewport.viewport.width)

        outsideY =
            let
                plus =
                    foy + hexSize

                minus =
                    foy - hexSize
            in
            (plus < 0) || (minus > widestViewport.viewport.height)

        solarSystem =
            Dict.get (HexAddress.toKey hexAddress) solarSystemDict

        hexSVG =
            if not (outsideX || outsideY) then
                Just
                    (viewHexDetailed solarSystem
                        playerHexId
                        hexAddress
                        ( ox, oy )
                        hexSize
                    )

            else
                Nothing
    in
    ( hexSVG, isEmptyHex solarSystem )


{-| View all the hexes in the system
-}
viewHexes :
    HexAddress
    -> { screenVp : Browser.Dom.Viewport, hexmapVp : Maybe Browser.Dom.Viewport }
    -> SolarSystemDict
    -> HexAddress
    -> Float
    -> Html Msg
viewHexes upperLeftHex { screenVp, hexmapVp } solarSystemDict playerHexId hexSize =
    let
        viewportHeightIsh =
            screenVp.viewport.height * 0.9

        viewportWidthIsh =
            min (screenVp.viewport.width * 0.9)
                (screenVp.viewport.width - 500.0)

        xOffset =
            -- view horizontal offset
            "0"

        yOffset =
            -- view vertical offset
            "0"

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
                        let
                            hexAddress : HexAddress
                            hexAddress =
                                { upperLeftHex | y = rowIdx + upperLeftHex.y, x = colIdx + upperLeftHex.x }
                        in
                        viewHex
                            widestViewport
                            hexSize
                            solarSystemDict
                            ( viewportWidthIsh, viewportHeightIsh )
                            hexAddress
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
                        ++ " ); user-select: none;"
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


{-| Builds a monospace text element
-}
monospaceText : String -> Element.Element msg
monospaceText someString =
    text someString |> el [ Font.family [ Font.monospace ] ]


renderGasGiant : Int -> GasGiantData -> Maybe StellarObject -> Element.Element Msg
renderGasGiant newNestingLevel gasGiantData selectedStellarObject =
    let
        planet =
            GasGiant gasGiantData
    in
    row
        [ Element.spacing 8
        , Element.moveRight <| calcNestedOffset newNestingLevel
        , Font.size 14
        , Element.Events.onClick <| FocusInSidebar planet
        ]
        [ text <| renderArrow planet selectedStellarObject
        , renderRawOrbit gasGiantData.au
        , text gasGiantData.orbitSequence
        , text gasGiantData.code
        , text "🛢"
        , text <| "j: " ++ gasGiantData.safeJumpTime
        , text <| renderTravelTime planet selectedStellarObject
        ]


renderArrow : StellarObject -> Maybe StellarObject -> String
renderArrow self selected =
    if Just self == selected then
        ">"

    else
        ""


renderTravelTime : StellarObject -> Maybe StellarObject -> String
renderTravelTime destination origin =
    let
        playerGravDrive =
            4

        travelTimeStr =
            case origin of
                Just obj ->
                    if obj /= destination then
                        let
                            destPosition =
                                (getStellarOrbit destination).orbitPosition

                            objPosition =
                                (getStellarOrbit obj).orbitPosition

                            dist =
                                calcDistance2F destPosition objPosition
                        in
                        travelTime dist playerGravDrive False

                    else
                        ""

                Nothing ->
                    ""
    in
    travelTimeStr


renderTerrestrialPlanet : Int -> TerrestrialData -> Maybe StellarObject -> Element.Element Msg
renderTerrestrialPlanet newNestingLevel terrestrialData selectedStellarObject =
    let
        planet =
            TerrestrialPlanet terrestrialData
    in
    row
        [ Element.spacing 8
        , Element.moveRight <| calcNestedOffset newNestingLevel
        , Font.size 14
        , Element.Events.onClick <| FocusInSidebar planet
        ]
        [ text <| renderArrow planet selectedStellarObject
        , renderRawOrbit terrestrialData.au
        , text terrestrialData.orbitSequence
        , let
            rawUwp =
                terrestrialData.uwp
          in
          case Parser.run TravellerParser.uwp rawUwp of
            Ok uwpData ->
                column []
                    [ monospaceText <| rawUwp
                    ]

            Err _ ->
                monospaceText <| rawUwp
        , text "🌍"
        , text <| "j: " ++ terrestrialData.safeJumpTime
        , text <|
            renderTravelTime planet selectedStellarObject
        ]


calcNestedOffset : Int -> Float
calcNestedOffset newNestingLevel =
    toFloat <| newNestingLevel * 10


renderRawOrbit : Float -> Element.Element msg
renderRawOrbit au =
    row []
        [ monospaceText <| Round.round 2 au
        ]



{-
   const travelTime = (kms, mdrive, useHours) => {
     const seconds = 2 * Math.sqrt(kms1000/(mdrive9.8));
     if (useHours) {
       let minutes = Math.ceil(seconds / 60);
       let hours = Math.floor(minutes/60);
       minutes -= hours60;
       if (hours > 0)
         return ${hours}h ${minutes}m;
       else
         return ${minutes}m;
     } else {
       let watches = Math.ceil(seconds/(60608));
       let days = Math.floor(watches/3);
       watches -= days3
       return ${days}d ${watches}w;
     }
   }
-}


travelTime : Float -> Int -> Bool -> String
travelTime kms mdrive useHours =
    let
        rawSeconds =
            2 * sqrt (kms * 1000 / (toFloat mdrive * 9.8))
    in
    if useHours then
        let
            rawMinutes =
                toFloat <| ceiling <| rawSeconds / 60

            hours =
                toFloat <| floor <| rawMinutes / 60

            minutes =
                rawMinutes - hours * 60
        in
        if hours > 0 then
            Round.floor 0 hours ++ "h " ++ Round.ceiling 0 minutes ++ "m"

        else
            Round.ceiling 0 minutes ++ "m"

    else
        let
            watches =
                toFloat <| ceiling <| rawSeconds / 60608

            days =
                toFloat <| floor <| watches / 3

            watches_ =
                watches - days * 3
        in
        Round.floor 0 days ++ "d " ++ Round.ceiling 0 watches_ ++ "w"



-- ( watches_, days )
{-
   const calculateDistance = (x1, y1, x2, y2) => {
     const deltaX = x2 - x1;
     const deltaY = y2 - y1;

     const distanceSquared = deltaX * deltaX + deltaY * deltaY;

     const distance = Math.sqrt(distanceSquared);

     return distance;
   }

-}


calcDistance2F : StellarPoint -> StellarPoint -> Float
calcDistance2F p1 p2 =
    let
        deltaX =
            p1.x - p2.x

        deltaY =
            p1.y - p2.y

        distanceSquared =
            deltaX * deltaX + deltaY * deltaY

        distance =
            sqrt distanceSquared
    in
    distance


renderPlanetoidBelt : Int -> PlanetoidBeltData -> Maybe StellarObject -> Element.Element Msg
renderPlanetoidBelt newNestingLevel planetoidBeltData selectedStellarObject =
    let
        belt =
            PlanetoidBelt planetoidBeltData
    in
    row
        [ Element.spacing 8
        , Element.moveRight <| calcNestedOffset newNestingLevel
        , Font.size 14
        , Element.Events.onClick <| FocusInSidebar belt
        ]
        [ text <| renderArrow belt selectedStellarObject
        , renderRawOrbit planetoidBeltData.au
        , text planetoidBeltData.orbitSequence
        , let
            rawUwp =
                planetoidBeltData.uwp
          in
          case Parser.run TravellerParser.uwp rawUwp of
            Ok uwpData ->
                column [] [ monospaceText <| rawUwp ]

            Err _ ->
                monospaceText <| rawUwp
        , text "🗿"
        , text <| "j: " ++ planetoidBeltData.safeJumpTime
        , text <| renderTravelTime belt selectedStellarObject
        ]


renderPlanetoid : Int -> PlanetoidData -> Maybe StellarObject -> Element.Element Msg
renderPlanetoid newNestingLevel planetoidData selectedStellarObject =
    let
        planet =
            Planetoid planetoidData
    in
    row
        [ Element.spacing 8
        , Element.moveRight <| calcNestedOffset newNestingLevel
        , Font.size 14
        , Element.Events.onClick <| FocusInSidebar planet
        ]
        [ text <| renderArrow planet selectedStellarObject
        , renderRawOrbit planetoidData.au
        , let
            rawUwp =
                planetoidData.uwp
          in
          case Parser.run TravellerParser.uwp rawUwp of
            Ok uwpData ->
                column [] [ monospaceText <| planetoidData.uwp ]

            Err _ ->
                monospaceText <| rawUwp
        , text planetoidData.orbitSequence
        , text "🌎"
        , text <| "j: " ++ planetoidData.safeJumpTime
        , text <| renderTravelTime planet selectedStellarObject
        ]


convertColor : Color.Color -> Element.Color
convertColor color =
    Element.fromRgb <| Color.toRgba <| color


renderStellarObject : ( Float, Float ) -> Int -> StellarObject -> Maybe StellarObject -> Element.Element Msg
renderStellarObject comparePos newNestingLevel stellarObject selectedStellarObject =
    row
        [ Element.spacing 8
        , Font.size 14
        , Element.width Element.fill
        ]
        [ case stellarObject of
            GasGiant gasGiantData ->
                renderGasGiant newNestingLevel gasGiantData selectedStellarObject

            TerrestrialPlanet terrestrialData ->
                renderTerrestrialPlanet newNestingLevel terrestrialData selectedStellarObject

            PlanetoidBelt planetoidBeltData ->
                renderPlanetoidBelt newNestingLevel planetoidBeltData selectedStellarObject

            Planetoid planetoidData ->
                renderPlanetoid newNestingLevel planetoidData selectedStellarObject

            Star starDataConfig ->
                el [ Element.paddingEach { top = 0, left = 0, right = 0, bottom = 5 } ] <| renderStar comparePos starDataConfig newNestingLevel selectedStellarObject
        ]


renderStar : ( Float, Float ) -> StarData -> Int -> Maybe StellarObject -> Element.Element Msg
renderStar comparePos (StarDataWrap starData) nestingLevel selectedStellarObject =
    let
        inJumpShadow obj =
            case starData.jumpShadow of
                Just jumpShadow ->
                    jumpShadow >= (getStellarOrbit obj).au

                Nothing ->
                    False

        nextNestingLevel =
            nestingLevel + 1
    in
    column
        [ Color.rgba (33 / 255.0) (37 / 255.0) (41 / 255.0) 0.15
            |> Color.Manipulate.darken 0.4
            |> convertColor
            |> Background.color
        , Element.width Element.fill
        , Element.moveRight <| toFloat <| nestingLevel * 5

        -- , Element.paddingEach {top=10, left=5, right=5, bottom=10}
        , Element.padding 10
        , Border.rounded 10
        ]
        [ row []
            [ if starData.orbitPosition.x == 0 && starData.orbitPosition.y == 0 then
                text <| ""

              else
                renderRawOrbit starData.au
            , el [ Font.size 16, Font.bold ] <|
                text <|
                    starData.stellarType
                        ++ (case starData.subtype of
                                Just num ->
                                    String.fromInt num

                                Nothing ->
                                    ""
                           )
                        ++ " "
                        ++ starData.stellarClass
            ]
        , starData.companion
            |> Maybe.map
                (\compStarData ->
                    renderStar comparePos compStarData nextNestingLevel selectedStellarObject
                )
            |> Maybe.withDefault Element.none
        , column []
            [ starData.stellarObjects
                |> List.filter inJumpShadow
                |> List.map (\so -> renderStellarObject comparePos nextNestingLevel so selectedStellarObject)
                |> column []
            , -- jump shadow
              column [ Font.size 14, Font.bold, Element.centerX ]
                [ case starData.jumpShadow of
                    Just jumpShadow ->
                        text <| "----  " ++ Round.round 2 jumpShadow ++ "  ----"

                    Nothing ->
                        text ""
                ]
            , starData.stellarObjects
                |> List.filter (not << inJumpShadow)
                |> List.map (\so -> renderStellarObject comparePos nextNestingLevel so selectedStellarObject)
                |> column []
            ]
        ]


addressToString : SolarSystem -> String
addressToString solarSystem =
    solarSystem.address
        |> (\{ x, y } ->
                (String.fromInt x |> String.padLeft 2 '0')
                    ++ (String.fromInt y |> String.padLeft 2 '0')
           )


renderSimpleStellarObject : StellarObject -> Element.Element Msg
renderSimpleStellarObject stellarObject =
    case stellarObject of
        GasGiant gasGiantData ->
            text <| "Gas Giant: " ++ gasGiantData.safeJumpTime

        TerrestrialPlanet terrestrialData ->
            text <| "Terrestrial: " ++ terrestrialData.safeJumpTime

        PlanetoidBelt planetoidBeltData ->
            text <| "Planetoid Belt: " ++ planetoidBeltData.safeJumpTime

        Planetoid planetoidData ->
            text <| "Planetoid: " ++ planetoidData.safeJumpTime

        Star (StarDataWrap starDataConfig) ->
            text <| "Star: " ++ starDataConfig.safeJumpTime


renderOrbit : StellarObject -> Element.Element Msg
renderOrbit stellarObject =
    case stellarObject of
        GasGiant gasGiantData ->
            text <| Round.round 2 gasGiantData.au

        TerrestrialPlanet terrestrialData ->
            text <| Round.round 2 terrestrialData.au

        PlanetoidBelt planetoidBeltData ->
            text <| Round.round 2 planetoidBeltData.au

        Planetoid planetoidData ->
            text <| Round.round 2 planetoidData.au

        Star (StarDataWrap starDataConfig) ->
            text <| Round.round 2 starDataConfig.au


renderSequence : StellarObject -> Element.Element Msg
renderSequence stellarObject =
    case stellarObject of
        GasGiant gasGiantData ->
            text gasGiantData.orbitSequence

        TerrestrialPlanet terrestrialData ->
            text terrestrialData.orbitSequence

        PlanetoidBelt planetoidBeltData ->
            text planetoidBeltData.orbitSequence

        Planetoid planetoidData ->
            text planetoidData.orbitSequence

        Star (StarDataWrap starDataConfig) ->
            text starDataConfig.orbitSequence


renderSafeJump : StellarObject -> Element.Element Msg
renderSafeJump stellarObject =
    case stellarObject of
        GasGiant gasGiantData ->
            text <| "j: " ++ gasGiantData.safeJumpTime

        TerrestrialPlanet terrestrialData ->
            text <| "j: " ++ terrestrialData.safeJumpTime

        PlanetoidBelt planetoidBeltData ->
            text <| "j: " ++ planetoidBeltData.safeJumpTime

        Planetoid planetoidData ->
            text <| "j: " ++ planetoidData.safeJumpTime

        Star (StarDataWrap starDataConfig) ->
            text <| "j: " ++ starDataConfig.safeJumpTime


renderDescription : StellarObject -> Element.Element Msg
renderDescription stellarObject =
    case stellarObject of
        GasGiant gasGiantData ->
            text gasGiantData.code

        TerrestrialPlanet terrestrialData ->
            text terrestrialData.uwp

        PlanetoidBelt planetoidBeltData ->
            text planetoidBeltData.uwp

        Planetoid planetoidData ->
            text planetoidData.uwp

        Star (StarDataWrap starDataConfig) ->
            text <|
                starDataConfig.stellarType
                    ++ (case starDataConfig.subtype of
                            Just num ->
                                String.fromInt num

                            Nothing ->
                                ""
                       )
                    ++ " "
                    ++ starDataConfig.stellarClass


viewSystemDetailsSidebar : ( HexAddress, Int ) -> SolarSystem -> Maybe StellarObject -> Element Msg
viewSystemDetailsSidebar ( viewingHexId, si ) solarSystem selectedStellarObject =
    let
        primaryStar : StarData
        primaryStar =
            solarSystem.primaryStar

        starDataConfig =
            getInnerStarData primaryStar

        stellarObjects =
            starDataConfig.stellarObjects
    in
    column [ Element.spacing 10 ] <|
        [ -- render the nested chart of the system
          text <| solarSystem.sectorName ++ " " ++ addressToString solarSystem
        , let
            table =
                Element.table []
                    { data = stellarObjects
                    , columns =
                        [ { header = text ""
                          , width = Element.fill
                          , view = renderOrbit
                          }
                        , { header = text ""
                          , width = Element.fill
                          , view = renderSequence
                          }
                        , { header = text ""
                          , width = Element.fill
                          , view = renderDescription
                          }
                        , { header = text ""
                          , width = Element.fill
                          , view = always (text "col")
                          }
                        , { header = text ""
                          , width = Element.fill
                          , view = renderSafeJump
                          }
                        , { header = text ""
                          , width = Element.fill
                          , view = always (text "col")
                          }
                        ]
                    }
          in
          table
        , let
            comparePos =
                ( 0, 0 )
          in
          renderStar comparePos solarSystem.primaryStar 0 selectedStellarObject
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
            , label = text <| "Visualize Solar System: " ++ HexAddress.toKey viewingHexId
            }
        ]



--calcDistance : HexId -> HexId -> Int
--calcDistance hex1 hex2 =
--    let
--        ( y1, x1 ) =
--            HexId.toRowCol hex1
--
--        ( y2, x2 ) =
--            HexId.toRowCol hex2
--
--        rowDiff =
--            abs (y1 - y2)
--
--        colDiff =
--            abs (x1 - x2)
--    in
--    max rowDiff colDiff + (min rowDiff colDiff // 2)


view : Model -> Element.Element Msg
view model =
    let
        sidebarColumn =
            column [ centerX, Element.width <| Element.px 400 ]
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
                , column
                    [ Font.size 14
                    , Font.color <| Element.rgb 0.5 1.5 0.5
                    ]
                  <|
                    [ column [ Element.spacing 15 ]
                        [ row []
                            [ text "Revelation location:"
                            , text <| HexAddress.toKey model.playerHex
                            ]

                        --, case model.hoveringHex of
                        --    Just hoveringHex ->
                        --        column []
                        --            [ text "Hovering HexId:"
                        --            , text <| String.fromInt hoveringHex.value
                        --            , text "distance to player hex"
                        --            , text <| String.fromInt <| calcDistance model.playerHex hoveringHex
                        --            ]
                        --
                        --    Nothing ->
                        --        text <| "None yet"
                        ]
                    ]
                , case model.solarSystems of
                    Success solarSystemDict ->
                        case model.sidebarSystemAndSI of
                            Just ( viewingAddress, si ) ->
                                case solarSystemDict |> Dict.get (HexAddress.toKey viewingAddress) of
                                    Just solarSystem ->
                                        viewSystemDetailsSidebar
                                            ( viewingAddress, si )
                                            solarSystem
                                            model.selectedStellarObject

                                    Nothing ->
                                        text "No solar system data found in dict"

                            Nothing ->
                                text "No viewing hex data yet"

                    _ ->
                        text "No loaded sector data yet"
                ]

        hexesColumn =
            let
                renderError : String -> UnstyledHtml.Html msg
                renderError txt =
                    Html.toUnstyled <|
                        Html.div [ Html.Styled.Attributes.css [ Css.color (Css.hex "#ff0000") ] ]
                            [ Html.text txt ]
            in
            column []
                [ Element.html <|
                    -- Note: we use elm-css for type-safe CSS, so we need to use the Html.Styled.* dropins for Html.
                    case ( model.solarSystems, model.viewport ) of
                        ( RemoteData.Success solarSystems, Just viewport ) ->
                            viewHexes
                                model.upperLeftHex
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
                                solarSystems
                                model.playerHex
                                model.hexScale
                                |> Html.toUnstyled

                        ( RemoteData.Failure (Http.BadBody jsonErr), _ ) ->
                            renderError jsonErr

                        ( RemoteData.Failure (Http.BadUrl url), _ ) ->
                            renderError <| "Invalid URL: " ++ url

                        ( RemoteData.Failure Http.NetworkError, _ ) ->
                            renderError "Network Error"

                        ( RemoteData.Failure (Http.BadStatus statusCode), _ ) ->
                            renderError <| "BadStatus: " ++ String.fromInt statusCode

                        ( RemoteData.Failure Http.Timeout, _ ) ->
                            renderError "Request timedout"

                        ( NotAsked, _ ) ->
                            Html.toUnstyled <|
                                Html.div [ Html.Styled.Attributes.css [ Css.color (Css.rgb 100 100 100) ] ]
                                    [ Html.text "Not yet asked" ]

                        ( Loading, _ ) ->
                            Html.toUnstyled <| Html.text "Loading..."

                        ( Success _, Nothing ) ->
                            Html.toUnstyled <| Html.text "Have sector data but no viewport"
                ]
    in
    column []
        [ row [ Font.size 20, Font.color <| Element.rgb 0.5 1.5 0.5 ]
            [ sidebarColumn
            , hexesColumn
            ]
        , -- displaying json errors for SectorData
          case model.solarSystems of
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
        ]


sendSolarSystemRequest : HexAddress -> HexAddress -> Cmd Msg
sendSolarSystemRequest upperLeft lowerRight =
    let
        solarSystemsDecoder : JsDecode.Decoder (List SolarSystem)
        solarSystemsDecoder =
            Codec.list SolarSystem.codec
                |> Codec.decoder

        url =
            Url.Builder.crossOrigin "https://radiofreewaba.net"
                [ "deepnight", "data", "solarsystems" ]
                [ Url.Builder.int "ulsx" upperLeft.sectorX
                , Url.Builder.int "ulsy" upperLeft.sectorY
                , Url.Builder.int "ulhx" upperLeft.x
                , Url.Builder.int "ulhy" upperLeft.y
                , Url.Builder.int "lrsx" lowerRight.sectorX
                , Url.Builder.int "lrsy" lowerRight.sectorY
                , Url.Builder.int "lrhx" lowerRight.x
                , Url.Builder.int "lrhy" lowerRight.y
                ]

        requestCmd =
            -- Http.get
            --     { url = url
            --     , expect = Http.expectJson DownloadedSolarSystems solarSystemsParser
            --     }
            -- using Http.request to set a timeout
            Http.request
                { method = "GET"
                , headers = []
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectJson DownloadedSolarSystems solarSystemsDecoder
                , timeout = Just 5000
                , tracker = Nothing
                }
    in
    requestCmd


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOpMsg ->
            ( model, Cmd.none )

        ZoomScaleChanged newScale ->
            ( { model | hexScale = newScale }, Cmd.none )

        DownloadSolarSystems ->
            ( model
            , sendSolarSystemRequest model.upperLeftHex model.lowerRightHex
            )

        DownloadedSolarSystems (Ok solarSystems) ->
            let
                sortedSolarSystems =
                    solarSystems |> List.sortBy (HexAddress.toKey << .address)

                solarSystemDict =
                    sortedSolarSystems
                        |> List.map (\system -> ( HexAddress.toKey system.address, system ))
                        |> Dict.fromList
            in
            ( { model | solarSystems = solarSystemDict |> RemoteData.Success }
            , Cmd.batch
                [ Browser.Dom.getViewportOf "hexmap"
                    |> Task.attempt GotHexMapViewport
                ]
            )

        DownloadedSolarSystems (Err err) ->
            ( { model | solarSystems = RemoteData.Failure err }, Cmd.none )

        HoveringHex hoveringHex ->
            ( { model | hoveringHex = Just hoveringHex }, Cmd.none )

        GotViewport viewport ->
            ( { model | viewport = Just viewport }
            , Browser.Dom.getViewportOf "hexmap"
                |> Task.attempt GotHexMapViewport
            )

        GotHexMapViewport hexmapOrErr ->
            ( { model | hexmapViewport = Just hexmapOrErr }, Cmd.none )

        GotResize width height ->
            ( model
            , Cmd.batch
                [ Browser.Dom.getViewport
                    |> Task.perform GotViewport
                , Browser.Dom.getViewportOf "hexmap"
                    |> Task.attempt GotHexMapViewport
                ]
            )

        ViewingHex ( hexAddress, si ) ->
            ( { model
                | sidebarSystemAndSI = Just ( hexAddress, si )
                , selectedStellarObject = Nothing
              }
            , Cmd.none
            )

        GoToSolarSystemPage hexAddress ->
            ( model
            , Browser.Navigation.pushUrl model.key <| "/view_system?hexid=" ++ "FIXME"
            )

        FocusInSidebar stellarObject ->
            ( { model | selectedStellarObject = Just stellarObject }
            , Cmd.none
            )

        MapMouseDown ( x, y ) ->
            ( { model
                | dragMode = Debug.log "mouseDown" <| IsDragging ( x, y )
              }
            , Cmd.none
            )

        MapMouseUp ->
            ( { model
                | dragMode = Debug.log "mouseUp" NoDragging
              }
            , sendSolarSystemRequest model.upperLeftHex model.lowerRightHex
            )

        MapMouseMove ( newX, newY ) ->
            case model.dragMode of
                IsDragging ( originX, originY ) ->
                    let
                        xDelta =
                            truncate <| (originX - newX) / model.hexScale

                        yDelta =
                            truncate <| (originY - newY) / model.hexScale

                        shiftAddress : HexAddress -> HexAddress
                        shiftAddress addr =
                            let
                                newXOffset =
                                    HexAddress.add { hexVal = addr.x, max = numHexCols, delta = xDelta }

                                newYOffset =
                                    HexAddress.add { hexVal = addr.y, max = numHexRows, delta = yDelta }
                            in
                            { sectorX = addr.sectorX + newXOffset.sectorDelta
                            , sectorY = addr.sectorY - newYOffset.sectorDelta
                            , x = newXOffset.hexVal
                            , y = newYOffset.hexVal
                            }

                        _ =
                            Debug.log "mouseMove" ( ( xDelta, yDelta ), ( newX, newY ) )

                        newModel =
                            { model
                                | dragMode = IsDragging ( newX, newY )
                                , upperLeftHex = shiftAddress model.upperLeftHex
                                , lowerRightHex = shiftAddress model.lowerRightHex
                            }
                    in
                    if xDelta /= 0 || yDelta /= 0 then
                        ( newModel
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                NoDragging ->
                    ( model, Cmd.none )
