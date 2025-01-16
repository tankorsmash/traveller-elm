module Traveller exposing (Model, Msg(..), init, numHexCols, numHexRows, subscriptions, update, view)

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
        , centerY
        , column
        , el
        , fill
        , height
        , row
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import FontAwesome as Icon exposing (Icon)
import FontAwesome.Attributes as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import HostConfig exposing (HostConfig)
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
import Traveller.HexAddress as HexAddress exposing (HexAddress, SectorHexAddress, hexLabel, sectorColumns, sectorRows, toSectorKey, toUniversalAddress, universalHexX, universalHexY, universalToSector)
import Traveller.Parser as TravellerParser
import Traveller.Point exposing (StellarPoint)
import Traveller.Sector exposing (Sector, SectorDict, codecSector, sectorKey)
import Traveller.SolarSystem as SolarSystem exposing (SolarSystem)
import Traveller.SolarSystemStars exposing (StarSystem, StarType, StarTypeData, getStarTypeData, starSystemCodec)
import Traveller.StarColour exposing (starColourRGB)
import Traveller.StellarObject
    exposing
        ( GasGiantData
        , InnerStarData
        , PlanetoidBeltData
        , PlanetoidData
        , StarData(..)
        , StellarObject(..)
        , TerrestrialData
        , getInnerStarData
        , getSafeJumpTime
        , getStellarOrbit
        )
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


{-| RequestNum is a unique identifier for a request.

'Mk' is a prefix meaning 'make', to distinguishg it from the RequestNum parent,
even though they could be the same name.

-}
type RequestNum
    = MkRequestNum Int


{-| RequestEntry is a record of a request made to the server.

Each time we call sendSolarSystemRequest we prep a RequestEntry and store it in
the Model.

Then when we get a response back, we mark the request as complete and update
our model, using `markRequestComplete`.

-}
type alias RequestEntry =
    { requestNum : RequestNum
    , upperLeftHex : HexAddress
    , lowerRightHex : HexAddress
    , status :
        RemoteData
            Http.Error
            -- no data is kept beyond pass/fail. '()' means 'unit' or 'void' in other languages
            ()
    }


type alias RequestHistory =
    List RequestEntry


nextRequestNum : RequestHistory -> RequestNum
nextRequestNum requestHistory =
    let
        lastRequestNum : Int
        lastRequestNum =
            case requestHistory of
                [] ->
                    0

                requests ->
                    requests
                        |> List.map (.requestNum >> (\(MkRequestNum x) -> x))
                        |> List.maximum
                        |> Maybe.withDefault 0
    in
    MkRequestNum (lastRequestNum + 1)


{-| Builds a RequestEntry and updates the existing History with it.

This is so we have less chance of getting the history out of sync with the
entries, because this is the only way to construct a RequestEntry.

-}
prepNextRequest : ( SolarSystemDict, RequestHistory ) -> HexAddress -> HexAddress -> ( RequestEntry, ( SolarSystemDict, RequestHistory ) )
prepNextRequest ( oldSolarSystemDict, requestHistory ) upperLeftHex lowerRightHex =
    let
        requestNum =
            nextRequestNum requestHistory

        requestEntry =
            { requestNum = requestNum
            , upperLeftHex = upperLeftHex
            , lowerRightHex = lowerRightHex
            , status = RemoteData.Loading
            }

        hexRange =
            HexAddress.between upperLeftHex lowerRightHex

        newSolarSystemDict =
            hexRange
                |> List.foldl
                    (\hexAddr ssDict ->
                        Dict.update (HexAddress.toKey hexAddr)
                            (\maybeSolarSystem ->
                                case maybeSolarSystem of
                                    Just existingSolarSystem ->
                                        Just existingSolarSystem

                                    Nothing ->
                                        Just LoadingSolarSystem
                            )
                            ssDict
                    )
                    oldSolarSystemDict
    in
    ( requestEntry, ( newSolarSystemDict, requestEntry :: requestHistory ) )


type alias Model =
    { key : Browser.Navigation.Key
    , hexScale : Float
    , solarSystems : SolarSystemDict
    , lastSolarSystemError : Maybe Http.Error
    , requestHistory : RequestHistory
    , dragMode : DragMode
    , sectors : SectorDict
    , playerHex : HexAddress
    , hoveringHex : Maybe HexAddress
    , selectedHex : Maybe HexAddress
    , selectedSystem : Maybe SolarSystem
    , sidebarHoverText : Maybe String
    , viewport : Maybe Browser.Dom.Viewport
    , hexmapViewport : Maybe (Result Browser.Dom.Error Browser.Dom.Viewport)
    , selectedStellarObject : Maybe StellarObject
    , upperLeftHex : HexAddress
    , lowerRightHex : HexAddress
    , hostConfig : HostConfig.HostConfig
    }


type Msg
    = NoOpMsg
    | ZoomScaleChanged Float
    | DownloadSolarSystems
    | DownloadedSolarSystems RequestEntry (Result Http.Error (List StarSystem))
    | FetchedSolarSystem (Result Http.Error SolarSystem)
    | DownloadedSectors RequestEntry (Result Http.Error (List Sector))
    | HoveringHex HexAddress
    | ViewingHex HexAddress
    | GotViewport Browser.Dom.Viewport
    | GotHexMapViewport (Result Browser.Dom.Error Browser.Dom.Viewport)
    | GotResize Int Int
    | FocusInSidebar StellarObject
    | TableColumnHovered (Maybe String)
    | MapMouseDown ( Float, Float )
    | MapMouseUp
    | MapMouseMove ( Float, Float )


{-| Where the Hex is on the screen, in pixel coordinates
-}
type alias VisualHexOrigin =
    ( Int, Int )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize GotResize


init : Browser.Navigation.Key -> HostConfig.HostConfig -> ( Model, Cmd Msg )
init key hostConfig =
    let
        -- requestHistory : RequestHistory
        ( requestEntry, ( solarSystemDict, requestHistory ) ) =
            prepNextRequest ( Dict.empty, [] ) upperLeftHex lowerRightHex

        upperLeftHex =
            toUniversalAddress
                { sectorX = -10
                , sectorY = -2
                , x = 21
                , y = 12
                }

        lowerRightHex =
            toUniversalAddress
                { sectorX = -10
                , sectorY = -2
                , x = 31
                , y = 23
                }

        model : Model
        model =
            { hexScale = defaultHexSize
            , solarSystems = solarSystemDict
            , lastSolarSystemError = Nothing
            , requestHistory = requestHistory
            , dragMode = NoDragging
            , playerHex = toUniversalAddress { sectorX = -10, sectorY = -2, x = 31, y = 24 }
            , hoveringHex = Nothing
            , selectedHex = Nothing
            , selectedSystem = Nothing
            , sidebarHoverText = Nothing
            , viewport = Nothing
            , hexmapViewport = Nothing
            , key = key
            , selectedStellarObject = Nothing
            , upperLeftHex = upperLeftHex
            , lowerRightHex = lowerRightHex
            , hostConfig = hostConfig
            , sectors = Dict.empty
            }
    in
    ( model
    , Cmd.batch
        [ sendSolarSystemRequest requestEntry model.hostConfig model.upperLeftHex model.lowerRightHex
        , sendSectorRequest requestEntry model.hostConfig
        , Browser.Dom.getViewport
            |> Task.perform GotViewport
        ]
    )


hoverableStyle : Css.Style
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



--isStarOrbit : StellarObject -> Bool
--isStarOrbit obj =
--    case obj of
--        GasGiant gasGiantData ->
--            gasGiantData.orbitType < 10
--
--        TerrestrialPlanet terrestrialData ->
--            terrestrialData.orbitType < 10
--
--        PlanetoidBelt planetoidBeltData ->
--            planetoidBeltData.orbitType < 10
--
--        Planetoid planetoidData ->
--            planetoidData.orbitType < 10
--
--        Star (StarDataWrap starDataConfig) ->
--            starDataConfig.orbitType < 10


scaleAttr : Float -> Int -> Float
scaleAttr hexSize default =
    toFloat default * min 1 (hexSize / defaultHexSize)


rotatePoint : Float -> Int -> ( Float, Float ) -> Float -> Int -> ( Float, Float )
rotatePoint hexSize idx ( x_, y_ ) degrees_ distance =
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
    ( x_ + (scaleAttr hexSize distance * cosTheta) - (0 * sinTheta)
    , y_ + (scaleAttr hexSize distance * sinTheta) + (0 * cosTheta)
    )


viewHexEmpty : HexAddress -> HexAddress -> VisualHexOrigin -> Float -> Svg Msg -> Svg Msg
viewHexEmpty playerHexAddress hexAddress (( x, y ) as origin) size childSvg =
    let
        si =
            0

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
            Html.Events.Extra.Mouse.eventDecoder
                |> JsDecode.map (.offsetPos >> MapMouseMove)
    in
    Svg.g
        [ SvgEvents.onMouseOver (HoveringHex hexAddress)
        , SvgEvents.onClick (ViewingHex hexAddress)
        , SvgEvents.onMouseUp MapMouseUp
        , -- listens for the JS 'mousedown' event and then runs the `downDecoder` on the JS Event, returning the Msg
          SvgEvents.on "mousedown" downDecoder
        , SvgEvents.on "mousemove" moveDecoder
        , SvgAttrs.style "cursor: pointer; user-select: none"
        , SvgAttrs.id <| HexAddress.toKey hexAddress
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
          Svg.text_
            [ SvgAttrs.x <| String.fromInt <| x
            , SvgAttrs.y <| String.fromInt <| y - (floor <| size * 0.65)
            , SvgAttrs.fontSize
                (if size > 15 then
                    "10"

                 else
                    "5"
                )
            , SvgAttrs.textAnchor "middle"
            ]
            [ HexAddress.hexLabel hexAddress |> Svg.text
            ]
        , childSvg
        ]


renderHexWithStar : StarSystem -> HexAddress -> HexAddress -> VisualHexOrigin -> Float -> Svg Msg
renderHexWithStar starSystem _ hexAddress (( vox, voy ) as visualOrigin) size =
    let
        si =
            starSystem.surveyIndex

        showStar =
            starSystem.surveyIndex > 0

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
            Html.Events.Extra.Mouse.eventDecoder
                |> JsDecode.map (.offsetPos >> MapMouseMove)

        drawStar : ( Float, Float ) -> Int -> StarTypeData -> Svg Msg
        drawStar ( starX, starY ) radius star =
            Svg.circle
                [ SvgAttrs.cx <| String.fromFloat <| starX
                , SvgAttrs.cy <| String.fromFloat <| starY
                , SvgAttrs.r <| String.fromFloat <| scaleAttr size radius
                , SvgAttrs.fill <| starColourRGB star.colour
                ]
                []
    in
    Svg.g
        [ SvgEvents.onMouseOver (HoveringHex hexAddress)
        , SvgEvents.onClick (ViewingHex hexAddress)
        , SvgEvents.onMouseUp MapMouseUp
        , -- listens for the JS 'mousedown' event and then runs the `downDecoder` on the JS Event, returning the Msg
          SvgEvents.on "mousedown" downDecoder
        , SvgEvents.on "mousemove" moveDecoder
        , SvgAttrs.style "cursor: pointer; user-select: none"
        ]
        [ -- background hex
          Svg.polygon
            [ points (hexagonPoints visualOrigin size)
            , SvgAttrs.fill defaultHexBg
            , SvgAttrs.stroke "#CCCCCC"
            , SvgAttrs.strokeWidth "1"
            , SvgAttrs.pointerEvents "visiblePainted"
            , SvgAttrs.css [ hoverableStyle ]
            ]
            []
        , -- center star
          if showStar then
            let
                primaryPos =
                    ( toFloat vox, toFloat voy )

                generateStar : Int -> StarType -> Svg Msg
                generateStar idx starType =
                    let
                        starData =
                            getStarTypeData starType

                        starPos =
                            if idx == 0 then
                                ( toFloat vox, toFloat voy )

                            else
                                rotatePoint size (idx + 2) primaryPos 60 20
                    in
                    case starData.companion of
                        Just companion ->
                            let
                                compStarPos =
                                    Tuple.mapFirst (\x_ -> x_ - 5) starPos

                                compStarData =
                                    getStarTypeData companion
                            in
                            Svg.g []
                                [ drawStar starPos 7 starData
                                , drawStar compStarPos 3 compStarData
                                ]

                        Nothing ->
                            drawStar starPos 7 starData
            in
            Svg.g
                []
                (starSystem.stars
                    |> List.indexedMap generateStar
                )

          else
            Svg.text_
                [ SvgAttrs.x <| String.fromInt <| vox
                , SvgAttrs.y <| String.fromInt <| voy - (floor <| size * 0.65)
                , SvgAttrs.fontSize
                    (if size > 15 then
                        "10"

                     else
                        "5"
                    )
                , SvgAttrs.textAnchor "middle"
                ]
                [ HexAddress.hexLabel hexAddress |> Svg.text
                ]
        , if showStar then
            Svg.g []
                [ -- hex index
                  Svg.text_
                    [ SvgAttrs.x <| String.fromInt <| vox
                    , SvgAttrs.y <| String.fromInt <| voy - (floor <| size * 0.65)
                    , SvgAttrs.fontSize
                        (if size > 15 then
                            "10"

                         else
                            "5"
                        )
                    , SvgAttrs.textAnchor "middle"
                    ]
                    [ HexAddress.hexLabel hexAddress |> Svg.text
                    ]
                , Svg.text_
                    [ SvgAttrs.x <| String.fromInt <| vox
                    , SvgAttrs.y <| String.fromInt <| voy + (floor <| size * 0.85)
                    , SvgAttrs.fontSize "12"
                    , SvgAttrs.textAnchor "middle"
                    ]
                    (let
                        showGasGiants =
                            if si >= gasGiantSI then
                                String.fromInt <| starSystem.gasGiantCount

                            else
                                "?"

                        showTerrestrialPlanets =
                            if si >= terrestrialSI then
                                String.fromInt <| starSystem.terrestrialPlanetCount

                            else
                                "?"

                        showplanetoidBelts =
                            if si >= planetoidSI then
                                String.fromInt <| starSystem.planetoidBeltCount

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

          else
            Svg.text ""
        ]


defaultHexBg =
    "#f5f5f5"


defaultHexSize =
    40


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


calcVisualOrigin : Float -> { row : Int, col : Int } -> VisualHexOrigin
calcVisualOrigin hexSize { row, col } =
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
    -> VisualHexOrigin
    -> HexAddress
    -> ( Maybe (Svg Msg), Int )
viewHex widestViewport hexSize solarSystemDict ( viewportWidth, viewportHeight ) hexAddress ( vox, voy ) playerHexAddress =
    let
        solarSystem =
            Dict.get (HexAddress.toKey hexAddress) solarSystemDict

        hexSVG =
            Just
                (case solarSystem of
                    Just (LoadedSolarSystem ss) ->
                        renderHexWithStar ss
                            playerHexAddress
                            hexAddress
                            ( vox, voy )
                            hexSize

                    Just LoadingSolarSystem ->
                        Svg.text_
                            [ SvgAttrs.x <| String.fromInt vox
                            , SvgAttrs.y <| String.fromInt voy
                            , SvgAttrs.fontSize "10"
                            , SvgAttrs.textAnchor "middle"
                            ]
                            [ Svg.text "Loading..." ]
                            |> viewHexEmpty playerHexAddress hexAddress ( vox, voy ) hexSize

                    Just (FailedSolarSystem httpError) ->
                        Svg.text_
                            [ SvgAttrs.x <| String.fromInt vox
                            , SvgAttrs.y <| String.fromInt voy
                            , SvgAttrs.fontSize "10"
                            , SvgAttrs.textAnchor "middle"
                            ]
                            [ Svg.text "Failed." ]
                            |> viewHexEmpty playerHexAddress hexAddress ( vox, voy ) hexSize

                    Just LoadedEmptyHex ->
                        Svg.text_
                            [ SvgAttrs.x <| String.fromInt vox
                            , SvgAttrs.y <| String.fromInt voy
                            , SvgAttrs.fontSize "12"
                            , SvgAttrs.textAnchor "middle"
                            , SvgAttrs.fill "#ccc"
                            ]
                            []
                            |> viewHexEmpty playerHexAddress hexAddress ( vox, voy ) hexSize

                    Nothing ->
                        -- Svg.text_
                        --     [ SvgAttrs.x <| String.fromInt ox
                        --     , SvgAttrs.y <| String.fromInt oy
                        --     , SvgAttrs.fontSize "10"
                        --     , SvgAttrs.textAnchor "middle"
                        --     ]
                        --     [ Svg.text "Nothing" ]
                        Svg.text ""
                            |> viewHexEmpty playerHexAddress hexAddress ( vox, voy ) hexSize
                )
    in
    ( hexSVG, isEmptyHex solarSystem )


type RemoteSolarSystem
    = LoadedSolarSystem StarSystem
    | LoadedEmptyHex
    | LoadingSolarSystem
    | FailedSolarSystem Http.Error


type alias SolarSystemDict =
    Dict.Dict String RemoteSolarSystem


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
            screenVp.viewport.width * 0.9

        widestViewport =
            case hexmapVp of
                Nothing ->
                    screenVp

                Just hexmapViewport ->
                    hexmapViewport

        lowerRightHex =
            upperLeftHex
                |> HexAddress.shiftAddressBy { deltaX = 30, deltaY = 30 }

        ( zero_x, zero_y ) =
            calcVisualOrigin hexSize
                { row = 0
                , col = 0
                }

        hexRange =
            HexAddress.between upperLeftHex lowerRightHex
    in
    hexRange
        |> List.map
            (\hexAddr ->
                let
                    hexSVGOrigin =
                        calcVisualOrigin hexSize
                            { row = upperLeftHex.y - hexAddr.y, col = hexAddr.x - upperLeftHex.x }
                            |> (\( x, y ) ->
                                    ( x - zero_x
                                    , y - zero_y
                                    )
                               )
                in
                viewHex
                    widestViewport
                    hexSize
                    solarSystemDict
                    ( viewportWidthIsh, viewportHeightIsh )
                    hexAddr
                    hexSVGOrigin
                    playerHexId
            )
        |> List.filterMap Tuple.first
        |> (let
                stringWidth =
                    String.fromFloat <| viewportWidthIsh

                stringHeight =
                    String.fromFloat <| viewportHeightIsh
            in
            Svg.svg
                [ SvgAttrs.width <| stringWidth
                , SvgAttrs.height <| stringHeight
                , SvgAttrs.css <|
                    [ Css.before
                        [ Css.boxShadowMany
                            [ { offsetX = Css.px 0
                              , offsetY = Css.px 0
                              , blurRadius = Just <| Css.px 10
                              , spreadRadius = Just <| Css.px 10
                              , color = Just <| Css.hex "#FFFFFF"
                              , inset = True
                              }
                            ]
                        ]
                    ]
                , SvgAttrs.id "hexmap"
                , viewBox <|
                    "0"
                        ++ " "
                        ++ "0"
                        ++ " "
                        ++ stringWidth
                        ++ " "
                        ++ stringHeight
                ]
           )


orbitStyle : List (Element.Attribute msg)
orbitStyle =
    [ width <| Element.px 40
    , Element.alignRight
    ]


descriptionStyle : List (Element.Attribute msg)
descriptionStyle =
    [ width <| Element.px 74
    ]


sequenceStyle : List (Element.Attribute msg)
sequenceStyle =
    [ width <| Element.px 60
    ]


safeJumpStyle : List (Element.Attribute msg)
safeJumpStyle =
    [ width <| Element.px 60
    ]


imageStyle : List (Element.Attribute msg)
imageStyle =
    [ width <| Element.px 40
    ]


travelStyle : List (Element.Attribute msg)
travelStyle =
    [ width <| Element.px 60
    ]


{-| Builds a monospace text element
-}
monospaceText : String -> Element.Element msg
monospaceText someString =
    text someString |> el [ Font.family [ Font.monospace ] ]


calcNestedOffset : Int -> Float
calcNestedOffset newNestingLevel =
    toFloat <| newNestingLevel * 10


renderRawOrbit : Float -> Element.Element msg
renderRawOrbit au =
    let
        roundedAU =
            if au < 1 then
                Round.round 2 au

            else if au < 100 then
                Round.round 1 au

            else
                Round.round 0 au
    in
    Element.el
        orbitStyle
        (monospaceText <| roundedAU)


renderOrbitSequence : String -> Element.Element msg
renderOrbitSequence sequence =
    Element.el
        sequenceStyle
        (monospaceText <| sequence)


renderSODescription : String -> Element.Element msg
renderSODescription description =
    Element.el
        descriptionStyle
        (monospaceText <| description)


renderJumpTime : String -> Element.Element Msg
renderJumpTime time =
    Element.row safeJumpStyle
        [ Element.html (Icon.arrowUpFromBracket |> Icon.styled [ Icon.lg ] |> Icon.view)
        , monospaceText <| time
        ]


renderImage : String -> Element.Element Msg
renderImage uwp =
    let
        gasGiantUwps =
            [ "GS", "GM", "GL" ]

        hydrographics =
            if String.length uwp == 9 then
                String.slice 3 4 uwp

            else
                ""

        imageUrl =
            if List.member uwp gasGiantUwps then
                "public/gasgiant-small.png"

            else if hydrographics == "A" then
                "public/waterworld-small.png"

            else if hydrographics == "0" then
                "public/desertworld-small.png"

            else
                "public/moon-small.png"
    in
    Element.image [ width <| Element.px 18, height <| Element.px 18 ]
        { src = imageUrl
        , description = ""
        }


renderTravelTime : StellarObject -> Maybe StellarObject -> Element.Element Msg
renderTravelTime destination origin =
    let
        shipMDrive =
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
                        travelTime dist shipMDrive False

                    else
                        ""

                Nothing ->
                    ""
    in
    case origin of
        Just obj ->
            if obj /= destination then
                Element.el
                    travelStyle
                    (monospaceText <| travelTimeStr)

            else
                Element.row travelStyle
                    [ Element.html (Icon.upDown |> Icon.styled [ Icon.lg ] |> Icon.view)
                    ]

        Nothing ->
            monospaceText <| ""



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


renderGasGiant : Int -> GasGiantData -> Maybe StellarObject -> Element.Element Msg
renderGasGiant newNestingLevel gasGiantData selectedStellarObject =
    let
        stellarObject =
            GasGiant gasGiantData
    in
    row
        [ Element.spacing 8
        , Element.moveRight <| calcNestedOffset newNestingLevel
        , Font.size 14
        , Element.Events.onClick <| FocusInSidebar stellarObject
        ]
        [ renderRawOrbit gasGiantData.au
        , renderOrbitSequence gasGiantData.orbitSequence
        , renderSODescription gasGiantData.code
        , renderImage gasGiantData.code
        , renderJumpTime gasGiantData.safeJumpTime
        , renderTravelTime stellarObject selectedStellarObject
        ]


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
        [ renderRawOrbit terrestrialData.au
        , renderOrbitSequence terrestrialData.orbitSequence
        , renderSODescription terrestrialData.uwp
        , renderImage terrestrialData.uwp
        , renderJumpTime terrestrialData.safeJumpTime
        , renderTravelTime planet selectedStellarObject
        ]


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
        [ renderRawOrbit planetoidBeltData.au
        , renderOrbitSequence planetoidBeltData.orbitSequence
        , renderSODescription planetoidBeltData.uwp
        , renderImage planetoidBeltData.uwp
        , renderJumpTime planetoidBeltData.safeJumpTime
        , renderTravelTime belt selectedStellarObject
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
        [ renderRawOrbit planetoidData.au
        , renderOrbitSequence planetoidData.orbitSequence
        , renderSODescription planetoidData.uwp
        , renderImage planetoidData.uwp
        , renderJumpTime planetoidData.safeJumpTime
        , renderTravelTime planet selectedStellarObject
        ]


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


convertColor : Color.Color -> Element.Color
convertColor color =
    Element.fromRgb <| Color.toRgba <| color


renderOrbit : StellarObject -> Element.Element Msg
renderOrbit stellarObject =
    stellarObject
        |> getStellarOrbit
        |> -- this is an anonymous function that gets this field off of what gets passed in
           .au
        |> Round.round 2
        |> String.padLeft 6 ' '
        |> text
        |> el [ Font.family [ Font.monospace ] ]


renderSequence : StellarObject -> Element.Element Msg
renderSequence stellarObject =
    stellarObject
        |> getStellarOrbit
        |> -- this is an anonymous function that gets this field off of what gets passed in
           .orbitSequence
        |> text
        |> el [ Font.family [ Font.monospace ] ]


renderSafeJump : StellarObject -> Element.Element Msg
renderSafeJump stellarObject =
    stellarObject
        |> getSafeJumpTime
        |> (\safeJumpTime ->
                text <| "j: " ++ safeJumpTime
           )
        |> el [ Font.family [ Font.monospace ] ]


renderDescription : StellarObject -> Element.Element Msg
renderDescription stellarObject =
    let
        description =
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
    in
    el [ Font.family [ Font.monospace ] ] description


viewSystemDetailsSidebar : Maybe String -> SolarSystem -> Maybe StellarObject -> Element Msg
viewSystemDetailsSidebar sidebarHoverText solarSystem selectedStellarObject =
    let
        primaryStar : StarData
        primaryStar =
            solarSystem.primaryStar

        starDataConfig =
            getInnerStarData primaryStar

        stellarObjects =
            starDataConfig.stellarObjects
    in
    column [ Element.spacing 10, Element.paddingXY 0 10 ] <|
        [ text <| solarSystem.sectorName ++ " " ++ addressToString solarSystem

        --, let
        --    tableColumn desc viewFunc =
        --        { header = text ""
        --        , width = Element.fill
        --        , view =
        --            Element.el [ Element.Events.onMouseEnter <| TableColumnHovered (Just desc) ]
        --                << viewFunc
        --        }
        --
        --    table =
        --        Element.table [ Element.spacingXY 5 0, Element.Events.onMouseLeave (TableColumnHovered Nothing) ]
        --            { data = stellarObjects
        --            , columns =
        --                [ tableColumn "Orbit" renderOrbit
        --                , tableColumn "Sequence" renderSequence
        --                , tableColumn "UWP/Desc" renderDescription
        --                , tableColumn "???" (always (text "col"))
        --                , tableColumn "Safe Jump Time" renderSafeJump
        --                , tableColumn "???" (always (text "col"))
        --                ]
        --            }
        --  in
        --  column []
        --    [ el [ Font.size 12, Font.italic, centerX ] <|
        --        text (sidebarHoverText |> Maybe.withDefault "--")
        --    , table
        --    ]
        , -- render the nested chart of the system
          let
            comparePos =
                ( 0, 0 )
          in
          renderStar comparePos solarSystem.primaryStar 0 selectedStellarObject
        ]


colorToElementColor : Color.Color -> Element.Color
colorToElementColor color =
    color
        |> Color.toRgba
        |> (\{ red, green, blue, alpha } -> Element.rgba red green blue alpha)


fontTextColor : Element.Color
fontTextColor =
    textColor |> colorToElementColor


{-| Color.Color is not Element.Color
-}
textColor : Color.Color
textColor =
    Color.rgb 0.5 1.0 0.5


fontDarkTextColor : Element.Color
fontDarkTextColor =
    textColor
        |> Color.Manipulate.desaturate 0.85
        |> Color.Manipulate.darken 0.25
        |> colorToElementColor


sliderColors : List Element.Color
sliderColors =
    -- gradient colors so it looks like a slider
    [ Element.rgba 0 0 0 0
    , textColor |> Color.Manipulate.fadeOut 0.99 |> colorToElementColor
    , textColor |> Color.Manipulate.fadeOut 0.95 |> colorToElementColor
    , textColor |> Color.Manipulate.fadeOut 0.95 |> colorToElementColor
    , textColor |> Color.Manipulate.fadeOut 0.05 |> colorToElementColor
    , textColor |> Color.Manipulate.fadeOut 0.95 |> colorToElementColor
    , textColor |> Color.Manipulate.fadeOut 0.95 |> colorToElementColor
    , textColor |> Color.Manipulate.fadeOut 0.99 |> colorToElementColor
    , Element.rgba 0 0 0 0
    ]


zoomSlider : Float -> Element Msg
zoomSlider hexScale =
    Input.slider
        [ Background.gradient
            { angle = 0
            , steps =
                sliderColors
            }
        , Border.rounded 200
        , Element.paddingXY 0 0
        , Element.width <| Element.px 200
        ]
        { onChange = ZoomScaleChanged
        , label =
            Input.labelLeft [ Font.family [ Font.monospace ], Font.size 16, Element.paddingXY 0 5 ]
                (text <| "HexSize: " ++ (String.padLeft 2 ' ' <| String.fromFloat hexScale))
        , min = 1
        , max = 76
        , step = Just 5
        , value = hexScale
        , thumb = Input.defaultThumb
        }


numHexCols =
    sectorColumns


numHexRows =
    sectorRows


view : Model -> Element.Element Msg
view model =
    let
        sidebarColumn =
            column []
                [ el [ Font.size 20 ] <|
                    text <|
                        "Deepnight Revelation Navigation Console"
                , el [ Font.size 14, Font.color <| fontDarkTextColor ] <|
                    text <|
                        "Viewing "
                            ++ String.fromInt numHexCols
                            ++ " columns and "
                            ++ String.fromInt numHexRows
                            ++ " rows"
                , el [ Font.size 14, Font.color <| fontDarkTextColor ] <|
                    text <|
                        "Total hexes: "
                            ++ String.fromInt (numHexCols * numHexRows)
                , -- zoom slider
                  zoomSlider model.hexScale
                , column
                    [ Font.size 14
                    , Font.color <| fontTextColor
                    ]
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
                , case model.selectedHex of
                    Just viewingAddress ->
                        case model.solarSystems |> Dict.get (HexAddress.toKey viewingAddress) of
                            Just (LoadedSolarSystem s) ->
                                let
                                    key =
                                        toSectorKey <| universalToSector viewingAddress

                                    hexDescription =
                                        case Dict.get key model.sectors of
                                            Just sector ->
                                                sector.name ++ " " ++ hexLabel viewingAddress

                                            Nothing ->
                                                "Hex Address: " ++ HexAddress.toKey viewingAddress

                                    foo =
                                        column [ centerX, centerY, Font.size 10, Element.moveDown 20 ]
                                            [ text <| hexDescription
                                            ]
                                in
                                foo

                            Just LoadingSolarSystem ->
                                column [ centerX, centerY, Font.size 10, Element.moveDown 20 ]
                                    [ text "loading..."
                                    , text <| "Hex Address: " ++ HexAddress.hexAddressToString viewingAddress
                                    ]

                            Just LoadedEmptyHex ->
                                column [ centerX, centerY, Font.size 10, Element.moveDown 20 ]
                                    [ text "[empty]"
                                    , text <| "Hex Address: " ++ HexAddress.hexAddressToString viewingAddress
                                    ]

                            Just (FailedSolarSystem httpError) ->
                                column [ centerX, centerY, Font.size 10, Element.moveDown 20 ]
                                    [ text "failed."
                                    , text <| "Hex Address: " ++ HexAddress.hexAddressToString viewingAddress
                                    ]

                            Nothing ->
                                column [ centerX, centerY, Font.size 10, Element.moveDown 20 ]
                                    [ text "No solar system data found for system."
                                    , text <| "Hex Address: " ++ HexAddress.hexAddressToString viewingAddress
                                    ]

                    Nothing ->
                        column [ centerX, centerY, Font.size 10, Element.moveDown 20 ]
                            [ text "Click a hex to view system details."
                            ]
                , case model.selectedSystem of
                    Just solarSystem ->
                        viewSystemDetailsSidebar
                            model.sidebarHoverText
                            solarSystem
                            model.selectedStellarObject

                    Nothing ->
                        column [ centerX, centerY, Font.size 10, Element.moveDown 20 ]
                            [ text "Click a hex to view system details."
                            ]
                ]

        renderError : String -> UnstyledHtml.Html msg
        renderError txt =
            Html.toUnstyled <|
                Html.div [ Html.Styled.Attributes.css [ Css.color (Css.hex "#ff0000") ] ]
                    [ Html.text txt ]

        renderHttpError httpError =
            case httpError of
                Http.BadBody error ->
                    renderError error

                Http.BadUrl url ->
                    renderError <| "Invalid URL: " ++ url

                Http.NetworkError ->
                    renderError "Network Error"

                Http.BadStatus statusCode ->
                    renderError <| "BadStatus: " ++ String.fromInt statusCode

                Http.Timeout ->
                    renderError "Request timedout"

        hexesColumn =
            column []
                [ Element.html <|
                    -- Note: we use elm-css for type-safe CSS, so we need to use the Html.Styled.* dropins for Html.
                    case model.viewport of
                        Just viewport ->
                            let
                                defaultViewport =
                                    { screenVp = viewport, hexmapVp = Nothing }

                                viewPortConfig =
                                    case model.hexmapViewport of
                                        Nothing ->
                                            defaultViewport

                                        Just (Ok hexmapViewport) ->
                                            { defaultViewport | hexmapVp = Just hexmapViewport }

                                        Just (Err notFoundError) ->
                                            let
                                                _ =
                                                    --TODO handle the hexmap svg not being present
                                                    Debug.log "cant use, element not present" notFoundError
                                            in
                                            defaultViewport
                            in
                            viewHexes
                                model.upperLeftHex
                                viewPortConfig
                                model.solarSystems
                                model.playerHex
                                model.hexScale
                                |> Html.toUnstyled

                        -- ( RemoteData.Failure httpError, _ ) ->
                        --     renderHttpError httpError
                        --
                        -- ( NotAsked, _ ) ->
                        --     Html.toUnstyled <|
                        --         Html.div [ Html.Styled.Attributes.css [ Css.color (Css.rgb 100 100 100) ] ]
                        --             [ Html.text "Not yet asked" ]
                        --
                        -- ( Loading, _ ) ->
                        --     Html.toUnstyled <| Html.text "Loading..."
                        --
                        Nothing ->
                            Html.toUnstyled <| Html.text "Have sector data but no viewport"
                ]
    in
    column [ width fill ]
        [ row
            [ width fill
            , Font.size 20
            , Font.color <| fontTextColor
            , Element.paddingXY 15 0
            ]
            [ el [ Element.width <| Element.px 400, Element.alignTop, Element.alignLeft ] <|
                sidebarColumn
            , el [ Element.alignTop ] <|
                hexesColumn
            ]

        -- , -- TODO: bring this back
        -- displaying json errors for SectorData
        --   case model.solarSystems of
        --     Failure (Http.BadBody error) ->
        --         (-- use <pre> to preserve whitespace
        --          Html.pre [ Html.Styled.Attributes.css [ Css.overflow Css.hidden ] ]
        --             [ Html.text error ]
        --             -- convert from elm-css's HTML
        --             |> Html.toUnstyled
        --             -- turn html into elm-ui
        --             |> Element.html
        --         )
        --
        --     _ ->
        --         Element.none
        ]


sendSolarSystemRequest : RequestEntry -> HostConfig -> HexAddress -> HexAddress -> Cmd Msg
sendSolarSystemRequest requestEntry hostConfig upperLeft lowerRight =
    let
        solarSystemsDecoder : JsDecode.Decoder (List StarSystem)
        solarSystemsDecoder =
            Codec.list starSystemCodec
                |> Codec.decoder

        ( urlHostRoot, urlHostPath ) =
            hostConfig

        url =
            Url.Builder.crossOrigin
                urlHostRoot
                (urlHostPath ++ [ "stars" ])
                [ Url.Builder.int "ulx" upperLeft.x
                , Url.Builder.int "uly" upperLeft.y
                , Url.Builder.int "lrx" lowerRight.x
                , Url.Builder.int "lry" lowerRight.y
                ]

        requestCmd =
            -- using Http.request instead of Http.get, to allow setting a timeout
            Http.request
                { method = "GET"
                , headers = []
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectJson (DownloadedSolarSystems requestEntry) solarSystemsDecoder
                , timeout = Just 5000
                , tracker = Nothing
                }
    in
    requestCmd


fetchSingleSolarSystemRequest : HostConfig -> SectorHexAddress -> Cmd Msg
fetchSingleSolarSystemRequest hostConfig hex =
    let
        solarSystemDecoder : JsDecode.Decoder SolarSystem
        solarSystemDecoder =
            SolarSystem.codec |> Codec.decoder

        ( urlHostRoot, urlHostPath ) =
            hostConfig

        url =
            Url.Builder.crossOrigin
                urlHostRoot
                (urlHostPath ++ [ "solarsystem" ])
                [ Url.Builder.int "sx" hex.sectorX
                , Url.Builder.int "sy" hex.sectorY
                , Url.Builder.int "hx" hex.x
                , Url.Builder.int "hy" hex.y
                ]

        requestCmd =
            -- using Http.request instead of Http.get, to allow setting a timeout
            Http.request
                { method = "GET"
                , headers = []
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectJson FetchedSolarSystem solarSystemDecoder
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
            let
                ( nextRequestEntry, ( newSolarSystemDict, newRequestHistory ) ) =
                    prepNextRequest ( model.solarSystems, model.requestHistory ) model.upperLeftHex model.lowerRightHex
            in
            ( { model
                | requestHistory = newRequestHistory
                , solarSystems = newSolarSystemDict
              }
            , sendSolarSystemRequest nextRequestEntry model.hostConfig model.upperLeftHex model.lowerRightHex
            )

        DownloadedSolarSystems requestEntry (Ok solarSystems) ->
            let
                rangeAsPairs =
                    HexAddress.between requestEntry.upperLeftHex model.lowerRightHex
                        |> List.map
                            (\addr ->
                                let
                                    addrKey =
                                        HexAddress.toKey addr
                                in
                                ( addrKey
                                , Dict.get addrKey sortedSolarSystems |> Maybe.withDefault LoadedEmptyHex
                                )
                            )

                sortedSolarSystems =
                    solarSystems
                        |> List.sortBy (HexAddress.toKey << .address)
                        |> List.map
                            (\system ->
                                ( HexAddress.toKey system.address
                                , LoadedSolarSystem system
                                )
                            )
                        |> Dict.fromList

                solarSystemDict =
                    rangeAsPairs
                        |> Dict.fromList
                        |> (\newDict ->
                                -- `Dict.union` merges the dict, preferring the left arg's to resolve dupes, so we want to prefer the new one
                                Dict.union newDict existingDict
                           )

                existingDict =
                    model.solarSystems

                newRequestHistory =
                    markRequestComplete requestEntry (RemoteData.Success ()) model.requestHistory
            in
            ( { model
                | solarSystems = solarSystemDict
                , requestHistory = newRequestHistory
              }
            , Cmd.batch
                [ Browser.Dom.getViewportOf "hexmap"
                    |> Task.attempt GotHexMapViewport
                ]
            )

        DownloadedSectors requestEntry (Ok sectors) ->
            let
                sectorDict =
                    List.foldl
                        (\sector acc ->
                            Dict.insert (sectorKey sector) sector acc
                        )
                        Dict.empty
                        sectors
            in
            ( { model
                | sectors = sectorDict
              }
            , Cmd.none
            )

        FetchedSolarSystem (Ok solarSystem) ->
            ( { model
                | selectedSystem = Just solarSystem
              }
            , Cmd.none
            )

        FetchedSolarSystem (Err _) ->
            ( model, Cmd.none )

        DownloadedSectors requestEntry (Err _) ->
            ( model, Cmd.none )

        DownloadedSolarSystems requestEntry (Err err) ->
            let
                newRequestHistory =
                    markRequestComplete requestEntry (RemoteData.Failure err) model.requestHistory

                rangeAsPairs =
                    HexAddress.between requestEntry.upperLeftHex model.lowerRightHex
                        |> List.map
                            (\addr ->
                                let
                                    addrKey =
                                        HexAddress.toKey addr
                                in
                                ( addrKey
                                , Dict.get addrKey existingDict |> Maybe.withDefault (FailedSolarSystem err)
                                )
                            )

                solarSystemDict =
                    rangeAsPairs
                        |> Dict.fromList
                        |> (\newDict ->
                                -- `Dict.union` merges the dict, preferring the left arg's to resolve dupes, so we want to prefer the new one
                                Dict.union newDict existingDict
                           )

                existingDict =
                    model.solarSystems
            in
            ( { model
                | solarSystems = solarSystemDict
                , lastSolarSystemError = Just err
                , requestHistory = newRequestHistory
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

        GotResize width height ->
            ( model
            , Cmd.batch
                [ Browser.Dom.getViewport
                    |> Task.perform GotViewport
                , Browser.Dom.getViewportOf "hexmap"
                    |> Task.attempt GotHexMapViewport
                ]
            )

        ViewingHex hexAddress ->
            ( { model
                | selectedHex = Just hexAddress
                , selectedStellarObject = Nothing
              }
            , fetchSingleSolarSystemRequest model.hostConfig <| universalToSector hexAddress
            )

        FocusInSidebar stellarObject ->
            ( { model | selectedStellarObject = Just stellarObject }
            , Cmd.none
            )

        MapMouseDown ( x, y ) ->
            ( { model
                | dragMode = IsDragging ( x, y )
              }
            , Cmd.none
            )

        MapMouseUp ->
            let
                ( nextRequestEntry, ( newSolarSystemDict, newRequestHistory ) ) =
                    prepNextRequest ( model.solarSystems, model.requestHistory ) model.upperLeftHex model.lowerRightHex
            in
            ( { model
                | dragMode = NoDragging
                , requestHistory = newRequestHistory
                , solarSystems = newSolarSystemDict
              }
            , sendSolarSystemRequest nextRequestEntry model.hostConfig model.upperLeftHex model.lowerRightHex
            )

        MapMouseMove ( newX, newY ) ->
            case model.dragMode of
                IsDragging ( originX, originY ) ->
                    let
                        xDelta =
                            truncate <| (originX - newX) / model.hexScale

                        yDelta =
                            truncate <| (originY - newY) / model.hexScale

                        shiftAddress hex =
                            HexAddress.shiftAddressBy
                                { deltaX = xDelta, deltaY = yDelta }
                                hex

                        newModel =
                            { model
                                | dragMode = IsDragging ( newX, newY )
                                , lowerRightHex = shiftAddress model.lowerRightHex
                                , upperLeftHex = shiftAddress model.upperLeftHex
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

        TableColumnHovered columnDesc ->
            ( { model | sidebarHoverText = columnDesc }, Cmd.none )


stripDataFromRemoteData : RemoteData err data -> RemoteData err ()
stripDataFromRemoteData remoteData =
    case remoteData of
        RemoteData.Success _ ->
            RemoteData.Success ()

        RemoteData.Failure err ->
            RemoteData.Failure err

        RemoteData.NotAsked ->
            RemoteData.NotAsked

        RemoteData.Loading ->
            RemoteData.Loading


markRequestComplete : RequestEntry -> RemoteData Http.Error () -> RequestHistory -> RequestHistory
markRequestComplete requestEntry remoteData requestHistory =
    requestHistory
        |> List.map
            (\entry ->
                if entry.requestNum == requestEntry.requestNum then
                    { entry | status = stripDataFromRemoteData remoteData }

                else
                    entry
            )


sendSectorRequest : RequestEntry -> HostConfig -> Cmd Msg
sendSectorRequest requestEntry hostConfig =
    let
        sectorDecoder : JsDecode.Decoder (List Sector)
        sectorDecoder =
            Codec.list codecSector
                |> Codec.decoder

        ( urlHostRoot, urlHostPath ) =
            hostConfig

        url =
            Url.Builder.crossOrigin
                urlHostRoot
                (urlHostPath ++ [ "sectors" ])
                []

        requestCmd =
            -- using Http.request instead of Http.get, to allow setting a timeout
            Http.request
                { method = "GET"
                , headers = []
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectJson (DownloadedSectors requestEntry) sectorDecoder
                , timeout = Just 5000
                , tracker = Nothing
                }
    in
    requestCmd
