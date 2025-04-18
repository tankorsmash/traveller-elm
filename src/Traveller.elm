port module Traveller exposing (Model, Msg(..), auToKMs, init, subscriptions, update, view)

import Browser.Dom
import Browser.Events
import Browser.Navigation
import Codec
import Color exposing (Color)
import Color.Convert
import Color.Manipulate
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
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FontAwesome as Icon exposing (Icon)
import FontAwesome.Solid as Icon
import HostConfig exposing (HostConfig)
import Html exposing (Html)
import Html.Attributes as HtmlAttrs
import Html.Events
import Html.Events.Extra.Mouse
import Html.Lazy
import Http
import Json.Decode as JsDecode
import Maybe.Extra as Maybe
import Parser
import RemoteData exposing (RemoteData(..))
import Result.Extra as Result
import Round
import Svg exposing (Svg)
import Svg.Attributes as SvgAttrs exposing (points, viewBox)
import Svg.Events as SvgEvents
import Svg.Keyed
import Svg.Lazy
import Task
import Traveller.Atmosphere exposing (atmosphereDescription)
import Traveller.Government as Government
import Traveller.HexAddress as HexAddress exposing (HexAddress, SectorHexAddress, createFromStarSystem, shiftAddressBy, toSectorAddress, toUniversalAddress)
import Traveller.LawLevel as LawLevel
import Traveller.Parser exposing (UWP, hydrosphereDescription, sizeDescription, uwp)
import Traveller.Point exposing (StellarPoint)
import Traveller.Population exposing (populationDescription)
import Traveller.Region as Region exposing (Region, RegionDict)
import Traveller.Route as Route exposing (Route, RouteList)
import Traveller.Sector exposing (Sector, SectorDict, codec, sectorKey)
import Traveller.SolarSystem as SolarSystem exposing (SolarSystem)
import Traveller.SolarSystemStars exposing (FallibleStarSystem, StarSystem, StarType, StarTypeData, fallibleStarSystemDecoder, getStarTypeData, isBrownDwarfType)
import Traveller.StarColour exposing (starColourRGB)
import Traveller.Starport as Starport
import Traveller.StellarObject exposing (GasGiantData, InnerStarData, PlanetoidBeltData, PlanetoidData, StarData(..), StellarObject(..), TerrestrialData, getInnerStarData, getStarData, getStellarOrbit, isBrownDwarf)
import Traveller.TechLevel as TechLevel
import Url.Builder


uiDeepnightColorFontColour =
    Font.color <| colorToElementColor <| deepnightColor


hexSizeFactor =
    2 * pi / 6


refereeSI =
    99


gasGiantSI =
    5


terrestrialSI =
    6


planetoidSI =
    6


sidebarWidth =
    400


consoleTitleHeight =
    46


fullJourneyImageWidth =
    2176


fullJourneyImageHeight =
    2240


type alias HexMapViewport =
    Result Browser.Dom.Error Browser.Dom.Viewport


type DragMode
    = IsDragging { start : ( Float, Float ), last : ( Float, Float ) }
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


type alias HexKey =
    String


type alias HexColorDict =
    Dict.Dict HexKey Color


type alias RegionLabelDict =
    Dict.Dict HexKey String


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


hexWidth : Float -> Float
hexWidth hexScale =
    hexScale * (1.0 + cos hexSizeFactor)


hexHeight : Float -> Float
hexHeight hexScale =
    hexScale * (1.0 + sin hexSizeFactor)


horizontalHexes : Maybe HexMapViewport -> Float -> Int
horizontalHexes hexmapViewport hexScale =
    case hexmapViewport of
        Just vp ->
            case vp of
                Ok viewport ->
                    (viewport.viewport.width / hexWidth hexScale) |> floor

                Err _ ->
                    defaultHorizontalHexes

        Nothing ->
            defaultHorizontalHexes


verticalHexes : Maybe HexMapViewport -> Float -> Int
verticalHexes hexmapViewport hexScale =
    case hexmapViewport of
        Just (Ok viewport) ->
            (viewport.viewport.height / hexHeight hexScale) |> floor

        Just (Err _) ->
            defaultVerticalHexes

        Nothing ->
            defaultVerticalHexes


{-| Builds a RequestEntry and updates the existing History with it.

This is so we have less chance of getting the history out of sync with the
entries, because this is the only way to construct a RequestEntry.

-}
prepNextRequest : ( SolarSystemDict, RequestHistory ) -> HexRect -> ( RequestEntry, ( SolarSystemDict, RequestHistory ) )
prepNextRequest ( oldSolarSystemDict, requestHistory ) { upperLeftHex, lowerRightHex } =
    let
        requestNum =
            nextRequestNum requestHistory

        requestEntry =
            { requestNum = requestNum
            , upperLeftHex = upperLeftHex
            , lowerRightHex = lowerRightHex
            , status = RemoteData.Loading
            }

        newSolarSystemDict =
            let
                markSolarSystemLoadingIfNotFound maybeSolarSystem =
                    case maybeSolarSystem of
                        Just existingSolarSystem ->
                            Just existingSolarSystem

                        Nothing ->
                            Just LoadingSolarSystem
            in
            HexAddress.between upperLeftHex lowerRightHex
                |> List.foldl
                    (\hexAddr solarSystemDict ->
                        Dict.update (HexAddress.toKey hexAddr)
                            markSolarSystemLoadingIfNotFound
                            solarSystemDict
                    )
                    oldSolarSystemDict
    in
    ( requestEntry, ( newSolarSystemDict, requestEntry :: requestHistory ) )


type ViewMode
    = HexMap
    | FullJourney


type alias JourneyModel =
    { zoomScale : Float
    , zoomOffset : ( Float, Float )
    , hoverPoint : Maybe ( Float, Float )
    , dragMode : DragMode
    }


type alias Model =
    { key : Browser.Navigation.Key
    , hexScale : Float
    , viewMode : ViewMode
    , journeyModel : JourneyModel
    , rawHexaPoints : List ( Float, Float )
    , solarSystems : SolarSystemDict
    , newSolarSystemErrors : List ( Http.Error, String )
    , oldSolarSystemErrors : List ( Http.Error, String )
    , lastSolarSystemError : Maybe Http.Error
    , requestHistory : RequestHistory
    , dragMode : DragMode
    , sectors : SectorDict
    , hoveringHex : Maybe HexAddress
    , selectedHex : Maybe HexAddress
    , selectedSystem : Maybe SolarSystem
    , sidebarHoverText : Maybe String
    , viewport : Browser.Dom.Viewport
    , hexmapViewport : Maybe HexMapViewport
    , selectedStellarObject : Maybe StellarObject
    , hexRect : HexRect
    , currentAddress : HexAddress
    , hostConfig : HostConfig.HostConfig
    , route : RouteList
    , regions : RegionDict
    , regionLabels : Dict.Dict String String
    , hexColours : Dict.Dict String Color
    , referee : Bool
    , hoveredStellarObject : Maybe String
    }


type ZoomType
    = ZoomIn
    | ZoomOut
    | ZoomSet Float


type Msg
    = NoOpMsg
    | DownloadSolarSystems
    | DownloadedSolarSystems ( RequestEntry, String ) (Result Http.Error (List FallibleStarSystem))
    | ClearAllErrors
    | FetchedSolarSystem (Result Http.Error SolarSystem)
    | DownloadedSectors ( RequestEntry, String ) (Result Http.Error (List Sector))
    | DownloadedRegions ( RequestEntry, String ) (Result Http.Error (List Region))
    | HoveringHex HexAddress
    | ViewingHex HexAddress
    | GotViewport Browser.Dom.Viewport
    | GotHexMapViewport (Result Browser.Dom.Error Browser.Dom.Viewport)
    | GotResize Int Int
    | FocusInSidebar StellarObject
    | HoveredStellarObject (Maybe String)
    | TableColumnHovered (Maybe String)
    | MapMouseDown ( Float, Float )
    | MapMouseUp
    | MapMouseMove ( Float, Float )
    | MapMouseLeave
    | DownloadedRoute ( RequestEntry, String ) (Result Http.Error (List Route))
    | SetHexSize Float
    | ToggleHexmap
    | JumpToShip
    | ZoomToHex HexAddress Bool
    | JourneyMsg JourneyMsg


type JourneyMsg
    = Zoom ZoomType
    | MouseDown ( Float, Float )
    | MouseMove ( Float, Float )
    | MouseUp ( Float, Float )
    | MouseLeave


{-| Where the Hex is on the screen, in pixel coordinates
-}
type alias VisualHexOrigin =
    ( Int, Int )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize GotResize


type alias Flags =
    { upperLeft : Maybe ( Int, Int )
    , hexSize : Float
    }


defaultHexRectSize : Int
defaultHexRectSize =
    30


defaultHorizontalHexes : Int
defaultHorizontalHexes =
    30


defaultVerticalHexes : Int
defaultVerticalHexes =
    25


init : Browser.Dom.Viewport -> Flags -> Browser.Navigation.Key -> HostConfig.HostConfig -> Bool -> ( Model, Cmd Msg )
init viewport settings key hostConfig referee =
    let
        -- requestHistory : RequestHistory
        ( initSystemDict, initRequestHistory ) =
            ( Dict.empty, [] )

        ( ( ssReqEntry, secReqEntry, routeReqEntry ), ( solarSystemDict, requestHistory ) ) =
            prepNextRequest ( initSystemDict, initRequestHistory ) hexRect
                |> -- build a new request entry for sector request
                   (\( ssReqEntry_, oldSsDictAndReqHistory ) ->
                        let
                            ( newReqEntry, ssDictAndReqHistory ) =
                                prepNextRequest oldSsDictAndReqHistory hexRect
                        in
                        ( ( ssReqEntry_, newReqEntry ), ssDictAndReqHistory )
                   )
                |> -- take the old ones and build a new one for route request
                   (\( ( ssReqEntry_, secReqEntry_ ), oldSsDictAndReqHistory ) ->
                        let
                            ( routeReqEntry_, ssDictAndReqHistory ) =
                                prepNextRequest oldSsDictAndReqHistory hexRect
                        in
                        ( ( ssReqEntry_, secReqEntry_, routeReqEntry_ ), ssDictAndReqHistory )
                   )

        hexRect =
            let
                upperLeftHex =
                    case settings.upperLeft of
                        Just ( x, y ) ->
                            HexAddress x y

                        Nothing ->
                            toUniversalAddress
                                { sectorX = -10
                                , sectorY = -2
                                , x = 21
                                , y = 12
                                }

                lowerRightHex =
                    upperLeftHex
                        |> HexAddress.shiftAddressBy
                            { deltaX = defaultHexRectSize
                            , deltaY = defaultHexRectSize
                            }
            in
            { upperLeftHex = upperLeftHex, lowerRightHex = lowerRightHex }

        journeyModel : JourneyModel
        journeyModel =
            { zoomScale = 1.0
            , zoomOffset = ( 0, 0 )
            , hoverPoint = Nothing
            , dragMode = NoDragging
            }

        model : Model
        model =
            { hexScale = settings.hexSize
            , viewMode = HexMap
            , journeyModel = journeyModel
            , rawHexaPoints = rawHexagonPoints <| settings.hexSize
            , solarSystems = solarSystemDict
            , newSolarSystemErrors = []
            , oldSolarSystemErrors = []
            , lastSolarSystemError = Nothing
            , requestHistory = requestHistory
            , dragMode = NoDragging
            , hoveringHex = Nothing
            , selectedHex = Nothing
            , selectedSystem = Nothing
            , sidebarHoverText = Nothing
            , viewport = viewport
            , hexmapViewport = Nothing
            , key = key
            , selectedStellarObject = Nothing
            , hexRect = hexRect
            , hostConfig = hostConfig
            , sectors = Dict.empty
            , route = []
            , currentAddress = toUniversalAddress { sectorX = -10, sectorY = -2, x = 31, y = 24 }
            , regions = Dict.empty
            , regionLabels = Dict.empty
            , hexColours = Dict.empty
            , referee = referee
            , hoveredStellarObject = Nothing
            }
    in
    ( model
    , Cmd.batch
        [ sendSolarSystemRequest ssReqEntry model.hostConfig model.hexRect
        , sendSectorRequest secReqEntry model.hostConfig
        , sendRegionRequest secReqEntry model.hostConfig -- Josh to fix later
        , sendRouteRequest routeReqEntry model.hostConfig
        ]
    )


rawHexagonPoint : Float -> Float -> ( Float, Float )
rawHexagonPoint size n =
    let
        x =
            size * cos (hexSizeFactor * n)

        y =
            size * sin (hexSizeFactor * n)
    in
    ( x, y )


hexagonPoints : ( Float, Float ) -> Float -> String
hexagonPoints ( xOrigin, yOrigin ) size =
    List.range 0 5
        |> List.map
            (toFloat
                >> rawHexagonPoint size
                >> (\( x, y ) -> String.fromFloat (xOrigin + x) ++ "," ++ String.fromFloat (yOrigin + y))
            )
        |> String.join " "


{-| hexagon points indepedent of origin
-}
rawHexagonPoints : Float -> List ( Float, Float )
rawHexagonPoints size =
    List.range 0 5
        |> List.map (toFloat >> rawHexagonPoint size)


{-| localize the hexagon points to the visualOrigin
-}
convertRawHexagonPoints : ( Float, Float ) -> List ( Float, Float ) -> String
convertRawHexagonPoints ( xOrigin, yOrigin ) points =
    points
        |> List.map
            (\( x, y ) -> String.fromFloat (xOrigin + x) ++ "," ++ String.fromFloat (yOrigin + y))
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


isOnRoute : RouteList -> HexAddress -> Bool
isOnRoute route address =
    List.any (\a -> a.address == address)
        route


viewHexEmpty : Int -> Int -> Int -> Int -> Float -> String -> String -> Svg Msg
viewHexEmpty hx hy x y size childSvgTxt hexColour =
    let
        origin =
            ( toFloat x, toFloat y )

        hexAddress =
            HexAddress hx hy

        childSvg =
            Svg.text_
                [ SvgAttrs.x <| String.fromInt x
                , SvgAttrs.y <| String.fromInt y
                , SvgAttrs.fontSize "10"
                , SvgAttrs.textAnchor "middle"
                ]
                [ Svg.text childSvgTxt ]
    in
    Svg.g
        [ SvgEvents.onMouseOver (HoveringHex hexAddress)
        , SvgEvents.onClick (ViewingHex hexAddress)
        , SvgEvents.onMouseUp MapMouseUp
        , -- listens for the JS 'mousedown' event and then runs the `downDecoder` on the JS Event, returning the Msg
          SvgEvents.on "mousedown" <| downDecoder MapMouseDown
        , SvgEvents.on "mousemove" <| moveDecoder MapMouseMove
        , SvgAttrs.style "cursor: pointer; user-select: none"
        , SvgAttrs.id <| "rendered-hex:" ++ HexAddress.toKey hexAddress
        ]
        [ -- background hex
          Svg.Lazy.lazy2 renderPolygon
            (hexagonPoints origin size)
            hexColour
        , Svg.text_
            [ SvgAttrs.x <| String.fromInt <| x
            , SvgAttrs.y <| String.fromInt <| y - (floor <| size * 0.65)
            , SvgAttrs.fontSize
                (if size > 15 then
                    "9"

                 else
                    "5"
                )
            , SvgAttrs.textAnchor "middle"
            , SvgAttrs.fontFamily "Tomorrow"
            , SvgAttrs.fontWeight "400"
            ]
            [ HexAddress.hexLabel hexAddress |> Svg.text
            ]
        , childSvg
        ]


renderPolyline : String -> String -> Svg msg
renderPolyline points_ borderColour =
    Svg.polyline
        [ points points_
        , SvgAttrs.stroke borderColour
        , SvgAttrs.fill "none"
        , SvgAttrs.strokeWidth "2"
        , SvgAttrs.pointerEvents "visiblePainted"
        ]
        []


renderPolygon : String -> String -> Svg msg
renderPolygon points_ fill =
    let
        borderColour =
            if fill == currentAddressHexBg then
                currentAddressHexBg

            else
                "#CCCCCC"

        strokeWidth =
            if fill == currentAddressHexBg then
                "2"

            else
                "1"

        hexColour =
            if fill == currentAddressHexBg then
                routeHexBg

            else
                fill
    in
    Svg.polygon
        [ points points_
        , SvgAttrs.fill hexColour
        , SvgAttrs.stroke borderColour
        , SvgAttrs.strokeWidth strokeWidth
        , SvgAttrs.pointerEvents "visiblePainted"
        , -- CSS class from index.html
          SvgAttrs.class "hex-hover"
        ]
        []


{-| a decoder that takes JSON and emits either a decode failure or a Msg
-}
downDecoder : (( Float, Float ) -> msg) -> JsDecode.Decoder msg
downDecoder onDownMsg =
    let
        -- takes a raw JS mouse event and turns it into a parsed Elm mouse event
        jsMouseEventDecoder =
            Html.Events.Extra.Mouse.eventDecoder
                |> JsDecode.andThen
                    (\evt ->
                        case evt.button of
                            Html.Events.Extra.Mouse.MainButton ->
                                JsDecode.succeed evt

                            _ ->
                                -- We fail decoding here, to signal to Elm that we don't want
                                --   to process the event.
                                -- So we'll never see the decoder failure, unlike our Codecs
                                JsDecode.fail "Won't drag on non-main/left button"
                    )
    in
    -- run the mouse event decoder
    jsMouseEventDecoder
        |> -- then if that succeeds, pass the event object into msgConstructor
           JsDecode.map (\evt -> onDownMsg evt.offsetPos)


clickDecoder : (( Float, Float ) -> msg) -> JsDecode.Decoder msg
clickDecoder onDownMsg =
    let
        -- takes a raw JS mouse event and turns it into a parsed Elm mouse event
        jsMouseEventDecoder =
            Html.Events.Extra.Mouse.eventDecoder
                |> JsDecode.andThen
                    (\evt ->
                        case evt.button of
                            Html.Events.Extra.Mouse.MainButton ->
                                JsDecode.succeed evt

                            _ ->
                                -- We fail decoding here, to signal to Elm that we don't want
                                --   to process the event.
                                -- So we'll never see the decoder failure, unlike our Codecs
                                JsDecode.fail "Won't drag on non-main/left button"
                    )
    in
    -- run the mouse event decoder
    jsMouseEventDecoder
        |> -- then if that succeeds, pass the event object into msgConstructor
           JsDecode.map (\evt -> onDownMsg evt.offsetPos)


mouseupDecoder : (( Float, Float ) -> msg) -> JsDecode.Decoder msg
mouseupDecoder onDownMsg =
    let
        -- takes a raw JS mouse event and turns it into a parsed Elm mouse event
        jsMouseEventDecoder =
            Html.Events.Extra.Mouse.eventDecoder
                |> JsDecode.andThen
                    (\evt ->
                        case evt.button of
                            Html.Events.Extra.Mouse.MainButton ->
                                JsDecode.succeed evt

                            _ ->
                                -- We fail decoding here, to signal to Elm that we don't want
                                --   to process the event.
                                -- So we'll never see the decoder failure, unlike our Codecs
                                JsDecode.fail "Won't drag on non-main/left button"
                    )
    in
    -- run the mouse event decoder
    jsMouseEventDecoder
        |> -- then if that succeeds, pass the event object into msgConstructor
           JsDecode.map (\evt -> onDownMsg evt.offsetPos)


moveDecoder : (( Float, Float ) -> msg) -> JsDecode.Decoder msg
moveDecoder onMoveMsg =
    -- equivalent to the `downDecoder`, only it returns `MapMouseMove` instead
    Html.Events.Extra.Mouse.eventDecoder
        |> JsDecode.map (.offsetPos >> onMoveMsg)


drawStar : ( Float, Float ) -> Int -> Float -> String -> Svg Msg
drawStar ( starX, starY ) radius size starColor =
    Svg.circle
        [ SvgAttrs.cx <| String.fromFloat <| starX
        , SvgAttrs.cy <| String.fromFloat <| starY
        , SvgAttrs.r <| String.fromFloat <| scaleAttr size radius
        , SvgAttrs.fill starColor
        , SvgAttrs.style "filter: drop-shadow( 2px 2px 2px rgba(0, 0, 0, .25))"
        ]
        []


renderHexWithStar : StarSystem -> String -> HexAddress -> VisualHexOrigin -> Float -> List ( Float, Float ) -> Svg Msg
renderHexWithStar starSystem hexColour hexAddress ( vox, voy ) size rawHexaPoints =
    let
        si =
            starSystem.surveyIndex

        showStar =
            starSystem.surveyIndex > 0
    in
    Svg.g
        [ SvgEvents.onMouseOver (HoveringHex hexAddress)
        , SvgEvents.onClick (ViewingHex hexAddress)
        , SvgEvents.onMouseUp MapMouseUp
        , -- listens for the JS 'mousedown' event and then runs the `downDecoder` on the JS Event, returning the Msg
          SvgEvents.on "mousedown" <| downDecoder MapMouseDown
        , SvgEvents.on "mousemove" <| moveDecoder MapMouseMove
        , SvgAttrs.style "cursor: pointer; user-select: none"
        ]
        [ -- background hex
          Svg.Lazy.lazy2 renderPolygon
            (convertRawHexagonPoints ( toFloat vox, toFloat voy ) rawHexaPoints)
            hexColour
        , -- center star
          if showStar then
            let
                primaryPos =
                    ( toFloat vox, toFloat voy )

                isKnown : StarType -> Bool
                isKnown theStar =
                    let
                        starData =
                            getStarTypeData theStar
                    in
                    if isBrownDwarfType starData then
                        starSystem.surveyIndex >= 4

                    else
                        starSystem.surveyIndex >= 1

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
                                [ drawStar starPos 7 size <| starColourRGB starData.colour
                                , drawStar compStarPos 3 size <| starColourRGB compStarData.colour
                                ]

                        Nothing ->
                            drawStar starPos 7 size <| starColourRGB starData.colour
            in
            Svg.g
                []
                (starSystem.stars
                    |> List.filter isKnown
                    |> List.indexedMap generateStar
                )

          else
            Svg.text_
                [ SvgAttrs.x <| String.fromInt <| vox
                , SvgAttrs.y <| String.fromInt <| voy - (floor <| size * 0.65)
                , SvgAttrs.fontSize
                    (if size > 15 then
                        "9"

                     else
                        "5"
                    )
                , SvgAttrs.textAnchor "middle"
                , SvgAttrs.fontFamily "Tomorrow"
                , SvgAttrs.fontWeight "400"
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
                            "9"

                         else
                            "5"
                        )
                    , SvgAttrs.textAnchor "middle"
                    , SvgAttrs.fontFamily "Tomorrow"
                    , SvgAttrs.fontWeight "400"
                    ]
                    [ HexAddress.hexLabel hexAddress |> Svg.text
                    ]
                , Svg.text_
                    [ SvgAttrs.x <| String.fromInt <| vox
                    , SvgAttrs.y <| String.fromInt <| voy + (floor <| size * 0.8) - 1
                    , SvgAttrs.fontSize "10"
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
                        [ SvgAttrs.fill "#109076", SvgAttrs.fontWeight "800" ]
                        [ showGasGiants |> Svg.text ]
                     , Svg.text "–"
                     , Svg.tspan
                        [ SvgAttrs.fill "#809076", SvgAttrs.fontWeight "800" ]
                        [ showTerrestrialPlanets |> Svg.text ]
                     , Svg.text "–"
                     , Svg.tspan
                        [ SvgAttrs.fill "#68B976", SvgAttrs.fontWeight "800" ]
                        [ showplanetoidBelts |> Svg.text ]
                     ]
                    )
                ]

          else
            Svg.text ""
        ]


defaultHexBg =
    "#f5f5f5"


selectedHexBg =
    "#a5f5a5"


routeHexBg =
    "#FCD299"


nativeSophontHexBg =
    "#B0E0E6"


extinctSophontHexBg =
    "#EEE8AA"


currentAddressHexBg =
    "#fe5a1d"


allegianceColours : Dict.Dict String String
allegianceColours =
    Dict.fromList
        [ ( "C", "#F0FFFF" )
        , ( "GR", "#FFDAB9" )
        , ( "AL", "#D2691E" )
        ]


defaultHexSize =
    40


isEmptyHex : Maybe a -> Int
isEmptyHex maybeSolarSystem =
    case maybeSolarSystem of
        Just _ ->
            1

        Nothing ->
            0


hexColOffset row =
    if remainderBy 2 row == 0 then
        1

    else
        0


calcVisualOrigin : Float -> { row : Int, col : Int } -> VisualHexOrigin
calcVisualOrigin hexSize { row, col } =
    let
        x =
            hexSize + toFloat col * (hexSize + hexSize * cos hexSizeFactor)

        y =
            -1 * (hexSize + toFloat row * 2 * hexSize * sin hexSizeFactor + hexSize * hexColOffset col * sin hexSizeFactor)
    in
    ( floor x, floor y )


viewHex :
    Float
    -> SolarSystemDict
    -> HexAddress
    -> VisualHexOrigin
    -> String
    -> List ( Float, Float )
    -> Svg Msg
viewHex hexSize solarSystemDict hexAddress visualHexOrigin hexColour rawHexaPoints =
    let
        solarSystem =
            Dict.get (HexAddress.toKey hexAddress) solarSystemDict

        ( vox, voy ) =
            visualHexOrigin

        viewEmptyHelper txt =
            Svg.Lazy.lazy7 viewHexEmpty hexAddress.x hexAddress.y vox voy hexSize txt hexColour

        hexSVG =
            case solarSystem of
                Just (LoadedSolarSystem ss) ->
                    Svg.Lazy.lazy6 renderHexWithStar
                        ss
                        hexColour
                        hexAddress
                        visualHexOrigin
                        hexSize
                        rawHexaPoints

                Just LoadingSolarSystem ->
                    viewEmptyHelper "Loading..."

                Just (FailedSolarSystem httpError) ->
                    viewEmptyHelper "Failed."

                Just LoadedEmptyHex ->
                    viewEmptyHelper ""

                Just (FailedStarsSolarSystem failedSolarSystem) ->
                    Svg.Lazy.lazy7 viewHexEmpty hexAddress.x hexAddress.y vox voy hexSize "Star Failed." "#aaaaaa"

                Nothing ->
                    viewEmptyHelper ""
    in
    hexSVG


type RemoteSolarSystem
    = LoadedSolarSystem StarSystem
    | LoadedEmptyHex
    | LoadingSolarSystem
    | FailedStarsSolarSystem FallibleStarSystem
    | FailedSolarSystem Http.Error


type alias SolarSystemDict =
    Dict.Dict String RemoteSolarSystem


type HorizontalOffsetDir
    = Top
    | Bottom


type VerticalOffsetDir
    = Left
    | Right


renderSectorOutline : Float -> SectorHexAddress -> Svg Msg
renderSectorOutline hexSize hex =
    let
        hWidth =
            hexWidth hexSize |> floor

        hHeight =
            hexHeight hexSize |> floor

        topLeft : HexAddress
        topLeft =
            { hex | x = 0, y = 0 } |> HexAddress.toUniversalAddress

        botRight : HexAddress
        botRight =
            { hex | x = 32, y = 40 } |> HexAddress.toUniversalAddress

        topRight : HexAddress
        topRight =
            { hex | x = 32, y = 0 } |> HexAddress.toUniversalAddress

        botLeft : HexAddress
        botLeft =
            { hex | x = 0, y = 40 } |> HexAddress.toUniversalAddress

        computePoints hexAddr =
            calcVisualOrigin hexSize
                { row = hexAddr.y, col = hexAddr.x }
                |> (\( x, y ) ->
                        ( x - hWidth // 2, y - hHeight // 2 )
                   )
                |> (\( x, y ) ->
                        (x |> String.fromInt)
                            ++ ", "
                            ++ (y |> String.fromInt)
                   )

        points_ =
            List.map computePoints [ topLeft, topRight, botRight, botLeft, topLeft ]
                |> String.join " "
    in
    Svg.polyline
        [ points points_
        , SvgAttrs.id "sectorOutline"
        , SvgAttrs.stroke "#0a0a0a40"
        , SvgAttrs.fill "none"
        , SvgAttrs.strokeWidth "3"
        , SvgAttrs.pointerEvents "visiblePainted"
        ]
        []


regionLabel x y name =
    Svg.text_
        [ SvgAttrs.x <| String.fromInt x
        , SvgAttrs.y <| String.fromInt y
        , SvgAttrs.textAnchor "middle"
        , SvgAttrs.dominantBaseline "middle"
        , SvgAttrs.fontFamily "Tomorrow"
        , SvgAttrs.fontWeight "500"
        , SvgAttrs.fill "#0A0A0A"
        , SvgAttrs.style "pointer-events: none; user-select: none;"
        ]
        [ Svg.text name ]


hexBackgroundColour : Bool -> String -> SolarSystemDict -> String
hexBackgroundColour referee hexKey solarSystemDict =
    if referee then
        case Dict.get hexKey solarSystemDict of
            Just rss ->
                case rss of
                    LoadedSolarSystem system ->
                        case system.allegiance of
                            Just allegiance ->
                                case Dict.get allegiance allegianceColours of
                                    Just color ->
                                        color

                                    Nothing ->
                                        nativeSophontHexBg

                            Nothing ->
                                if system.nativeSophont then
                                    nativeSophontHexBg

                                else if system.extinctSophont then
                                    extinctSophontHexBg

                                else
                                    defaultHexBg

                    _ ->
                        defaultHexBg

            Nothing ->
                defaultHexBg

    else
        defaultHexBg


viewHexes :
    ( HexRect, List ( Float, Float ) )
    -> { screenVp : Browser.Dom.Viewport, hexmapVp : Maybe Browser.Dom.Viewport }
    -> { solarSystemDict : SolarSystemDict, hexColours : HexColorDict, regionLabels : RegionLabelDict }
    -> ( RouteList, HexAddress )
    -> Float
    -> Maybe HexAddress
    -> Bool
    -> Html Msg
viewHexes ( { upperLeftHex, lowerRightHex }, rawHexaPoints ) { screenVp, hexmapVp } { solarSystemDict, hexColours, regionLabels } ( route, currentAddress ) hexSize maybeSelectedHex referee =
    let
        svgHeight =
            screenVp.viewport.height - consoleTitleHeight

        svgWidth =
            screenVp.viewport.width - 420

        renderCurrentAddressOutline : HexAddress -> Svg Msg
        renderCurrentAddressOutline ca =
            let
                locationOrigin =
                    calcVisualOrigin hexSize
                        { row = upperLeftHex.y - ca.y, col = ca.x - upperLeftHex.x }
                        |> (\( x, y ) ->
                                ( toFloat <| x
                                , toFloat <| y
                                )
                           )

                points =
                    case String.split " " <| hexagonPoints locationOrigin hexSize of
                        first :: points_ ->
                            (first :: points_) ++ [ first ]

                        other ->
                            other
            in
            Svg.Lazy.lazy2 renderPolyline
                (points |> String.join " ")
                currentAddressHexBg

        widestViewport =
            case hexmapVp of
                Nothing ->
                    screenVp

                Just hexmapViewport ->
                    hexmapViewport

        hexRange =
            HexAddress.betweenWithMax
                (HexAddress.shiftAddressBy { deltaX = -1, deltaY = -1 } upperLeftHex)
                lowerRightHex
                { maxAcross = maxAcross, maxTall = maxTall }

        ( visualHexWidth, visualHexHeight ) =
            let
                ( left_x, left_y ) =
                    calcVisualOrigin hexSize { row = 1, col = 1 }

                ( right_x, _ ) =
                    calcVisualOrigin hexSize { row = 1, col = 2 }

                ( _, down_y ) =
                    calcVisualOrigin hexSize { row = 2, col = 1 }
            in
            ( left_x - right_x |> abs, down_y - left_y |> abs )

        ( maxAcross, maxTall ) =
            case hexmapVp of
                Nothing ->
                    ( 10000, 10000 )

                Just hvp ->
                    ( (hvp.viewport.width / toFloat visualHexWidth) + 2 |> floor
                    , (hvp.viewport.height / toFloat visualHexHeight) + 2 |> floor
                    )
    in
    hexRange
        |> List.map
            (\hexAddr ->
                let
                    hexKey =
                        HexAddress.toKey hexAddr

                    hexSVGOrigin =
                        calcVisualOrigin hexSize
                            { row = hexAddr.y, col = hexAddr.x }

                    hexColour =
                        if hexAddr == currentAddress then
                            currentAddressHexBg

                        else if isOnRoute route hexAddr then
                            routeHexBg

                        else
                            case Dict.get hexKey hexColours of
                                Just color ->
                                    Color.Convert.colorToHex <| color

                                Nothing ->
                                    case maybeSelectedHex of
                                        Just selectedHex ->
                                            if selectedHex == hexAddr then
                                                selectedHexBg

                                            else
                                                hexBackgroundColour referee hexKey solarSystemDict

                                        Nothing ->
                                            hexBackgroundColour referee hexKey solarSystemDict

                    hexSvgsWithHexAddr =
                        ( hexAddr
                        , viewHex
                            hexSize
                            solarSystemDict
                            hexAddr
                            hexSVGOrigin
                            hexColour
                            rawHexaPoints
                        )
                in
                hexSvgsWithHexAddr
            )
        |> (\hexSvgsWithHexAddress ->
                let
                    labelPos hexAddr =
                        calcVisualOrigin hexSize
                            { row = hexAddr.y, col = hexAddr.x }

                    renderRegionLabel : HexAddress -> Maybe (Svg.Svg msg)
                    renderRegionLabel hexAddress =
                        regionLabels
                            |> Dict.get (HexAddress.toKey hexAddress)
                            |> Maybe.map
                                (\name ->
                                    let
                                        ( x, y ) =
                                            labelPos hexAddress
                                    in
                                    Html.Lazy.lazy3 regionLabel x y name
                                )

                    labels =
                        hexRange
                            |> List.filterMap renderRegionLabel
                in
                ( hexSvgsWithHexAddress, labels )
           )
        |> (\( hexSvgsWithHexAddress, labels ) ->
                let
                    singlePolyHex =
                        renderCurrentAddressOutline currentAddress

                    keyedHexes : Svg Msg
                    keyedHexes =
                        Svg.Keyed.node "g" [] <|
                            List.map (Tuple.mapFirst HexAddress.toKey) hexSvgsWithHexAddress
                in
                [ keyedHexes
                , renderSectorOutline hexSize (upperLeftHex |> HexAddress.toSectorAddress)
                , renderSectorOutline hexSize (lowerRightHex |> HexAddress.toSectorAddress)
                , singlePolyHex
                ]
                    ++ labels
           )
        |> (let
                widthString =
                    String.fromFloat <| svgWidth

                heightString =
                    String.fromFloat <| svgHeight
            in
            Svg.svg
                [ SvgAttrs.width <| widthString
                , SvgAttrs.height <| heightString
                , SvgAttrs.id "hexmap"
                , SvgEvents.onMouseOut MapMouseLeave
                , viewBox <|
                    toViewBox hexSize upperLeftHex
                        ++ " "
                        ++ widthString
                        ++ " "
                        ++ heightString
                ]
           )


toViewBox : Float -> HexAddress -> String
toViewBox hexScale { x, y } =
    calcVisualOrigin hexScale { col = x, row = y }
        |> (\( x_, y_ ) ->
                String.fromFloat (toFloat x_)
                    ++ " "
                    ++ String.fromFloat (toFloat y_)
           )


orbitStyle : List (Element.Attribute msg)
orbitStyle =
    [ width <| Element.px 45
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
    [ width <| Element.px 62
    , Font.size 12
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


renderOrbit : Float -> Float -> Maybe Float -> Bool -> Element.Element msg
renderOrbit au hzcoDeviation maybeTemp isReferee =
    let
        roundedAU =
            if au < 1 then
                Round.round 2 au

            else if au < 100 then
                Round.round 1 au

            else
                Round.round 0 au

        zoneImage =
            if abs hzcoDeviation <= 0.2 then
                "🌐"

            else if abs hzcoDeviation <= 1 then
                if hzcoDeviation > 0 then
                    "🔥"

                else
                    "❄"

            else
                ""

        style =
            case maybeTemp of
                Just temp ->
                    orbitStyle ++ [ Element.htmlAttribute <| HtmlAttrs.title <| Round.round 0 (temp - 237) ++ " ºC" ]

                Nothing ->
                    orbitStyle
    in
    Element.el
        style
        (monospaceText <| roundedAU ++ zoneImage)


renderOrbitSequence : String -> Element.Element msg
renderOrbitSequence sequence =
    Element.el
        sequenceStyle
        (monospaceText <| sequence)


renderSODescription : String -> String -> Element.Element Msg
renderSODescription description orbitSequence =
    Element.el
        descriptionStyle
        (monospaceText <| description)
        |> Element.el
            [ Events.onMouseLeave <| HoveredStellarObject Nothing
            , Events.onMouseEnter <| HoveredStellarObject (Just orbitSequence)
            ]


iconSizing : List (Element.Attribute msg)
iconSizing =
    [ Element.height <| Element.px 16, Element.width <| Element.px 16 ]


{-| Zero spacing for padding
Use with elm-ui to easily add padding to an element. e.g.

        Element.paddingEach { zeroEach | left = 4 }
        Border.roundEach { zeroEach | right = 2 }
        Border.widthEach { zeroEach | top = 2 }

-}
zeroEach : { top : number, left : number, bottom : number, right : number }
zeroEach =
    { top = 0, left = 0, bottom = 0, right = 0 }


renderIcon : Icon a -> Element.Element Msg
renderIcon icon =
    let
        iconSpacing =
            { zeroEach | right = 4 }
    in
    icon
        |> Icon.view
        |> Element.html
        |> Element.el (Element.paddingEach iconSpacing :: iconSizing)


renderJumpTime : Maybe Float -> String -> Element.Element Msg
renderJumpTime maxJumpTime time =
    Element.row safeJumpStyle
        [ renderIcon Icon.arrowUpFromBracket
        , text <|
            case maxJumpTime of
                Just maxTime ->
                    secondsToDaysWatches maxTime

                Nothing ->
                    time
        ]


renderImage : String -> Maybe Float -> Element.Element Msg
renderImage uwp maybeTemp =
    let
        gasGiantUwps =
            [ "GS", "GM", "GL" ]

        hydrographics =
            if String.length uwp == 9 then
                String.slice 3 4 uwp

            else
                ""

        atmosphere =
            if String.length uwp == 9 then
                String.slice 2 3 uwp

            else
                ""

        imageUrl =
            if List.member uwp gasGiantUwps then
                "public/gasgiant-small.png"

            else if atmosphere == "B" || atmosphere == "C" then
                "public/corrosivehellworld-small.png"

            else if hydrographics == "A" then
                "public/waterworld-small.png"

            else if hydrographics == "0" then
                "public/desertworld-small.png"

            else if atmosphere == "1" || atmosphere == "2" || atmosphere == "3" then
                "public/traceworld-small.png"

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
                    [ renderIcon Icon.upDown
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


travelTimeInSeconds : Float -> Int -> Float
travelTimeInSeconds kms mdrive =
    2 * sqrt (kms * 1000 / (toFloat mdrive * 9.8))


secondsToDaysWatches : Float -> String
secondsToDaysWatches secs =
    let
        watches =
            toFloat <| ceiling <| secs / (60 * 60 * 8)

        days =
            toFloat <| floor <| watches / 3

        watches_ =
            watches - days * 3
    in
    Round.floor 0 days ++ "d " ++ Round.ceiling 0 watches_ ++ "w"


travelTime : Float -> Int -> Bool -> String
travelTime kms mdrive useHours =
    let
        rawSeconds =
            travelTimeInSeconds kms mdrive
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
        secondsToDaysWatches rawSeconds



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


renderGasGiant : Int -> GasGiantData -> JumpShadowCheckers -> Maybe StellarObject -> Bool -> Element.Element Msg
renderGasGiant newNestingLevel gasGiantData jumpShadowCheckers selectedStellarObject isReferee =
    let
        stellarObject =
            GasGiant gasGiantData

        maxShadow =
            List.maximum <|
                List.filterMap (\checker -> checker stellarObject) jumpShadowCheckers

        orbit =
            renderOrbit gasGiantData.au gasGiantData.effectiveHZCODeviation Nothing isReferee
    in
    row
        [ Element.spacing 8
        , Element.moveRight <| calcNestedOffset newNestingLevel
        , Font.size 14
        , Events.onClick <| FocusInSidebar stellarObject
        ]
        [ orbit
        , renderOrbitSequence gasGiantData.orbitSequence
        , renderSODescription gasGiantData.code gasGiantData.orbitSequence
        , renderImage gasGiantData.code Nothing
        , renderJumpTime maxShadow gasGiantData.safeJumpTime
        , renderTravelTime stellarObject selectedStellarObject
        ]


uwpBreakdown : UWP -> List ( String, String )
uwpBreakdown uwp =
    [ ( "Starport", Starport.description uwp.starport )
    , ( "Size", sizeDescription uwp.size )
    , ( "Atmosphere", atmosphereDescription uwp.atmosphere )
    , ( "Hydrosphere", hydrosphereDescription uwp.hydrosphere )
    , ( "Population", populationDescription uwp.population )
    , ( "Government", Government.description uwp.government )
    , ( "Law level", LawLevel.description uwp.lawLevel )
    , ( "Tech level", TechLevel.description uwp.techLevel )
    ]


uwpExplainer : String -> Element.Element Msg
uwpExplainer uwpString =
    let
        parsedUWP =
            Parser.run uwp uwpString
    in
    case parsedUWP of
        Ok theUWP ->
            column
                [ Background.color <| colorToElementColor deepnightGray
                , Element.width <| Element.px 300
                , Element.moveDown 20
                , Element.padding 1
                , Border.rounded 3
                , Border.widthEach { zeroEach | top = 2 }
                , Border.color <| colorToElementColor deepnightColor
                , Border.glow (Element.rgba255 0 0 0 100) 6
                , Font.color <| Element.rgb 1 1 1
                , Font.shadow
                    { offset = ( 1, 1 )
                    , blur = 1
                    , color = Element.rgb 0 0 0
                    }
                ]
                [ Element.table [ Element.spacing 4 ]
                    { columns =
                        [ { header = Element.text "", width = Element.shrink, view = \u -> text <| Tuple.first u }
                        , { header = Element.text "", width = Element.fill, view = \u -> Element.paragraph [] [ text <| Tuple.second u ] }
                        ]
                    , data = uwpBreakdown theUWP
                    }
                ]

        _ ->
            text ("Could not parse " ++ uwpString)


renderTerrestrialPlanet : Int -> TerrestrialData -> JumpShadowCheckers -> Maybe StellarObject -> Bool -> Element.Element Msg
renderTerrestrialPlanet newNestingLevel terrestrialData jumpShadowCheckers selectedStellarObject isReferee =
    let
        planet =
            TerrestrialPlanet terrestrialData

        maxShadow =
            List.maximum <| List.filterMap (\checker -> checker planet) jumpShadowCheckers

        orbit =
            renderOrbit terrestrialData.au terrestrialData.effectiveHZCODeviation terrestrialData.meanTemperature isReferee
    in
    row
        [ Element.spacing 8
        , Element.moveRight <| calcNestedOffset newNestingLevel
        , Font.size 14
        , Events.onClick <| FocusInSidebar planet
        ]
        [ orbit
        , renderOrbitSequence terrestrialData.orbitSequence
        , renderSODescription terrestrialData.uwp terrestrialData.orbitSequence
        , renderImage terrestrialData.uwp terrestrialData.meanTemperature
        , renderJumpTime maxShadow terrestrialData.safeJumpTime
        , renderTravelTime planet selectedStellarObject
        ]


renderPlanetoidBelt : Int -> PlanetoidBeltData -> JumpShadowCheckers -> Maybe StellarObject -> Bool -> Element.Element Msg
renderPlanetoidBelt newNestingLevel planetoidBeltData jumpShadowCheckers selectedStellarObject isReferee =
    let
        belt =
            PlanetoidBelt planetoidBeltData

        maxShadow =
            List.maximum <| List.filterMap (\checker -> checker belt) jumpShadowCheckers

        orbit =
            renderOrbit planetoidBeltData.au planetoidBeltData.effectiveHZCODeviation Nothing isReferee
    in
    row
        [ Element.spacing 8
        , Element.moveRight <| calcNestedOffset newNestingLevel
        , Font.size 14
        , Events.onClick <| FocusInSidebar belt
        ]
        [ orbit
        , renderOrbitSequence planetoidBeltData.orbitSequence
        , renderSODescription planetoidBeltData.uwp planetoidBeltData.orbitSequence
        , renderImage planetoidBeltData.uwp Nothing
        , renderJumpTime maxShadow planetoidBeltData.safeJumpTime
        , renderTravelTime belt selectedStellarObject
        ]


renderPlanetoid : Int -> PlanetoidData -> JumpShadowCheckers -> Maybe StellarObject -> Bool -> Element.Element Msg
renderPlanetoid newNestingLevel planetoidData jumpShadowCheckers selectedStellarObject isReferee =
    let
        planet =
            Planetoid planetoidData

        maxShadow =
            List.maximum <| List.filterMap (\checker -> checker planet) jumpShadowCheckers

        orbit =
            renderOrbit planetoidData.au planetoidData.effectiveHZCODeviation planetoidData.meanTemperature isReferee
    in
    row
        [ Element.spacing 8
        , Element.moveRight <| calcNestedOffset newNestingLevel
        , Font.size 14
        , Events.onClick <| FocusInSidebar planet
        ]
        [ orbit
        , renderOrbitSequence planetoidData.orbitSequence
        , renderSODescription planetoidData.uwp planetoidData.orbitSequence
        , renderImage planetoidData.uwp planetoidData.meanTemperature
        , renderJumpTime maxShadow planetoidData.safeJumpTime
        , renderTravelTime planet selectedStellarObject
        ]


renderStellarObject : Int -> Int -> StellarObject -> JumpShadowCheckers -> Maybe StellarObject -> Maybe String -> Bool -> Element.Element Msg
renderStellarObject surveyIndex newNestingLevel stellarObject jumpShadowCheckers selectedStellarObject hoveredStellarObject isReferee =
    let
        uwpHover orbitSequence uwp =
            el
                [ Element.below <|
                    case hoveredStellarObject of
                        Just hPlanet ->
                            if hPlanet == orbitSequence then
                                uwpExplainer uwp

                            else
                                Element.none

                        Nothing ->
                            Element.none
                ]
    in
    row
        [ Element.spacing 8
        , Font.size 14
        , Element.width Element.fill
        ]
        [ case stellarObject of
            GasGiant gasGiantData ->
                renderGasGiant newNestingLevel gasGiantData jumpShadowCheckers selectedStellarObject isReferee

            TerrestrialPlanet terrestrialData ->
                uwpHover terrestrialData.orbitSequence terrestrialData.uwp <|
                    renderTerrestrialPlanet newNestingLevel terrestrialData jumpShadowCheckers selectedStellarObject isReferee

            PlanetoidBelt planetoidBeltData ->
                uwpHover planetoidBeltData.orbitSequence planetoidBeltData.uwp <|
                    renderPlanetoidBelt newNestingLevel planetoidBeltData jumpShadowCheckers selectedStellarObject isReferee

            Planetoid planetoidData ->
                uwpHover planetoidData.orbitSequence planetoidData.uwp <|
                    renderPlanetoid newNestingLevel planetoidData jumpShadowCheckers selectedStellarObject isReferee

            Star starDataConfig ->
                el [ Element.width Element.fill, Element.paddingEach { top = 0, left = 0, right = 0, bottom = 5 } ] <|
                    displayStarDetails surveyIndex starDataConfig newNestingLevel jumpShadowCheckers selectedStellarObject hoveredStellarObject isReferee
        ]


displayStarDetails : Int -> StarData -> Int -> JumpShadowCheckers -> Maybe StellarObject -> Maybe String -> Bool -> Element.Element Msg
displayStarDetails surveyIndex (StarDataWrap starData) nestingLevel jumpShadowCheckers selectedStellarObject hoveredStellarObject isReferee =
    let
        inJumpShadow obj =
            case starData.jumpShadow of
                Just jumpShadow ->
                    jumpShadow >= (getStellarOrbit obj).au

                Nothing ->
                    False

        isKnown obj =
            case obj of
                GasGiant _ ->
                    surveyIndex >= 5

                TerrestrialPlanet _ ->
                    surveyIndex >= 6

                PlanetoidBelt _ ->
                    surveyIndex >= 6

                Planetoid _ ->
                    surveyIndex >= 6

                Star childStar ->
                    if isBrownDwarf <| getInnerStarData childStar then
                        surveyIndex >= 4

                    else
                        surveyIndex >= 3

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
        , Border.rounded 10
        , Element.width <| Element.minimum 200 Element.fill
        , Element.spacing 10
        , Element.paddingXY 0 5
        ]
        [ row [ Element.paddingXY 10 0, Font.alignLeft, Element.alignLeft ]
            [ if starData.orbitPosition.x == 0 && starData.orbitPosition.y == 0 then
                Element.none

              else
                renderRawOrbit starData.au
            , el [ Font.alignLeft, Element.alignLeft, Font.size 16, Font.bold ] <|
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
                    displayStarDetails surveyIndex compStarData nextNestingLevel jumpShadowCheckers selectedStellarObject hoveredStellarObject isReferee
                )
            |> Maybe.withDefault Element.none
        , column [ Element.paddingXY 10 0, Element.width Element.fill ] <|
            let
                red =
                    Element.el
                        [ width <| Element.fill
                        , height <| Element.px 4
                        , Element.centerY
                        , Border.rounded 2
                        , Background.gradient
                            { angle = pi / 2.0
                            , steps =
                                [ travellerRed
                                , Element.rgba 0 0 0 0.25
                                , travellerRed
                                ]
                            }
                        ]
                    <|
                        text <|
                            " "
            in
            [ starData.stellarObjects
                |> List.filter inJumpShadow
                |> List.filter isKnown
                |> List.map (\so -> renderStellarObject surveyIndex nextNestingLevel so jumpShadowCheckers selectedStellarObject hoveredStellarObject isReferee)
                |> column []
            , column [ Font.size 14, Font.shadow { blur = 1, color = jumpShadowTextColor, offset = ( 0.5, 0.5 ) }, Element.width Element.fill, Element.behindContent red ]
                [ case starData.jumpShadow of
                    Just jumpShadow ->
                        Element.el [ Element.centerX ] <| text <| Round.round 2 jumpShadow

                    Nothing ->
                        text ""
                ]
            , starData.stellarObjects
                |> List.filter (not << inJumpShadow)
                |> List.filter isKnown
                |> List.map (\so -> renderStellarObject surveyIndex nextNestingLevel so jumpShadowCheckers selectedStellarObject hoveredStellarObject isReferee)
                |> column []
            ]
        ]


convertColor : Color.Color -> Element.Color
convertColor color =
    Element.fromRgb <| Color.toRgba <| color


type alias JumpShadowChecker =
    StellarObject -> Maybe Float


type alias JumpShadowCheckers =
    List JumpShadowChecker


viewSystemDetailsSidebar : SolarSystem -> Maybe StellarObject -> Maybe String -> Bool -> Element Msg
viewSystemDetailsSidebar solarSystem selectedStellarObject hoveredStellarObject isReferee =
    let
        jumpShadowCheckers : JumpShadowCheckers
        jumpShadowCheckers =
            List.filterMap
                (getStarData >> Maybe.map getInnerStarData)
                (Star solarSystem.primaryStar :: (getInnerStarData solarSystem.primaryStar).stellarObjects)
                |> List.map
                    (\star so ->
                        let
                            objToStarKMs =
                                calcDistance2F star.orbitPosition (getStellarOrbit so).orbitPosition

                            jumpShadowDistanceKMs =
                                auToKMs (Maybe.withDefault 0 star.jumpShadow)
                        in
                        if objToStarKMs > jumpShadowDistanceKMs then
                            Nothing

                        else
                            Just <| travelTimeInSeconds (jumpShadowDistanceKMs - objToStarKMs) 4
                    )
    in
    displayStarDetails
        solarSystem.surveyIndex
        solarSystem.primaryStar
        0
        jumpShadowCheckers
        selectedStellarObject
        hoveredStellarObject
        isReferee


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


deepnightColor : Color.Color
deepnightColor =
    Color.rgb255 223 127 51


deepnightLightGray : Color.Color
deepnightLightGray =
    Color.rgb255 167 180 183


deepnightGray : Color.Color
deepnightGray =
    Color.rgb255 121 137 144



--Color.rgb 1.0 0.498 0.0


fontDarkTextColor : Element.Color
fontDarkTextColor =
    textColor
        |> Color.Manipulate.desaturate 0.85
        |> Color.Manipulate.darken 0.25
        |> colorToElementColor


jumpShadowTextColor : Element.Color
jumpShadowTextColor =
    textColor
        |> Color.Manipulate.desaturate 0.85
        |> Color.Manipulate.darken 0.85
        |> colorToElementColor


travellerRed : Element.Color
travellerRed =
    Element.rgb 0.882 0.024 0


universalHexLabel : SectorDict -> HexAddress -> String
universalHexLabel sectors hexAddress =
    case Dict.get (HexAddress.toSectorKey <| HexAddress.toSectorAddress hexAddress) sectors of
        Nothing ->
            " "

        Just sector ->
            sector.name ++ " " ++ HexAddress.hexLabel hexAddress


universalHexLabelMaybe : SectorDict -> HexAddress -> Maybe String
universalHexLabelMaybe sectors hexAddress =
    sectors
        |> Dict.get (HexAddress.toSectorKey <| HexAddress.toSectorAddress hexAddress)
        |> Maybe.map (\sector -> sector.name ++ " " ++ HexAddress.hexLabel hexAddress)


errorDialog : List ( Http.Error, String ) -> Html Msg
errorDialog httpErrors =
    let
        openAttr =
            if (not << List.isEmpty) httpErrors then
                HtmlAttrs.attribute "open" "open"

            else
                HtmlAttrs.classList []

        errorButton { onPress, label } =
            Input.button
                [ Element.width <| Element.px 100
                , Border.width 2
                , Border.rounded 10
                , Element.padding 10
                , Element.mouseOver [ Background.color <| Element.rgb 0.5 0.5 0.5 ]
                ]
                { onPress = onPress, label = el [ centerX ] <| text label }

        pluralize n singular plural =
            if n == 1 then
                singular

            else
                plural

        renderError ( httpError, url ) =
            column []
                [ -- clickable url
                  Element.link []
                    { url = url
                    , label =
                        el
                            [ Element.mouseOver [ Font.color <| colorToElementColor Color.green ]
                            , Font.italic
                            , Font.color <| colorToElementColor Color.grey
                            ]
                        <|
                            monospaceText url
                    }
                , case httpError of
                    Http.BadBody error ->
                        Element.textColumn []
                            [ monospaceText "JSON Decode Error:"
                            , monospaceText error
                            ]

                    Http.BadUrl url_ ->
                        text <| "Invalid URL: " ++ url_

                    Http.NetworkError ->
                        text "Network Error"

                    Http.BadStatus statusCode ->
                        text <| "BadStatus: " ++ String.fromInt statusCode

                    Http.Timeout ->
                        text "Request timedout"
                ]
    in
    Html.node "dialog"
        [ openAttr ]
        [ Element.layoutWith { options = [ Element.noStaticStyleSheet ] }
            [ Element.centerX
            , width fill
            ]
          <|
            column
                [ Element.height <| Element.minimum 500 <| Element.fill
                , Element.width <| Element.minimum 500 <| Element.fill
                , Font.color <| Element.rgb 1 1 1
                , Element.scrollbars
                , Element.spacing 10
                ]
                [ -- header
                  el [ centerX, Font.size 24, Font.underline, Element.padding 10 ] <|
                    let
                        errorCount =
                            List.length httpErrors
                    in
                    text <|
                        pluralize
                            errorCount
                            "One Error!"
                            ("Many errors! (" ++ String.fromInt errorCount ++ " of 'em)")
                , -- error renderer
                  column
                    [ Element.spacing 10
                    , Element.height <| Element.minimum 100 <| Element.fill
                    , width fill
                    , Element.scrollbars
                    , Font.size 16
                    ]
                  <|
                    List.map renderError httpErrors
                , --buttons
                  row [ centerX, Element.spacing 10 ]
                    [ errorButton { onPress = Just ClearAllErrors, label = "Close" }
                    ]
                ]
        ]


renderFAIcon : String -> Int -> Element.Element Msg
renderFAIcon icon size =
    Element.el
        [ Element.width (Element.px size)
        , Element.height (Element.px size)
        ]
    <|
        Element.html <|
            Html.i
                [ HtmlAttrs.style "font-size" (String.fromInt size ++ "px"), HtmlAttrs.class icon ]
                []


type alias HexRect =
    { upperLeftHex : HexAddress, lowerRightHex : HexAddress }


type alias XY a =
    { x : a, y : a }


toXY : ( a, a ) -> XY a
toXY ( left, right ) =
    { x = left, y = right }


type alias ImageSize =
    { width : Float, height : Float }


mouseCoordsToSector : XY Float -> XY Float -> ImageSize -> XY Int
mouseCoordsToSector mousePos offset imageSize =
    let
        ( sectorsAcross, sectorsTall ) =
            ( 17, 14 )

        ( correctedX, correctedY ) =
            let
                ( officialX, officialY ) =
                    ( -21, -2 )

                ( oursX, oursY ) =
                    ( 5, 1 )
            in
            ( officialX - oursX, officialY + oursY )

        ( sectorX, sectorY ) =
            ( (mousePos.x - offset.x) / (imageSize.width / sectorsAcross)
            , (mousePos.y - offset.y) / (imageSize.height / sectorsTall)
            )
    in
    { x = correctedX + floor sectorX, y = correctedY - floor sectorY }


viewFullJourney : JourneyModel -> Browser.Dom.Viewport -> Element.Element Msg
viewFullJourney model viewport =
    let
        ( maxWidth, maxHeight ) =
            let
                scaledWidth =
                    viewport.viewport.width - sidebarWidth - 50
            in
            ( scaledWidth
            , scaledWidth * (fullJourneyImageHeight / fullJourneyImageWidth)
            )

        ( imageSizeWidth, imageSizeHeight ) =
            ( maxWidth * model.zoomScale
            , maxHeight * model.zoomScale
            )

        ( offsetLeft, offsetTop ) =
            model.zoomOffset
    in
    el
        [ Element.alignTop
        , width <| Element.px <| floor maxWidth
        , height <| Element.px <| floor maxHeight
        , Element.clip
        , Element.htmlAttribute <| Html.Events.on "mousemove" <| moveDecoder (JourneyMsg << MouseMove)
        , Element.htmlAttribute <| Html.Events.on "mousedown" <| downDecoder (JourneyMsg << MouseDown)
        , Element.htmlAttribute <| Html.Events.on "mouseup" <| mouseupDecoder (JourneyMsg << MouseUp)
        , Events.onMouseLeave (JourneyMsg MouseLeave)
        , Background.color <| Element.rgb 1.0 0.498 0.0
        ]
    <|
        Element.image
            [ width <| Element.px <| floor <| imageSizeWidth
            , height <| Element.px <| floor <| imageSizeHeight
            , Element.moveRight <| offsetLeft
            , Element.moveDown <| offsetTop
            , pointerEventsNone
            , userSelectNone
            , case model.hoverPoint of
                Just ( hovX, hovY ) ->
                    Element.inFront <|
                        let
                            ( sectorsAcross, sectorsTall ) =
                                ( 17, 14 )

                            ( correctedX, correctedY ) =
                                let
                                    ( officialX, officialY ) =
                                        ( -21, -2 )

                                    ( oursX, oursY ) =
                                        ( 5, 1 )
                                in
                                ( officialX - oursX, officialY + oursY )

                            ( sectorX, sectorY ) =
                                ( (hovX - offsetLeft) / (imageSizeWidth / sectorsAcross)
                                , (hovY - offsetTop) / (imageSizeHeight / sectorsTall)
                                )

                            ( xoff, yoff ) =
                                ( (toFloat <| floor sectorX)
                                    * (imageSizeWidth / sectorsAcross)
                                , (toFloat <| floor sectorY)
                                    * (imageSizeHeight / sectorsTall)
                                )
                        in
                        Element.el
                            [ Element.moveRight xoff
                            , Element.moveDown yoff
                            , userSelectNone
                            , pointerEventsNone
                            , Font.size 14
                            ]
                        <|
                            text <|
                                "Sector: "
                                    ++ String.fromInt (correctedX + floor sectorX)
                                    ++ ", "
                                    ++ String.fromInt (correctedY - floor sectorY)

                Nothing ->
                    noopAttribute
            ]
            { src = "public/uncharted-space.png", description = "Full Journey Map" }


pointerEventsNone =
    Element.htmlAttribute <| HtmlAttrs.style "pointer-events" "none"


userSelectNone =
    Element.htmlAttribute <| HtmlAttrs.style "user-select" "none"


{-| Element attribute that does nothing
-}
noopAttribute : Element.Attribute msg
noopAttribute =
    Element.htmlAttribute <| HtmlAttrs.style "" ""


conditionalAttribute : Bool -> Element.Attribute msg -> Element.Attribute msg
conditionalAttribute condition attribute =
    if condition then
        attribute

    else
        noopAttribute


viewStatusRow : Model -> Element.Element Msg
viewStatusRow model =
    let
        extras =
            case model.viewMode of
                HexMap ->
                    [ -- hovered hex
                      el
                        [ uiDeepnightColorFontColour
                        , Font.size 14
                        , Element.spacing 5
                        , Element.pointer
                        , Events.onClick DownloadSolarSystems
                        , Element.alignBottom
                        , Element.htmlAttribute <| HtmlAttrs.title "Refresh map"
                        , Element.mouseOver
                            [ Font.color <| convertColor (Color.Manipulate.lighten 0.25 deepnightColor)
                            ]
                        ]
                      <|
                        renderFAIcon "fa-regular fa-refresh" 14
                    , el
                        [ uiDeepnightColorFontColour
                        , Font.family [ Font.monospace ]
                        , Font.size 14
                        , Element.alignBottom
                        , Element.width <| Element.minimum 10 Element.shrink
                        ]
                      <|
                        case model.hoveringHex of
                            Just hoveringHex ->
                                text <| universalHexLabel model.sectors hoveringHex

                            Nothing ->
                                Element.none
                    , -- hex rect display
                      el [ Element.alignBottom, Font.size 14, uiDeepnightColorFontColour, Element.centerX ] <|
                        text <|
                            let
                                first =
                                    shiftAddressBy { deltaX = 1, deltaY = 1 } model.hexRect.upperLeftHex

                                last =
                                    shiftAddressBy { deltaX = -3, deltaY = -1 } model.hexRect.lowerRightHex
                            in
                            (universalHexLabelMaybe model.sectors first
                                |> Maybe.withDefault "???"
                            )
                                ++ " – "
                                ++ (universalHexLabelMaybe model.sectors last
                                        |> Maybe.withDefault "???"
                                   )
                    , -- player location display
                      row
                        [ uiDeepnightColorFontColour
                        , Font.size 14
                        , Element.spacing 5
                        , Element.pointer
                        , Events.onClick JumpToShip
                        , Element.alignBottom
                        , Element.mouseOver
                            [ Font.color <| convertColor (Color.Manipulate.lighten 0.25 deepnightColor)
                            ]
                        ]
                        [ text "Revelation"
                        , renderFAIcon "fa-regular fa-crosshairs-simple" 14
                        , text <|
                            (universalHexLabelMaybe model.sectors model.currentAddress
                                |> Maybe.withDefault "???"
                            )
                        ]
                    ]

                FullJourney ->
                    [ -- zoom out button
                      el
                        [ uiDeepnightColorFontColour
                        , Font.size 14
                        , Element.spacing 5
                        , Element.pointer
                        , Element.transparent <| model.journeyModel.zoomScale <= 1.0
                        , userSelectNone
                        , conditionalAttribute (model.journeyModel.zoomScale > 1.0) <|
                            Events.onClick (JourneyMsg <| Zoom ZoomOut)
                        , Element.alignBottom
                        , Element.mouseOver
                            [ Font.color <| convertColor (Color.Manipulate.lighten 0.25 deepnightColor)
                            ]
                        ]
                      <|
                        renderFAIcon "fa-regular fa-magnifying-glass-minus" 14
                    , -- zoom indicator
                      el
                        [ uiDeepnightColorFontColour
                        , Font.size 14
                        , Font.family [ Font.monospace ]
                        , userSelectNone
                        ]
                      <|
                        let
                            roundedZoom =
                                ceiling <| model.journeyModel.zoomScale

                            zoomAscii =
                                [ 1, 2, 3, 4, 6, 8 ]
                                    |> List.indexedMap
                                        (\i r ->
                                            if r == roundedZoom then
                                                text "|"

                                            else
                                                el
                                                    [ Element.pointer
                                                    , Events.onClick <| JourneyMsg <| Zoom <| ZoomSet (1.5 ^ toFloat i)
                                                    , Element.mouseOver
                                                        [ Font.color <| convertColor (Color.Manipulate.lighten 0.25 deepnightColor)
                                                        ]
                                                    ]
                                                <|
                                                    text "-"
                                        )
                        in
                        row [] zoomAscii
                    , -- zoom in button
                      el
                        [ uiDeepnightColorFontColour
                        , Font.size 14
                        , Element.spacing 5
                        , Element.pointer
                        , conditionalAttribute (model.journeyModel.zoomScale < 7.0) <|
                            Events.onClick (JourneyMsg <| Zoom ZoomIn)
                        , Element.transparent <| model.journeyModel.zoomScale >= 7.0
                        , userSelectNone
                        , Element.alignBottom
                        , Element.mouseOver
                            [ Font.color <| convertColor (Color.Manipulate.lighten 0.25 deepnightColor)
                            ]
                        ]
                      <|
                        renderFAIcon "fa-regular fa-magnifying-glass-plus" 14
                    ]
    in
    Element.wrappedRow [ Element.spacing 8, Element.width Element.fill, Element.paddingEach { zeroEach | bottom = 8 } ] <|
        (el [ Font.size 20, uiDeepnightColorFontColour ] <|
            text <|
                "Deepnight Navigation Console"
        )
            :: extras


view : Model -> Element.Element Msg
view model =
    let
        sidebarColumn =
            column [ Element.spacing 10, Element.centerX, Element.height Element.fill ]
                [ column [ Element.width Element.fill ]
                    [ let
                        clickableIcon size =
                            let
                                selectorColor =
                                    if model.hexScale == size / 2 && model.viewMode == HexMap then
                                        deepnightColor

                                    else
                                        textColor

                                hexStyle =
                                    if model.hexScale == size / 2 && model.viewMode == HexMap then
                                        "fa-regular"

                                    else
                                        "fa-thin"
                            in
                            renderFAIcon (hexStyle ++ " fa-hexagon") (floor size)
                                |> Element.el
                                    [ Element.pointer
                                    , Element.mouseOver [ Font.color <| convertColor (Color.Manipulate.lighten 0.15 selectorColor) ]
                                    , Events.onClick <| SetHexSize <| size / 2
                                    , Font.color <| convertColor selectorColor
                                    ]
                      in
                      row [ Element.spacing 6, Element.centerX ]
                        [ clickableIcon 80
                        , clickableIcon 60
                        , clickableIcon 50
                        , clickableIcon 30
                        , let
                            selectorColor =
                                if model.viewMode == FullJourney then
                                    deepnightColor

                                else
                                    textColor

                            hexStyle =
                                if model.viewMode == FullJourney then
                                    "fa-regular"

                                else
                                    "fa-thin"
                          in
                          el
                            [ Element.width <| Element.px 50
                            , Element.height <| Element.px 50
                            , Element.pointer
                            , Events.onClick ToggleHexmap
                            , Element.mouseOver [ Font.color <| convertColor (Color.Manipulate.lighten 0.15 selectorColor) ]
                            , Font.color <| convertColor selectorColor
                            ]
                          <|
                            renderFAIcon (hexStyle ++ " " ++ "fa-map") 50
                        ]
                    , case model.selectedHex of
                        Just viewingAddress ->
                            column [ centerY, Element.paddingXY 0 10, width fill ]
                                [ case model.solarSystems |> Dict.get (HexAddress.toKey viewingAddress) of
                                    Just (LoadedSolarSystem _) ->
                                        Element.none

                                    Just LoadingSolarSystem ->
                                        text "loading..."

                                    Just LoadedEmptyHex ->
                                        Element.none

                                    Just (FailedSolarSystem _) ->
                                        text "failed."

                                    Just (FailedStarsSolarSystem _) ->
                                        text "decoding a star failed"

                                    Nothing ->
                                        text "No solar system data found for system."
                                , row [ Element.spacing 5, width fill ]
                                    [ text <| universalHexLabel model.sectors viewingAddress
                                    , model.regions
                                        |> Dict.values
                                        |> -- abusing lists here since we only expect one label,
                                           -- but this iterates over all the regions
                                           -- and renders the name if it exists
                                           List.filterMap
                                            (\region ->
                                                if List.member viewingAddress region.hexes then
                                                    text region.name
                                                        |> el [ Font.size 12, Font.color <| convertColor region.colour ]
                                                        |> Just

                                                else
                                                    Nothing
                                            )
                                        |> column [ Element.alignRight ]
                                    ]
                                ]

                        Nothing ->
                            column [ centerX, centerY, Font.size 10 ]
                                [ text "Select hex in console to view parsec details."
                                ]
                    , case model.selectedSystem of
                        Just solarSystem ->
                            viewSystemDetailsSidebar
                                solarSystem
                                model.selectedStellarObject
                                model.hoveredStellarObject
                                model.referee

                        Nothing ->
                            column [ centerX, centerY, Font.size 10, Element.moveDown 20 ]
                                [ text "Click a hex to view system details."
                                ]
                    ]
                , -- footer
                  Element.el
                    [ Element.padding 10
                    , Element.alignBottom
                    , centerX
                    , Font.size 10
                    , uiDeepnightColorFontColour
                    ]
                  <|
                    case model.selectedHex of
                        Just viewingAddress ->
                            text <| Debug.toString viewingAddress

                        Nothing ->
                            text "Deepnight Corporation LLC"
                ]

        contentColumn =
            case model.viewMode of
                HexMap ->
                    let
                        defaultViewport =
                            { screenVp = model.viewport, hexmapVp = Nothing }

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
                        ( model.hexRect, model.rawHexaPoints )
                        viewPortConfig
                        { solarSystemDict = model.solarSystems, hexColours = model.hexColours, regionLabels = model.regionLabels }
                        ( model.route, model.currentAddress )
                        model.hexScale
                        model.selectedHex
                        model.referee
                        |> Element.html

                FullJourney ->
                    viewFullJourney model.journeyModel model.viewport
    in
    row
        [ width fill
        , height fill
        , Font.size 20
        , Font.color <| fontTextColor
        , Element.paddingXY 15 0
        ]
        [ el [ Element.height fill, Element.width <| Element.px sidebarWidth, Element.alignTop, Element.alignLeft ] <|
            sidebarColumn
        , column []
            [ viewStatusRow model
            , el [ Element.alignTop ] <| contentColumn
            ]
        , Element.html <| errorDialog model.newSolarSystemErrors
        ]


sendSolarSystemRequest : RequestEntry -> HostConfig -> HexRect -> Cmd Msg
sendSolarSystemRequest requestEntry hostConfig { upperLeftHex, lowerRightHex } =
    let
        solarSystemsDecoder : JsDecode.Decoder (List FallibleStarSystem)
        solarSystemsDecoder =
            JsDecode.list fallibleStarSystemDecoder

        ( urlHostRoot, urlHostPath ) =
            hostConfig

        url =
            Url.Builder.crossOrigin
                urlHostRoot
                (urlHostPath ++ [ "stars" ])
                [ Url.Builder.int "ulx" upperLeftHex.x
                , Url.Builder.int "uly" upperLeftHex.y
                , Url.Builder.int "lrx" lowerRightHex.x
                , Url.Builder.int "lry" lowerRightHex.y
                ]

        requestCmd =
            -- using Http.request instead of Http.get, to allow setting a timeout
            Http.request
                { method = "GET"
                , headers = []
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectJson (DownloadedSolarSystems ( requestEntry, url )) solarSystemsDecoder
                , timeout = Just 15000
                , tracker = Nothing
                }
    in
    requestCmd


sendRouteRequest : RequestEntry -> HostConfig -> Cmd Msg
sendRouteRequest requestEntry hostConfig =
    let
        routeDecoder : JsDecode.Decoder (List Route)
        routeDecoder =
            Codec.list Route.codec
                |> Codec.decoder

        ( urlHostRoot, urlHostPath ) =
            hostConfig

        url =
            Url.Builder.crossOrigin
                urlHostRoot
                (urlHostPath ++ [ "route" ])
                []

        requestCmd =
            Http.request
                { method = "GET"
                , headers = []
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectJson (DownloadedRoute ( requestEntry, url )) routeDecoder
                , timeout = Just 15000
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
                , Url.Builder.int "hx" <| hex.x + 1
                , Url.Builder.int "hy" <| hex.y + 1
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


saveMapCoords : HexAddress -> Cmd Msg
saveMapCoords upperLeft =
    storeInLocalStorage ( upperLeft.x, upperLeft.y )


port storeInLocalStorage : ( Int, Int ) -> Cmd msg


saveHexSize : Float -> Cmd Msg
saveHexSize size =
    storeHexSize size


port storeHexSize : Float -> Cmd msg


updateJourney : JourneyMsg -> Model -> ( Model, Cmd Msg )
updateJourney journeyMsg ({ journeyModel } as model) =
    let
        setJourneyModel newJourneyModel =
            { model | journeyModel = newJourneyModel }
    in
    case journeyMsg of
        Zoom zoomType ->
            let
                newZoomScale =
                    case zoomType of
                        ZoomIn ->
                            journeyModel.zoomScale * 1.5

                        ZoomOut ->
                            journeyModel.zoomScale / 1.5

                        ZoomSet newZoom ->
                            newZoom

                newJourneyModel =
                    { journeyModel | zoomScale = Debug.log "new zoom" newZoomScale }
            in
            ( setJourneyModel newJourneyModel, Cmd.none )

        MouseDown originalPos ->
            ( setJourneyModel { journeyModel | dragMode = IsDragging { start = originalPos, last = originalPos } }, Cmd.none )

        MouseLeave ->
            ( setJourneyModel { journeyModel | dragMode = NoDragging, hoverPoint = Nothing }, Cmd.none )

        MouseMove ( newX, newY ) ->
            case journeyModel.dragMode of
                IsDragging { start, last } ->
                    let
                        ( originalX, originalY ) =
                            last

                        ( xDelta, yDelta ) =
                            ( newX - originalX
                            , newY - originalY
                            )

                        ( maxWidth, maxHeight ) =
                            ( model.viewport.viewport.width - sidebarWidth - 50
                            , model.viewport.viewport.height
                            )

                        ( oldX, oldY ) =
                            journeyModel.zoomOffset

                        ( curImgWidth, curImgHeight ) =
                            ( maxWidth * journeyModel.zoomScale
                            , maxHeight * journeyModel.zoomScale
                            )

                        newModel =
                            { journeyModel
                                | dragMode = IsDragging { start = start, last = ( newX, newY ) }
                                , zoomOffset =
                                    ( clamp (maxWidth - curImgWidth) 0 (oldX + xDelta)
                                    , clamp (maxHeight - curImgHeight) 0 (oldY + yDelta)
                                    )
                                , hoverPoint = Nothing
                            }
                    in
                    if xDelta /= 0 || yDelta /= 0 then
                        ( setJourneyModel newModel, Cmd.none )

                    else
                        ( setJourneyModel { journeyModel | hoverPoint = Nothing }, Cmd.none )

                NoDragging ->
                    ( setJourneyModel { journeyModel | hoverPoint = Just ( newX, newY ) }, Cmd.none )

        MouseUp coordinates ->
            case journeyModel.dragMode of
                IsDragging { start } ->
                    let
                        ( ox, oy ) =
                            start

                        ( cx, cy ) =
                            coordinates

                        dist =
                            sqrt ((cx - ox) ^ 2 + (cy - oy) ^ 2)

                        _ =
                            Debug.log "mouse up" 123

                        hexViewToSector arg1 =
                            let
                                ( maxWidth, maxHeight ) =
                                    let
                                        scaledWidth =
                                            model.viewport.viewport.width - sidebarWidth - 50
                                    in
                                    ( scaledWidth
                                    , scaledWidth * (fullJourneyImageHeight / fullJourneyImageWidth)
                                    )

                                imageSize =
                                    { width = maxWidth * journeyModel.zoomScale, height = maxHeight * journeyModel.zoomScale }

                                sector =
                                    mouseCoordsToSector (toXY coordinates) (toXY journeyModel.zoomOffset) imageSize

                                hexAddress =
                                    shiftAddressBy { deltaX = -2, deltaY = -2 } <| createFromStarSystem { x = 1, y = 1, sectorX = sector.x, sectorY = sector.y }

                                hh =
                                    horizontalHexes model.hexmapViewport model.hexScale + 2

                                vh =
                                    verticalHexes model.hexmapViewport model.hexScale + 3

                                newHexRect =
                                    { upperLeftHex = hexAddress
                                    , lowerRightHex = shiftAddressBy { deltaX = hh, deltaY = vh } hexAddress
                                    }

                                newJourneyModel =
                                    { journeyModel | dragMode = NoDragging }

                                newModel =
                                    { model
                                        | hexRect = newHexRect
                                        , dragMode = NoDragging
                                        , viewMode = HexMap
                                        , journeyModel = newJourneyModel
                                    }
                            in
                            update DownloadSolarSystems newModel
                    in
                    if dist > 2 then
                        ( setJourneyModel { journeyModel | dragMode = NoDragging }, Cmd.none )

                    else
                        hexViewToSector ()

                NoDragging ->
                    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOpMsg ->
            ( model, Cmd.none )

        SetHexSize newSize ->
            let
                extraPaddingHexes =
                    2

                hh =
                    horizontalHexes model.hexmapViewport newSize + extraPaddingHexes

                vh =
                    verticalHexes model.hexmapViewport newSize + extraPaddingHexes

                lr =
                    HexAddress.shiftAddressBy
                        { deltaX = hh, deltaY = vh }
                        model.hexRect.upperLeftHex

                newHexRect =
                    { upperLeftHex = model.hexRect.upperLeftHex
                    , lowerRightHex = lr
                    }

                ( newModel, newCmds ) =
                    let
                        ( newModel_, newCmds_ ) =
                            ( { model
                                | hexScale = newSize
                                , rawHexaPoints = rawHexagonPoints newSize
                                , hexRect = newHexRect
                                , viewMode = HexMap
                              }
                            , saveHexSize newSize
                            )
                    in
                    update DownloadSolarSystems newModel_
                        |> Tuple.mapSecond (\newestCmds -> Cmd.batch [ newCmds_, newestCmds ])
            in
            ( newModel, newCmds )

        DownloadSolarSystems ->
            let
                ( nextRequestEntry, ( newSolarSystemDict, newRequestHistory ) ) =
                    prepNextRequest ( model.solarSystems, model.requestHistory ) model.hexRect
            in
            ( { model
                | requestHistory = newRequestHistory
                , solarSystems = newSolarSystemDict
              }
            , sendSolarSystemRequest nextRequestEntry model.hostConfig model.hexRect
            )

        DownloadedSolarSystems ( requestEntry, url_ ) (Ok fallibleSolarSystems) ->
            let
                rangeAsPairs =
                    HexAddress.between requestEntry.upperLeftHex requestEntry.lowerRightHex
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

                newErrors =
                    potentiallyNewErrors
                        |> List.filter
                            (\( newErr, errUrl ) ->
                                List.member newErr (model.oldSolarSystemErrors |> List.map Tuple.first) |> not
                            )

                ( sortedSolarSystems, potentiallyNewErrors ) =
                    fallibleSolarSystems
                        |> List.foldl
                            (\fallibleSystem ( systems, errs ) ->
                                let
                                    -- WARN: don't skip this check before filtering out errors, or we'll miss errors (see below)
                                    hasFailed =
                                        List.any Result.isErr fallibleSystem.stars
                                in
                                if not hasFailed then
                                    let
                                        si =
                                            if model.referee then
                                                refereeSI

                                            else
                                                fallibleSystem.surveyIndex

                                        starSystem : StarSystem
                                        starSystem =
                                            { address = fallibleSystem.address
                                            , sectorName = fallibleSystem.sectorName
                                            , name = fallibleSystem.name
                                            , scanPoints = fallibleSystem.scanPoints
                                            , surveyIndex = si
                                            , gasGiantCount = fallibleSystem.gasGiantCount
                                            , terrestrialPlanetCount = fallibleSystem.terrestrialPlanetCount
                                            , planetoidBeltCount = fallibleSystem.planetoidBeltCount
                                            , allegiance = fallibleSystem.allegiance
                                            , nativeSophont = fallibleSystem.nativeSophont
                                            , extinctSophont = fallibleSystem.extinctSophont
                                            , techLevel = fallibleSystem.techLevel
                                            , stars =
                                                -- WARN: relies on hasFailed to be false. if we don't do that check, we'll miss errors
                                                List.map Result.toMaybe fallibleSystem.stars |> List.filterMap identity
                                            }
                                    in
                                    ( ( HexAddress.toKey fallibleSystem.address
                                      , LoadedSolarSystem starSystem
                                      )
                                        :: systems
                                    , errs
                                    )

                                else
                                    ( ( HexAddress.toKey fallibleSystem.address
                                      , FailedStarsSolarSystem <| Debug.log "failed" fallibleSystem
                                      )
                                        :: systems
                                    , (fallibleSystem.stars
                                        |> List.filterMap
                                            (\res ->
                                                case res of
                                                    Ok _ ->
                                                        Nothing

                                                    Err er ->
                                                        Just ("Specific Star failed to decode:\n" ++ JsDecode.errorToString er)
                                            )
                                        |> List.map
                                            (\er ->
                                                ( Http.BadBody er, url_ )
                                            )
                                      )
                                        ++ errs
                                    )
                            )
                            ( [], [] )
                        |> Tuple.mapFirst Dict.fromList

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
                , newSolarSystemErrors = newErrors ++ model.newSolarSystemErrors
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

        DownloadedSectors ( requestEntry, url ) (Err err) ->
            let
                _ =
                    Debug.log "Sectors did not work" err
            in
            ( { model | newSolarSystemErrors = ( err, url ) :: model.newSolarSystemErrors }, Cmd.none )

        DownloadedRegions requestEntry (Ok regions) ->
            let
                parsecList : Region -> List ( String, Color )
                parsecList region =
                    List.map (\p -> ( HexAddress.toKey p, region.colour ))
                        region.hexes

                regionDict =
                    List.foldl
                        (\region acc ->
                            Dict.insert region.id region acc
                        )
                        Dict.empty
                        regions

                hexColourDict : Dict.Dict String Color
                hexColourDict =
                    List.map
                        (\region ->
                            parsecList region
                        )
                        regions
                        |> List.concat
                        |> Dict.fromList

                regionLabelDict : Dict.Dict String String
                regionLabelDict =
                    List.map
                        (\region ->
                            ( HexAddress.toKey region.labelPosition, region.name )
                        )
                        regions
                        |> Dict.fromList
            in
            ( { model
                | regions = regionDict
                , hexColours = hexColourDict
                , regionLabels = regionLabelDict
              }
            , Cmd.none
            )

        DownloadedRegions ( requestEntry, url ) (Err err) ->
            let
                parsecList : Region -> List ( String, Color )
                parsecList region =
                    List.map (\p -> ( HexAddress.toKey p, region.colour ))
                        region.hexes

                stub : Region
                stub =
                    { id = 1
                    , colour = Color.blue
                    , name = "Stub Hennlix Nebula"
                    , labelPosition = { x = -308, y = -104 }
                    , hexes =
                        [ { x = -308, y = -104 }
                        , { x = -308, y = -105 }
                        , { x = -307, y = -104 }
                        , { x = -309, y = -104 }
                        ]
                    }

                hexColourDict : Dict.Dict String Color
                hexColourDict =
                    List.map
                        (\region ->
                            parsecList region
                        )
                        [ stub ]
                        |> List.concat
                        |> Dict.fromList

                regionLabelDict : Dict.Dict String String
                regionLabelDict =
                    List.map
                        (\region ->
                            ( HexAddress.toKey region.labelPosition, region.name )
                        )
                        [ stub ]
                        |> Dict.fromList
            in
            ( { model
                | regions = Dict.fromList [ ( 1, stub ) ]
                , hexColours = hexColourDict
                , regionLabels = regionLabelDict
              }
            , Cmd.none
            )

        --let
        --    _ =
        --        Debug.log "Regions did not work" err
        --in
        --( { model | newSolarSystemErrors = ( err, url ) :: model.newSolarSystemErrors }, Cmd.none )
        DownloadedRoute ( requestEntry, url ) (Ok route) ->
            let
                firstEntry =
                    List.reverse route |> List.head |> Maybe.map .address
            in
            case firstEntry of
                Just address ->
                    ( { model
                        | route = route
                        , currentAddress = address
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model
                        | route = route
                      }
                    , Cmd.none
                    )

        DownloadedRoute ( requestEntry, url ) (Err err) ->
            let
                _ =
                    Debug.log "Route did not work" err
            in
            ( { model | newSolarSystemErrors = ( err, url ) :: model.newSolarSystemErrors }, Cmd.none )

        FetchedSolarSystem (Ok solarSystem) ->
            let
                si =
                    if model.referee then
                        refereeSI

                    else
                        solarSystem.surveyIndex

                updatedSS =
                    { solarSystem
                        | surveyIndex = si
                    }
            in
            ( { model
                | selectedSystem = Just updatedSS
              }
            , Cmd.none
            )

        FetchedSolarSystem (Err _) ->
            ( model, Cmd.none )

        DownloadedSolarSystems ( requestEntry, url ) (Err err) ->
            let
                newRequestHistory =
                    markRequestComplete requestEntry (RemoteData.Failure err) model.requestHistory

                rangeAsPairs =
                    HexAddress.between requestEntry.upperLeftHex requestEntry.lowerRightHex
                        |> List.map
                            (\addr ->
                                let
                                    addrKey =
                                        HexAddress.toKey addr
                                in
                                ( addrKey
                                , Dict.get addrKey existingDict
                                    |> (\maybeSolarsystem ->
                                            case maybeSolarsystem of
                                                Just LoadingSolarSystem ->
                                                    FailedSolarSystem err

                                                Just LoadedEmptyHex ->
                                                    FailedSolarSystem err

                                                Just (FailedSolarSystem _) ->
                                                    FailedSolarSystem err

                                                Just (LoadedSolarSystem solarSystem) ->
                                                    LoadedSolarSystem solarSystem

                                                Just (FailedStarsSolarSystem _) ->
                                                    FailedSolarSystem err

                                                Nothing ->
                                                    FailedSolarSystem err
                                       )
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
                , newSolarSystemErrors = ( err, url ) :: model.newSolarSystemErrors
                , lastSolarSystemError = Just err
                , requestHistory = newRequestHistory
              }
            , Cmd.none
            )

        HoveringHex hoveringHex ->
            ( { model | hoveringHex = Just hoveringHex }, Cmd.none )

        GotViewport viewport ->
            ( { model | viewport = viewport }
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
            let
                focusedErrors : List ( Http.Error, String )
                focusedErrors =
                    Dict.get (HexAddress.toKey hexAddress) model.solarSystems
                        |> Maybe.map
                            (\system ->
                                case system of
                                    FailedStarsSolarSystem fallibleSystem ->
                                        fallibleSystem.stars
                                            |> List.filterMap
                                                (\res ->
                                                    case res of
                                                        Ok _ ->
                                                            Nothing

                                                        Err er ->
                                                            Just ("Specific Star failed to decode:\n" ++ JsDecode.errorToString er)
                                                )
                                            |> List.map
                                                (\er ->
                                                    ( Http.BadBody er, "TODO: tie RequestEntry to URL" )
                                                )

                                    _ ->
                                        []
                            )
                        |> Maybe.withDefault []
            in
            ( { model
                | selectedHex = Just hexAddress
                , selectedStellarObject = Nothing
                , selectedSystem = Nothing
                , newSolarSystemErrors = focusedErrors
              }
            , fetchSingleSolarSystemRequest model.hostConfig <| toSectorAddress hexAddress
            )

        FocusInSidebar stellarObject ->
            ( { model | selectedStellarObject = Just stellarObject }
            , Cmd.none
            )

        HoveredStellarObject maybeOrbitSequence ->
            ( { model | hoveredStellarObject = maybeOrbitSequence }
            , Cmd.none
            )

        MapMouseDown coordinates ->
            ( { model
                | dragMode = IsDragging { start = coordinates, last = coordinates }
              }
            , Cmd.none
            )

        MapMouseUp ->
            case model.dragMode of
                IsDragging { start, last } ->
                    if start /= last then
                        let
                            ( nextRequestEntry, ( newSolarSystemDict, newRequestHistory ) ) =
                                prepNextRequest ( model.solarSystems, model.requestHistory ) model.hexRect
                        in
                        ( { model
                            | dragMode = NoDragging
                            , requestHistory = newRequestHistory
                            , solarSystems = newSolarSystemDict
                          }
                        , sendSolarSystemRequest nextRequestEntry model.hostConfig model.hexRect
                        )

                    else
                        ( { model
                            | dragMode = NoDragging
                          }
                        , Cmd.none
                        )

                _ ->
                    ( { model
                        | dragMode = NoDragging
                      }
                    , Cmd.none
                    )

        MapMouseMove ( newX, newY ) ->
            case model.dragMode of
                IsDragging { start, last } ->
                    let
                        ( originX, originY ) =
                            last

                        xDelta =
                            truncate <| (originX - newX) / model.hexScale

                        yDelta =
                            truncate <| (originY - newY) / model.hexScale

                        shiftAddress hex =
                            HexAddress.shiftAddressBy
                                { deltaX = xDelta, deltaY = yDelta }
                                hex

                        newHexRect =
                            { lowerRightHex = shiftAddress model.hexRect.lowerRightHex
                            , upperLeftHex = shiftAddress model.hexRect.upperLeftHex
                            }

                        newModel =
                            { model
                                | dragMode = IsDragging { start = start, last = ( newX, newY ) }
                                , hexRect = newHexRect
                            }
                    in
                    if xDelta /= 0 || yDelta /= 0 then
                        ( newModel
                        , saveMapCoords newModel.hexRect.upperLeftHex
                        )

                    else
                        ( model, Cmd.none )

                NoDragging ->
                    ( model, Cmd.none )

        MapMouseLeave ->
            ( { model | hoveringHex = Nothing }, Cmd.none )

        TableColumnHovered columnDesc ->
            ( { model | sidebarHoverText = columnDesc }, Cmd.none )

        ClearAllErrors ->
            ( { model | newSolarSystemErrors = [], oldSolarSystemErrors = model.newSolarSystemErrors ++ model.oldSolarSystemErrors }, Cmd.none )

        JumpToShip ->
            update (ZoomToHex model.currentAddress True) model

        ZoomToHex hexAddress centre ->
            let
                extraPadding =
                    2

                hHexes =
                    horizontalHexes model.hexmapViewport model.hexScale + extraPadding

                vHexes =
                    verticalHexes model.hexmapViewport model.hexScale + extraPadding

                newUpperLeft =
                    if centre then
                        hexAddress
                            |> HexAddress.shiftAddressBy
                                { deltaX = -1 * hHexes // 2
                                , deltaY = -1 * vHexes // 2
                                }

                    else
                        hexAddress
                            |> HexAddress.shiftAddressBy
                                { deltaX = -2
                                , deltaY = -2
                                }

                newHexRect =
                    { upperLeftHex = newUpperLeft
                    , lowerRightHex =
                        newUpperLeft
                            |> HexAddress.shiftAddressBy
                                { deltaX = hHexes
                                , deltaY = vHexes
                                }
                    }

                ( nextRequestEntry, ( newSolarSystemDict, newRequestHistory ) ) =
                    prepNextRequest ( model.solarSystems, model.requestHistory ) newHexRect
            in
            ( { model
                | hexRect = newHexRect
                , requestHistory = newRequestHistory
                , solarSystems = newSolarSystemDict
              }
            , Cmd.batch
                [ saveMapCoords newHexRect.upperLeftHex
                , sendSolarSystemRequest nextRequestEntry model.hostConfig newHexRect
                ]
            )

        ToggleHexmap ->
            let
                newViewMode =
                    if model.viewMode == HexMap then
                        FullJourney

                    else
                        HexMap
            in
            ( { model | viewMode = newViewMode }, Cmd.none )

        JourneyMsg journeyMsg ->
            updateJourney journeyMsg model


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
            Codec.list codec
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
                , expect = Http.expectJson (DownloadedSectors ( requestEntry, url )) sectorDecoder
                , timeout = Just 15000
                , tracker = Nothing
                }
    in
    requestCmd


auToKMs : Float -> Float
auToKMs au =
    au * 149597871.0


sendRegionRequest : RequestEntry -> HostConfig -> Cmd Msg
sendRegionRequest requestEntry hostConfig =
    let
        regionsDecoder : JsDecode.Decoder (List Region)
        regionsDecoder =
            Codec.list Region.codec
                |> Codec.decoder

        ( urlHostRoot, urlHostPath ) =
            hostConfig

        url =
            Url.Builder.crossOrigin
                urlHostRoot
                (urlHostPath ++ [ "regions" ])
                []

        requestCmd =
            Http.request
                { method = "GET"
                , headers = []
                , url = url
                , body = Http.emptyBody
                , expect = Http.expectJson (DownloadedRegions ( requestEntry, url )) regionsDecoder
                , timeout = Just 15000
                , tracker = Nothing
                }
    in
    requestCmd
