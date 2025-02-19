port module Traveller exposing (Model, Msg(..), init, numHexCols, numHexRows, subscriptions, update, view)

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
import Html.Attributes as UnstyledHtmlAttrs
import Html.Events.Extra.Mouse
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as HtmlStyledAttrs
import Http
import Json.Decode as JsDecode
import Parser
import RemoteData exposing (RemoteData(..))
import Result.Extra as Result
import Round
import Svg.Attributes exposing (transform)
import Svg.Styled as Svg exposing (Svg)
import Svg.Styled.Attributes as SvgAttrs exposing (points, viewBox)
import Svg.Styled.Events as SvgEvents
import Svg.Styled.Keyed
import Svg.Styled.Lazy
import Task
import Traveller.HexAddress as HexAddress exposing (HexAddress, SectorHexAddress, hexLabel, sectorColumns, sectorRows, toSectorAddress, toSectorKey, toUniversalAddress, universalHexX, universalHexY)
import Traveller.Parser as TravellerParser
import Traveller.Point exposing (StellarPoint)
import Traveller.Route as Route exposing (Route, RouteList)
import Traveller.Sector exposing (Sector, SectorDict, codec, sectorKey)
import Traveller.SolarSystem as SolarSystem exposing (SolarSystem)
import Traveller.SolarSystemStars exposing (FallibleStarSystem, StarSystem, StarType, StarTypeData, fallibleStarSystemDecoder, getStarTypeData, isBrownDwarfType, starSystemCodec)
import Traveller.StarColour exposing (starColourRGB)
import Traveller.StellarObject exposing (GasGiantData, InnerStarData, PlanetoidBeltData, PlanetoidData, StarData(..), StellarObject(..), TerrestrialData, getInnerStarData, getSafeJumpTime, getStellarOrbit, isBrownDwarf)
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


type alias Model =
    { key : Browser.Navigation.Key
    , hexScale : Float
    , solarSystems : SolarSystemDict
    , newSolarSystemErrors : List ( Http.Error, String )
    , oldSolarSystemErrors : List ( Http.Error, String )
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
    , currentAddress : Maybe HexAddress
    , hostConfig : HostConfig.HostConfig
    , route : RouteList
    }


type Msg
    = NoOpMsg
    | ZoomScaleChanged Float
    | DownloadSolarSystems
    | DownloadedSolarSystems ( RequestEntry, String ) (Result Http.Error (List FallibleStarSystem))
    | ClearAllErrors
    | FetchedSolarSystem (Result Http.Error SolarSystem)
    | DownloadedSectors ( RequestEntry, String ) (Result Http.Error (List Sector))
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
    | DownloadedRoute ( RequestEntry, String ) (Result Http.Error (List Route))


{-| Where the Hex is on the screen, in pixel coordinates
-}
type alias VisualHexOrigin =
    ( Int, Int )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize GotResize


init : Maybe ( Int, Int ) -> Browser.Navigation.Key -> HostConfig.HostConfig -> ( Model, Cmd Msg )
init maybeUpperLeft key hostConfig =
    let
        -- requestHistory : RequestHistory
        ( initSystemDict, initRequestHistory ) =
            ( Dict.empty, [] )

        ( ( ssReqEntry, secReqEntry, routeReqEntry ), ( solarSystemDict, requestHistory ) ) =
            prepNextRequest ( initSystemDict, initRequestHistory ) upperLeftHex lowerRightHex
                |> -- build a new request entry for sector request
                   (\( ssReqEntry_, oldSsDictAndReqHistory ) ->
                        let
                            ( newReqEntry, ssDictAndReqHistory ) =
                                prepNextRequest oldSsDictAndReqHistory upperLeftHex lowerRightHex
                        in
                        ( ( ssReqEntry_, newReqEntry ), ssDictAndReqHistory )
                   )
                |> -- take the old ones and build a new one for route request
                   (\( ( ssReqEntry_, secReqEntry_ ), oldSsDictAndReqHistory ) ->
                        let
                            ( routeReqEntry_, ssDictAndReqHistory ) =
                                prepNextRequest oldSsDictAndReqHistory upperLeftHex lowerRightHex
                        in
                        ( ( ssReqEntry_, secReqEntry_, routeReqEntry_ ), ssDictAndReqHistory )
                   )

        upperLeftHex =
            case maybeUpperLeft of
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
            let
                squareSize =
                    30
            in
            upperLeftHex
                |> HexAddress.shiftAddressBy
                    { deltaX = squareSize
                    , deltaY = squareSize
                    }

        model : Model
        model =
            { hexScale = defaultHexSize
            , solarSystems = solarSystemDict
            , newSolarSystemErrors = []
            , oldSolarSystemErrors = []
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
            , route = []
            , currentAddress = Nothing
            }
    in
    ( model
    , Cmd.batch
        [ sendSolarSystemRequest ssReqEntry model.hostConfig model.upperLeftHex model.lowerRightHex
        , sendSectorRequest secReqEntry model.hostConfig
        , sendRouteRequest routeReqEntry model.hostConfig
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


hexagonPointZero : Float -> Float -> ( Float, Float )
hexagonPointZero size n =
    let
        a =
            2 * pi / 6

        x =
            size * cos (a * n)

        y =
            size * sin (a * n)

        buildPoint =
            ( x, y )
    in
    buildPoint


hexagonPoint : ( Int, Int ) -> Float -> Float -> ( Float, Float )
hexagonPoint ( xOrigin, yOrigin ) size n =
    let
        a =
            2 * pi / 6

        -- angle deg =
        --     (deg + 90) * pi / 180
        x =
            toFloat xOrigin
                + (size * cos (a * n))

        y =
            toFloat yOrigin
                + (size * sin (a * n))

        buildPoint =
            ( x, y )
    in
    buildPoint


hexagonPoints : ( Int, Int ) -> Float -> String
hexagonPoints ( xOrigin, yOrigin ) size =
    List.range 0 5
        |> List.map
            (toFloat
                >> hexagonPoint ( xOrigin, yOrigin ) size
                >> (\( x, y ) -> String.fromFloat x ++ "," ++ String.fromFloat y)
            )
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


isCurrentLocation : Model -> HexAddress -> Bool
isCurrentLocation model address =
    case model.currentAddress of
        Just a ->
            a == address

        Nothing ->
            False


viewHexEmpty : Int -> Int -> Int -> Int -> Float -> String -> String -> Svg Msg
viewHexEmpty hx hy x y size childSvgTxt hexColour =
    let
        origin =
            ( x, y )

        si =
            0

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
          SvgEvents.on "mousedown" downDecoder
        , SvgEvents.on "mousemove" moveDecoder
        , SvgAttrs.style "cursor: pointer; user-select: none"
        , SvgAttrs.id <| HexAddress.toKey hexAddress
        ]
        [ -- background hex
          Svg.Styled.Lazy.lazy2 renderPolygon
            (hexagonPoints origin size)
            hexColour
        , Svg.text_
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
        , SvgAttrs.css [ hoverableStyle ]
        ]
        []



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


moveDecoder : JsDecode.Decoder Msg
moveDecoder =
    -- equivalent to the `downDecoder`, only it returns `MapMouseMove` instead
    Html.Events.Extra.Mouse.eventDecoder
        |> JsDecode.map (.offsetPos >> MapMouseMove)


drawStar : ( Float, Float ) -> Int -> Float -> String -> Svg Msg
drawStar ( starX, starY ) radius size starColor =
    Svg.circle
        [ SvgAttrs.cx <| String.fromFloat <| starX
        , SvgAttrs.cy <| String.fromFloat <| starY
        , SvgAttrs.r <| String.fromFloat <| scaleAttr size radius
        , SvgAttrs.fill starColor -- <| starColourRGB star.colour
        ]
        []


renderHexWithStar : StarSystem -> String -> HexAddress -> VisualHexOrigin -> Float -> Svg Msg
renderHexWithStar starSystem hexColour hexAddress (( vox, voy ) as visualOrigin) size =
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
          SvgEvents.on "mousedown" downDecoder
        , SvgEvents.on "mousemove" moveDecoder
        , SvgAttrs.style "cursor: pointer; user-select: none"
        ]
        [ -- background hex
          Svg.Styled.Lazy.lazy2 renderPolygon
            (hexagonPoints visualOrigin size)
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


routeHexBg =
    "#FCD299"


currentAddressHexBg =
    "#fe5a1d"


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
            -1 * (hexSize + toFloat row * 2 * hexSize * sin a + hexSize * hexColOffset col * sin a)
    in
    ( floor x, floor y )


viewHex :
    Browser.Dom.Viewport
    -> ( HexAddress, HexAddress )
    -> Float
    -> SolarSystemDict
    -> ( Float, Float )
    -> HexAddress
    -> VisualHexOrigin
    -> String
    -> ( Svg Msg, Int )
viewHex widestViewport ( upperLeftHex, lowerRightHex ) hexSize solarSystemDict ( viewportWidth, viewportHeight ) hexAddress visualHexOrigin hexColour =
    let
        solarSystem =
            Dict.get (HexAddress.toKey hexAddress) solarSystemDict

        ( vox, voy ) =
            visualHexOrigin

        viewEmptyHelper txt =
            Svg.Styled.Lazy.lazy7 viewHexEmpty hexAddress.x hexAddress.y vox voy hexSize txt hexColour

        hexSVG =
            case solarSystem of
                Just (LoadedSolarSystem ss) ->
                    Svg.Styled.Lazy.lazy5 renderHexWithStar
                        ss
                        hexColour
                        hexAddress
                        visualHexOrigin
                        hexSize

                Just LoadingSolarSystem ->
                    viewEmptyHelper "Loading..."

                Just (FailedSolarSystem httpError) ->
                    viewEmptyHelper "Failed."

                Just LoadedEmptyHex ->
                    viewEmptyHelper ""

                Just (FailedStarsSolarSystem failedSolarSystem) ->
                    Svg.Styled.Lazy.lazy7 viewHexEmpty hexAddress.x hexAddress.y vox voy hexSize "Star Failed." "#aaaaaa"

                Nothing ->
                    viewEmptyHelper ""
    in
    ( hexSVG, isEmptyHex solarSystem )


type RemoteSolarSystem
    = LoadedSolarSystem StarSystem
    | LoadedEmptyHex
    | LoadingSolarSystem
    | FailedStarsSolarSystem FallibleStarSystem
    | FailedSolarSystem Http.Error


type alias SolarSystemDict =
    Dict.Dict String RemoteSolarSystem


type OffsetDir
    = Left
    | Right


renderSectorOutline : ( HexAddress, Int, Int ) -> Float -> SectorHexAddress -> Svg Msg
renderSectorOutline ( upperLeftHex, zero_x, zero_y ) hexSize hex =
    let
        -- calcVisualOrigin
        topLeft : HexAddress
        topLeft =
            { hex | x = 1, y = 1 } |> HexAddress.toUniversalAddress

        botRight =
            { hex | x = 32, y = 40 } |> HexAddress.toUniversalAddress

        topRight =
            { hex | x = 32, y = 1 } |> HexAddress.toUniversalAddress

        botLeft =
            { hex | x = 1, y = 40 } |> HexAddress.toUniversalAddress

        ( offsetNum1, _ ) =
            hexagonPointZero hexSize 2

        ( offsetNum2, _ ) =
            hexagonPointZero hexSize 3

        avgNum =
            (offsetNum1 + offsetNum2) / 2 |> floor

        adjustPos ( hexAddr, offsetDir ) =
            calcVisualOrigin hexSize
                { row = upperLeftHex.y - hexAddr.y, col = hexAddr.x - upperLeftHex.x }
                |> (\( x, y ) ->
                        case offsetDir of
                            Left ->
                                ( x - avgNum, y )

                            Right ->
                                ( x + avgNum, y )
                   )
                |> (\( x, y ) ->
                        (x - zero_x |> String.fromInt)
                            ++ ", "
                            ++ (y - zero_y |> String.fromInt)
                   )

        points_ =
            List.map adjustPos [ ( topLeft, Left ), ( topRight, Right ), ( botRight, Right ), ( botLeft, Left ), ( topLeft, Left ) ]
                |> String.join " "
    in
    Svg.polyline
        [ points points_
        , SvgAttrs.stroke "#0a0a0a"
        , SvgAttrs.fill "none"
        , SvgAttrs.strokeWidth "6"
        , SvgAttrs.pointerEvents "visiblePainted"
        ]
        []


viewHexes :
    HexAddress
    -> HexAddress
    -> { screenVp : Browser.Dom.Viewport, hexmapVp : Maybe Browser.Dom.Viewport }
    -> SolarSystemDict
    -> ( RouteList, Maybe HexAddress )
    -> Float
    -> Html Msg
viewHexes upperLeftHex lowerRightHex { screenVp, hexmapVp } solarSystemDict ( route, currentAddress ) hexSize =
    let
        viewportHeightIsh =
            screenVp.viewport.height * 0.9

        viewportWidthIsh =
            screenVp.viewport.width * 0.9

        renderCurrentAddressOutline : HexAddress -> Svg Msg
        renderCurrentAddressOutline ca =
            let
                locationOrigin =
                    calcVisualOrigin hexSize
                        { row = upperLeftHex.y - ca.y, col = ca.x - upperLeftHex.x }
                        |> (\( x, y ) ->
                                ( x - zero_x
                                , y - zero_y
                                )
                           )

                points =
                    case String.split " " <| hexagonPoints locationOrigin hexSize of
                        first :: points_ ->
                            (first :: points_) ++ [ first ]

                        other ->
                            other
            in
            Svg.Styled.Lazy.lazy2 renderPolyline
                (points |> String.join " ")
                currentAddressHexBg

        widestViewport =
            case hexmapVp of
                Nothing ->
                    screenVp

                Just hexmapViewport ->
                    hexmapViewport

        sectorUpperLeft =
            upperLeftHex |> HexAddress.toSectorAddress

        ( zero_x, zero_y ) =
            ( 0, 0 )

        hexRange =
            HexAddress.betweenWithMax
                (HexAddress.shiftAddressBy { deltaX = -1, deltaY = -1 } upperLeftHex)
                lowerRightHex
                { maxAcross = maxAcross, maxTall = maxTall }

        ( visualHexWidth, visualHexHeight ) =
            let
                ( left_x, left_y ) =
                    calcVisualOrigin hexSize
                        { row = 1, col = 1 }

                ( right_x, _ ) =
                    calcVisualOrigin hexSize
                        { row = 1, col = 2 }

                ( _, down_y ) =
                    calcVisualOrigin hexSize
                        { row = 2, col = 1 }
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
                    hexSVGOrigin =
                        calcVisualOrigin hexSize
                            { row = hexAddr.y, col = hexAddr.x }
                            |> (\( x, y ) ->
                                    ( x - zero_x
                                    , y - zero_y
                                    )
                               )

                    hexColour =
                        if Just hexAddr == currentAddress then
                            currentAddressHexBg

                        else if isOnRoute route hexAddr then
                            routeHexBg

                        else
                            defaultHexBg

                    ( hexSVG, isEmpty ) =
                        viewHex
                            widestViewport
                            ( upperLeftHex, lowerRightHex )
                            hexSize
                            solarSystemDict
                            ( viewportWidthIsh, viewportHeightIsh )
                            hexAddr
                            hexSVGOrigin
                            hexColour
                in
                hexSVG
            )
        |> (\hexSvgsWithHexAddress ->
                let
                    singlePolyHex =
                        Maybe.map renderCurrentAddressOutline currentAddress
                            |> Maybe.withDefault (Svg.text "")
                in
                [ renderSectorOutline
                    ( upperLeftHex, zero_x, zero_y + (floor <| hexSize / 1.6) )
                    hexSize
                    (upperLeftHex |> HexAddress.toSectorAddress)
                , renderSectorOutline ( upperLeftHex, zero_x, zero_y ) hexSize (lowerRightHex |> HexAddress.toSectorAddress)
                , singlePolyHex
                ]
                    ++ hexSvgsWithHexAddress
           )
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
                    toViewBox hexSize upperLeftHex
                        ++ " "
                        ++ stringWidth
                        ++ " "
                        ++ stringHeight
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


renderJumpTime : String -> Element.Element Msg
renderJumpTime time =
    Element.row safeJumpStyle
        [ renderIcon Icon.arrowUpFromBracket
        , text <| time
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
        , renderImage gasGiantData.code Nothing
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
        , renderImage terrestrialData.uwp terrestrialData.meanTemperature
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
        , renderImage planetoidBeltData.uwp Nothing
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
        , renderImage planetoidData.uwp planetoidData.meanTemperature
        , renderJumpTime planetoidData.safeJumpTime
        , renderTravelTime planet selectedStellarObject
        ]


renderStellarObject : Int -> Int -> StellarObject -> Maybe StellarObject -> Element.Element Msg
renderStellarObject surveyIndex newNestingLevel stellarObject selectedStellarObject =
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
                el [ Element.width Element.fill, Element.paddingEach { top = 0, left = 0, right = 0, bottom = 5 } ] <| displayStarDetails surveyIndex starDataConfig newNestingLevel selectedStellarObject
        ]


displayStarDetails : Int -> StarData -> Int -> Maybe StellarObject -> Element.Element Msg
displayStarDetails surveyIndex (StarDataWrap starData) nestingLevel selectedStellarObject =
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

        -- , Element.paddingEach {top=10, left=5, right=5, bottom=10}
        , Element.padding 10
        , Border.rounded 10
        ]
        [ row [ Font.alignLeft, Element.alignLeft ]
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
                    displayStarDetails surveyIndex compStarData nextNestingLevel selectedStellarObject
                )
            |> Maybe.withDefault Element.none
        , column [ Element.width Element.fill ] <|
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
                |> List.map (\so -> renderStellarObject surveyIndex nextNestingLevel so selectedStellarObject)
                |> column []
            , -- jump shadow
              column [ Font.size 14, Font.shadow { blur = 1, color = jumpShadowTextColor, offset = ( 0.5, 0.5 ) }, Element.width Element.fill, Element.behindContent red ]
                [ case starData.jumpShadow of
                    Just jumpShadow ->
                        Element.el [ Element.centerX ] <| text <| Round.round 2 jumpShadow

                    Nothing ->
                        text ""
                ]
            , starData.stellarObjects
                |> List.filter (not << inJumpShadow)
                |> List.filter isKnown
                |> List.map (\so -> renderStellarObject surveyIndex nextNestingLevel so selectedStellarObject)
                |> column []
            ]
        ]


convertColor : Color.Color -> Element.Color
convertColor color =
    Element.fromRgb <| Color.toRgba <| color


viewSystemDetailsSidebar : SolarSystem -> Maybe StellarObject -> Element Msg
viewSystemDetailsSidebar solarSystem selectedStellarObject =
    column [ Element.spacing 10, Element.paddingXY 0 10 ] <|
        [ displayStarDetails solarSystem.surveyIndex solarSystem.primaryStar 0 selectedStellarObject
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


deepnightColor : Element.Color
deepnightColor =
    colorToElementColor <| Color.rgb 1.0 0.498 0.0


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


numHexCols : number
numHexCols =
    sectorColumns


numHexRows : number
numHexRows =
    sectorRows


universalHexLabel : SectorDict -> HexAddress -> String
universalHexLabel sectors hexAddress =
    case Dict.get (HexAddress.toSectorKey <| HexAddress.toSectorAddress hexAddress) sectors of
        Nothing ->
            " "

        Just sector ->
            sector.name ++ " " ++ HexAddress.hexLabel hexAddress


errorDialog : List ( Http.Error, String ) -> UnstyledHtml.Html Msg
errorDialog httpErrors =
    let
        openAttr =
            if (not << List.isEmpty) httpErrors then
                UnstyledHtmlAttrs.attribute "open" "open"

            else
                UnstyledHtmlAttrs.classList []

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
    UnstyledHtml.node "dialog"
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


view : Model -> Element.Element Msg
view model =
    let
        sidebarColumn =
            column []
                [ el [ Font.size 14, Font.color <| fontDarkTextColor ] <|
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
                        [ row [ Element.spacing 2 ]
                            [ text "Revelation location:"
                            , text <| universalHexLabel model.sectors model.playerHex
                            ]
                        ]
                    ]
                , case model.selectedHex of
                    Just viewingAddress ->
                        column [ centerY, Element.paddingXY 0 10 ]
                            [ case model.solarSystems |> Dict.get (HexAddress.toKey viewingAddress) of
                                Just (LoadedSolarSystem s) ->
                                    Element.none

                                Just LoadingSolarSystem ->
                                    text "loading..."

                                Just LoadedEmptyHex ->
                                    Element.none

                                Just (FailedSolarSystem httpError) ->
                                    text "failed."

                                Just (FailedStarsSolarSystem _) ->
                                    text "decoding a star failed"

                                Nothing ->
                                    text "No solar system data found for system."
                            , text <| Debug.toString viewingAddress
                            , text <| universalHexLabel model.sectors viewingAddress
                            ]

                    Nothing ->
                        column [ centerX, centerY, Font.size 10 ]
                            [ text "Click a hex to view system details."
                            ]
                , case model.selectedSystem of
                    Just solarSystem ->
                        viewSystemDetailsSidebar
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
                Html.div [ HtmlStyledAttrs.css [ Css.color (Css.hex "#ff0000") ] ]
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
                [ el [ Font.size 20, Font.color <| deepnightColor, Element.paddingEach { zeroEach | bottom = 4 } ] <|
                    text <|
                        "Deepnight Revelation Navigation Console"
                , Element.html <|
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
                                model.lowerRightHex
                                viewPortConfig
                                model.solarSystems
                                ( model.route, model.currentAddress )
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
            , Element.html <| errorDialog model.newSolarSystemErrors
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
        solarSystemsDecoder : JsDecode.Decoder (List FallibleStarSystem)
        solarSystemsDecoder =
            -- Codec.list starSystemCodec
            --     |> Codec.decoder
            JsDecode.list fallibleStarSystemDecoder

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
                , Url.Builder.int "hy" <| hex.y - 1
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
                                        starSystem : StarSystem
                                        starSystem =
                                            { address = fallibleSystem.address
                                            , sectorName = fallibleSystem.sectorName
                                            , name = fallibleSystem.name
                                            , scanPoints = fallibleSystem.scanPoints
                                            , surveyIndex = fallibleSystem.surveyIndex
                                            , gasGiantCount = fallibleSystem.gasGiantCount
                                            , terrestrialPlanetCount = fallibleSystem.terrestrialPlanetCount
                                            , planetoidBeltCount = fallibleSystem.planetoidBeltCount
                                            , allegiance = fallibleSystem.allegiance
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

        DownloadedRoute ( requestEntry, url ) (Ok route) ->
            ( { model
                | route = route
                , currentAddress = List.reverse route |> List.head |> Maybe.map .address
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
            ( { model
                | selectedSystem = Just solarSystem
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
                        , saveMapCoords newModel.upperLeftHex
                        )

                    else
                        ( model, Cmd.none )

                NoDragging ->
                    ( model, Cmd.none )

        TableColumnHovered columnDesc ->
            ( { model | sidebarHoverText = columnDesc }, Cmd.none )

        ClearAllErrors ->
            ( { model | newSolarSystemErrors = [], oldSolarSystemErrors = model.newSolarSystemErrors ++ model.oldSolarSystemErrors }, Cmd.none )


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



--sendRouteRequest : RequestEntry -> HostConfig -> Cmd Msg
--sendRouteRequest requestEntry hostConfig =
--    let
--        routeDecoder : JsDecode.Decoder (List Route)
--        routeDecoder =
--            Codec.list codecRoute
--                |> Codec.decoder
--
--        ( urlHostRoot, urlHostPath ) =
--            hostConfig
--
--        url =
--            Url.Builder.crossOrigin
--                urlHostRoot
--                (urlHostPath ++ [ "route" ])
--                []
--
--        requestCmd =
--            -- using Http.request instead of Http.get, to allow setting a timeout
--            Http.request
--                { method = "GET"
--                , headers = []
--                , url = url
--                , body = Http.emptyBody
--                , expect = Http.expectJson (DownloadedRoute requestEntry) routeDecoder
--                , timeout = Just 5000
--                , tracker = Nothing
--                }
--    in
--    requestCmd
