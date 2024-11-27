module SolarSystemPage exposing (Model, Msg(..), init, update, view)

import Angle
import Browser.Dom
import Codec
import Dict
import Element exposing (Element, column, row, text)
import Element.Font as Font
import Http
import Json.Decode as JsDecode
import Math.Vector2 as Vector2
import RemoteData exposing (RemoteData)
import Svg.Styled as Svg
import Svg.Styled.Attributes as SvgAttrs
import Svg.Styled.Events
import Task
import Traveller exposing (Msg(..))
import Traveller.HexId exposing (HexId, RawHexId)
import Traveller.Moon exposing (Moon)
import Traveller.Orbit exposing (StellarOrbit(..))
import Traveller.SectorData exposing (SectorData, codecSectorData)
import Traveller.SolarSystem as SolarSystem exposing (SolarSystem)
import Traveller.StellarObject exposing (StarData(..), StarDataConfig, StellarObject(..))


type alias Model =
    { solarSystem : Maybe SolarSystem
    , solarSystems : RemoteData Http.Error (Dict.Dict RawHexId SolarSystem)
    , hexId : HexId
    , hoveredBody : HoveredBody
    }


type HoveredBody
    = HoveredPlanet StellarObject
    | HoveredStar StarDataConfig
    | HoveredMoon Moon
    | NoHoveredBody


type Msg
    = NoOp
    | DownloadSectorJson
    | DownloadedSectorJson (Result Http.Error (List SolarSystem))
    | HoveredBody HoveredBody


init : HexId -> ( Model, Cmd Msg )
init hexId =
    ( { solarSystem = Nothing
      , solarSystems = RemoteData.NotAsked
      , hexId = hexId
      , hoveredBody = NoHoveredBody
      }
      --, sendSectorRequest
    , Cmd.none
    )


rotatePoint : Vector2.Vec2 -> Angle.Angle -> Float -> Vector2.Vec2
rotatePoint vec2 angle distance =
    let
        rads =
            Angle.inRadians angle

        cosTheta =
            cos rads

        sinTheta =
            sin rads

        { x, y } =
            Vector2.toRecord vec2
    in
    Vector2.vec2
        (x + (distance * cosTheta) - (0 * sinTheta))
        (y + (distance * sinTheta) + (0 * cosTheta))


viewSystem : SolarSystem -> Svg.Svg Msg
viewSystem system =
    let
        { stars } =
            Debug.todo "remove later system"

        ( width, stringWidth ) =
            500 |> (\x -> ( x, String.fromFloat x ))

        ( height, stringHeight ) =
            500 |> (\x -> ( x, String.fromFloat x ))

        ( horizOffset, vertOffset ) =
            ( 0.0, 0.0 )

        xOffset =
            -- view horizontal offset
            String.fromFloat (width * horizOffset)

        yOffset =
            -- view vertical offset
            String.fromFloat (height * vertOffset)

        drawStar : StarDataConfig -> ( Float, Float ) -> Float -> Svg.Svg Msg
        drawStar star ( starX, starY ) radius =
            Svg.g []
                [ Svg.circle
                    [ SvgAttrs.cx <| String.fromFloat <| starX
                    , SvgAttrs.cy <| String.fromFloat <| starY
                    , SvgAttrs.r <| String.fromFloat <| radius
                    , SvgAttrs.fill <| "red"
                    , SvgAttrs.stroke <| "green"
                    , Svg.Styled.Events.onMouseOver <| HoveredBody <| HoveredStar star
                    ]
                    []
                , Svg.text_
                    [ SvgAttrs.x <| String.fromFloat <| starX
                    , SvgAttrs.y <| String.fromFloat <| starY
                    , SvgAttrs.fill <| "black"
                    , SvgAttrs.textAnchor <| "middle"
                    , SvgAttrs.dominantBaseline <| "middle"
                    , SvgAttrs.pointerEvents "none"
                    ]
                    [ Svg.text <| "* " ++ star.orbitSequence
                    ]
                ]

        drawStellarObject stellarData ( planetX, planetY ) radius =
            Svg.g []
                [ Svg.circle
                    [ SvgAttrs.cx <| String.fromFloat <| planetX
                    , SvgAttrs.cy <| String.fromFloat <| planetY
                    , SvgAttrs.r <| String.fromFloat <| radius
                    , SvgAttrs.fill <| "green"
                    , Svg.Styled.Events.onMouseOver <| HoveredBody <| HoveredPlanet stellarData
                    ]
                    []
                , Svg.text_
                    [ SvgAttrs.x <| String.fromFloat <| planetX
                    , SvgAttrs.y <| String.fromFloat <| planetY
                    , SvgAttrs.fill <| "black"
                    , SvgAttrs.textAnchor <| "middle"
                    , SvgAttrs.dominantBaseline <| "middle"
                    , SvgAttrs.pointerEvents "none"
                    , SvgAttrs.fontSize "8"
                    ]
                    [ Svg.text <| "Planet: " ++ "WIP"

                    -- [ Svg.text <| "Planet: " ++ String.fromInt (List.length (Maybe.withDefault [] <| stellarData.moons)) ++ " m"
                    -- [ Svg.text <| "Planet, " ++ String.fromInt (List.length stellarData.moons) ++ " moons"
                    ]
                ]

        drawOrbit ( originX, originY ) radius strokeColor =
            Svg.circle
                [ SvgAttrs.cx <| String.fromFloat <| originX
                , SvgAttrs.cy <| String.fromFloat <| originY
                , SvgAttrs.r <| String.fromFloat <| radius
                , SvgAttrs.fill <| "none"
                , SvgAttrs.strokeDasharray <| "5"
                , SvgAttrs.stroke strokeColor
                ]
                []
    in
    Svg.svg
        [ SvgAttrs.width <| stringWidth
        , SvgAttrs.height <| stringHeight
        , SvgAttrs.id "start_svg"
        , SvgAttrs.style "border: 1px solid black; background-color: #f0f0f0;"
        , SvgAttrs.viewBox <|
            xOffset
                ++ " "
                ++ yOffset
                ++ " "
                ++ stringWidth
                ++ " "
                ++ stringHeight
        ]
        (List.indexedMap
            (\i (StarData star) ->
                let
                    _ =
                        Debug.log "star" <| star

                    ( parentX, parentY ) =
                        ( 250, 250 )

                    ( starX, starY ) =
                        rotatePoint
                            (Vector2.vec2 parentX parentY)
                            (Angle.degrees (10 * toFloat i))
                            (star.orbit * 20)
                            |> Vector2.toRecord
                            |> (\{ x, y } -> ( x, y ))

                    drawnStar =
                        Svg.g []
                            [ drawOrbit ( parentX, parentY ) (star.orbit * 20) "blue"
                            , drawStar star
                                ( starX, starY )
                                (case star.mass of
                                    Just mass ->
                                        mass

                                    Nothing ->
                                        999999
                                )
                            ]

                    -- orbitingBodies =
                    --     List.indexedMap
                    --         (\planetI stellarObjectX ->
                    --             let
                    --                 planetPos =
                    --                     rotatePoint
                    --                         (Vector2.vec2 0 0)
                    --                         (Angle.degrees (35 * toFloat planetI))
                    --                         (case stellarBody.orbit of
                    --                             SimpleOrbit orbitVal ->
                    --                                 55 + (toFloat planetI * 5)
                    --
                    --                             ComplexOrbit { zone, orbit } ->
                    --                                 55 + (toFloat planetI * 5)
                    --                         )
                    --                         |> Vector2.add (Vector2.vec2 starX starY)
                    --             in
                    --             Svg.g []
                    --                 [ drawStellarObject
                    --                     stellarBody
                    --                     (planetPos
                    --                         |> Vector2.toRecord
                    --                         |> (\{ x, y } -> ( x, y ))
                    --                     )
                    --                     5
                    --                 , drawOrbit
                    --                     ( starX, starY )
                    --                     (case stellarBody.orbit of
                    --                         SimpleOrbit orbitVal ->
                    --                             55 + (toFloat planetI * 5)
                    --
                    --                         ComplexOrbit { zone, orbit } ->
                    --                             55 + (toFloat planetI * 5)
                    --                     )
                    --                     "gray"
                    --                 ]
                    --         )
                    --         star.stellarObjects
                in
                -- Svg.g [] (drawnStar :: orbitingBodies)
                Svg.g [] [ drawnStar ]
            )
            stars
        )


view : Model -> Element.Element Msg
view model =
    case model.solarSystem of
        Just system ->
            column
                [ Font.size 24
                , Element.centerX
                , Font.color <| Element.rgb 0.5 0.75 0.0
                ]
                [ text "Solar System"
                , text <| "Coordinate/hexid: " ++ model.hexId.raw
                , row []
                    [ case model.hoveredBody of
                        NoHoveredBody ->
                            text "Hover body to see details"

                        HoveredPlanet hoveredPlanet ->
                            text "TODO: hovering planet"

                        HoveredStar hoveredStar ->
                            text "TODO: hovering star"

                        HoveredMoon hoveredMoon ->
                            text "TODO: hovering moon"
                    , viewSystem system
                        |> Svg.toUnstyled
                        |> Element.html
                    ]
                ]

        Nothing ->
            column
                [ Font.size 24 ]
                [ text "No Solar System" ]


sendSectorRequest : Cmd Msg
sendSectorRequest =
    let
        solarSystemListParser : JsDecode.Decoder (List SolarSystem)
        solarSystemListParser =
            Codec.list SolarSystem.codec
                |> Codec.decoder
    in
    Http.get
        -- { url = "/Few Stars.json"
        { url = "/public/Deepnight.json"
        , expect = Http.expectJson DownloadedSectorJson solarSystemListParser
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        DownloadSectorJson ->
            ( model
            , sendSectorRequest
            )

        DownloadedSectorJson (Ok solarSystems) ->
            let
                --sortedSolarSystems =
                --    sectorData.solarSystems |> List.sortBy (.coordinates >> .value)
                solarSystemDict =
                    solarSystems
                        |> List.map (\system -> ( system.coordinates.value, system ))
                        |> Dict.fromList

                --newSectorData =
                --    { sectorData | solarSystems = sortedSolarSystems }
                newSolarSystem =
                    Dict.get model.hexId.value solarSystemDict
            in
            ( { model
                | solarSystems =
                    solarSystemDict
                        |> RemoteData.Success
                , solarSystem = newSolarSystem
              }
            , Cmd.batch
                [--  Browser.Dom.getViewportOf "hexmap"
                 -- |> Task.attempt GotHexMapViewport
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
                            Debug.log bodyErr "__ END OF ERROR SolarSystemPage.elm __"
                    in
                    ( { model | solarSystems = RemoteData.Failure err }, Cmd.none )

        HoveredBody ((HoveredPlanet hoveredPlanet) as hoveredBody) ->
            ( { model | hoveredBody = hoveredBody }, Cmd.none )

        HoveredBody ((HoveredStar hoveredStar) as hoveredBody) ->
            ( { model | hoveredBody = hoveredBody }, Cmd.none )

        HoveredBody ((HoveredMoon hoveredMoon) as hoveredBody) ->
            ( { model | hoveredBody = hoveredBody }, Cmd.none )

        HoveredBody NoHoveredBody ->
            ( { model | hoveredBody = NoHoveredBody }, Cmd.none )
