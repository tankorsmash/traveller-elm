module SolarSystemPage exposing (Model, Msg(..), init, update, view)

import Angle
import Browser.Dom
import Codec
import Dict
import Element exposing (column, text)
import Element.Font as Font
import Http
import RemoteData exposing (RemoteData)
import Svg.Styled as Svg
import Svg.Styled.Attributes as SvgAttrs
import Task
import Traveller exposing (Msg(..))
import Traveller.HexId exposing (HexId, RawHexId)
import Traveller.Orbit exposing (StellarOrbit(..))
import Traveller.SectorData exposing (SectorData, codecSectorData)
import Traveller.SolarSystem exposing (SolarSystem)
import Traveller.Star exposing (Star)
import Traveller.StellarObject exposing (StellarObject)


type alias Model =
    { solarSystem : Maybe SolarSystem
    , sectorData : RemoteData Http.Error ( SectorData, Dict.Dict RawHexId SolarSystem )
    , hexId : HexId
    }


init : HexId -> ( Model, Cmd Msg )
init hexId =
    ( { solarSystem = Nothing
      , sectorData = RemoteData.NotAsked
      , hexId = hexId
      }
    , sendSectorRequest
    )


rotatePoint : ( Float, Float ) -> Angle.Angle -> Float -> ( Float, Float )
rotatePoint ( x, y ) angle distance =
    let
        rads =
            Angle.inRadians angle

        cosTheta =
            cos rads

        sinTheta =
            sin rads
    in
    ( x + (distance * cosTheta) - (0 * sinTheta)
    , y + (distance * sinTheta) + (0 * cosTheta)
    )


viewSystem : SolarSystem -> Svg.Svg msg
viewSystem system =
    let
        { stars } =
            system

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

        drawStar ( starX, starY ) radius =
            Svg.g []
                [ Svg.circle
                    [ SvgAttrs.cx <| String.fromFloat <| starX
                    , SvgAttrs.cy <| String.fromFloat <| starY
                    , SvgAttrs.r <| String.fromFloat <| radius
                    , SvgAttrs.fill <| "red"
                    , SvgAttrs.stroke <| "green"
                    ]
                    []
                , Svg.text_
                    [ SvgAttrs.x <| String.fromFloat <| starX
                    , SvgAttrs.y <| String.fromFloat <| starY
                    , SvgAttrs.fill <| "black"
                    , SvgAttrs.textAnchor <| "middle"
                    , SvgAttrs.dominantBaseline <| "middle"
                    ]
                    [ Svg.text <| "Star"
                    ]
                ]

        drawPlanet ( planetX, planetY ) radius =
            Svg.g []
                [ Svg.circle
                    [ SvgAttrs.cx <| String.fromFloat <| planetX
                    , SvgAttrs.cy <| String.fromFloat <| planetY
                    , SvgAttrs.r <| String.fromFloat <| radius
                    , SvgAttrs.fill <| "green"
                    ]
                    []
                , Svg.text_
                    [ SvgAttrs.x <| String.fromFloat <| planetX
                    , SvgAttrs.y <| String.fromFloat <| planetY
                    , SvgAttrs.fill <| "black"
                    , SvgAttrs.textAnchor <| "middle"
                    , SvgAttrs.dominantBaseline <| "middle"
                    ]
                    [ Svg.text <| "Planet"
                    ]
                ]

        drawOrbit ( originX, originY ) radius =
            Svg.circle
                [ SvgAttrs.cx <| String.fromFloat <| originX
                , SvgAttrs.cy <| String.fromFloat <| originY
                , SvgAttrs.r <| String.fromFloat <| radius
                , SvgAttrs.fill <| "none"
                , SvgAttrs.strokeDasharray <| "5"
                , SvgAttrs.stroke <| "black"
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
            (\i star ->
                let
                    star_ : Star
                    star_ =
                        star

                    _ =
                        Debug.log "star" <| star

                    drawnStar =
                        let
                            ( starX, starY ) =
                                rotatePoint
                                    ( 250, 250 )
                                    (Angle.degrees 10)
                                    (star.orbit * 20)
                        in
                        drawStar ( starX, starY ) (star.mass * 50)

                    orbitingBodies =
                        List.map
                            (\stellarBody ->
                                let
                                    stellarBody_ : StellarObject
                                    stellarBody_ =
                                        stellarBody
                                in
                                Svg.g [] []
                            )
                            star.stellarObjects
                in
                Svg.g [] (drawnStar :: orbitingBodies)
             -- case star.orbit of
             --     SimpleOrbit orbitVal ->
             --         drawOrbit ( 250, 250 ) orbitVal
             --     ComplexOrbit { zone, orbit } ->
             --         drawOrbit ( 250, 250 ) (orbit)
            )
            stars
        )


view : Model -> Element.Element msg
view model =
    case model.solarSystem of
        Just system ->
            column
                [ Font.size
                    24
                , Element.centerX
                , Font.color <| Element.rgb 0.5 0.75 0.0
                ]
                [ text "Solar System"
                , text <| "Coordinate/hexid: " ++ model.hexId.raw
                , viewSystem system
                    |> Svg.toUnstyled
                    |> Element.html
                ]

        Nothing ->
            column
                [ Font.size 24 ]
                [ text "No Solar System" ]


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
        NoOp ->
            ( model, Cmd.none )

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

                newSolarSystem =
                    Dict.get model.hexId.value solarSystemDict
            in
            ( { model
                | sectorData =
                    ( newSectorData, solarSystemDict )
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
                            Debug.log bodyErr ""
                    in
                    Debug.todo "branch 'DownloadedSectorJson BadBody' not implemented"


type Msg
    = NoOp
    | DownloadSectorJson
    | DownloadedSectorJson (Result Http.Error SectorData)
