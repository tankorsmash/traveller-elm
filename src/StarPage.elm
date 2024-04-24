module StarPage exposing (Model, Msg(..), init, update, view)

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
import Traveller.HexId exposing (RawHexId)
import Traveller.SectorData exposing (SectorData, codecSectorData)
import Traveller.SolarSystem exposing (SolarSystem)


type alias Model =
    { solarSystem : Maybe SolarSystem
    , sectorData : RemoteData Http.Error ( SectorData, Dict.Dict RawHexId SolarSystem )
    }


viewSystem : SolarSystem -> Svg.Svg msg
viewSystem system =
    let
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
            Svg.circle
                [ SvgAttrs.cx <| String.fromFloat <| starX
                , SvgAttrs.cy <| String.fromFloat <| starY
                , SvgAttrs.r <| String.fromFloat <| radius
                , SvgAttrs.fill <| "red"
                ]
                []

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
        [ drawStar ( 250, 250 ) 20
        , drawOrbit ( 250, 250 ) 60
        , drawOrbit ( 250, 250 ) 80
        , drawOrbit ( 250, 250 ) 120
        , drawOrbit ( 250, 250 ) 200
        ]


view : { a | solarSystem : Maybe SolarSystem } -> Element.Element msg
view model =
    case model.solarSystem of
        Just system ->
            column
                [ Font.size 24 ]
                [ text "Solar System"
                , viewSystem system
                    |> Svg.toUnstyled
                    |> Element.html
                ]

        Nothing ->
            column
                [ Font.size 24 ]
                [ text "No Solar System" ]


init : ( Model, Cmd Msg )
init =
    ( { solarSystem = Nothing, sectorData = RemoteData.NotAsked }, sendSectorRequest )


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


update : Msg -> Model -> (Model, Cmd Msg)
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
            in
            ( { model
                | sectorData =
                    ( newSectorData, solarSystemDict )
                        |> RemoteData.Success
              }
            , Cmd.batch
                [
                    --  Browser.Dom.getViewportOf "hexmap"
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
