module Traveller.SectorData exposing (SectorData, SectorSolarSystem, codecSectorData)

import Codec exposing (Codec)
import Json.Decode as JsDecode
import Json.Encode as JsEncode
import Parser exposing ((|.), (|=), Parser)
import Parser.Extras as Parser
import Traveller.HexId exposing (HexId, codecHexId)
import Traveller.Star exposing (Star, codecStar)


type alias SectorSolarSystem =
    { stars : List Star, coordinates : HexId }


type alias SectorData =
    { solarSystems : List SectorSolarSystem }


codecSectorData : Codec.Codec SectorData
codecSectorData =
    Codec.object SectorData
        |> Codec.field "solarSystems" .solarSystems (Codec.list codecSectorSolarSystems)
        |> Codec.buildObject


codecSectorSolarSystems : Codec.Codec SectorSolarSystem
codecSectorSolarSystems =
    Codec.object SectorSolarSystem
        |> Codec.field "stars" .stars (Codec.list codecStar)
        |> Codec.field "coordinates" .coordinates codecHexId
        |> Codec.buildObject
