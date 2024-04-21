module Traveller.SectorData exposing (SectorData, codecSectorData)

import Codec exposing (Codec)
import Json.Decode as JsDecode
import Json.Encode as JsEncode
import Parser exposing ((|.), (|=), Parser)
import Parser.Extras as Parser
import Traveller.HexId exposing (HexId, codecHexId)
import Traveller.SolarSystem exposing (SolarSystem, codecSolarSystem)
import Traveller.Star exposing (Star, codecStar)


type alias SectorData =
    { solarSystems : List SolarSystem
    , name : String
    , x : Int
    , y : Int
    }


codecSectorData : Codec.Codec SectorData
codecSectorData =
    Codec.object SectorData
        |> Codec.field "solarSystems" .solarSystems (Codec.list codecSolarSystem)
        |> Codec.field "name" .name Codec.string
        |> Codec.field "X" .x Codec.int
        |> Codec.field "Y" .y Codec.int
        |> Codec.buildObject
