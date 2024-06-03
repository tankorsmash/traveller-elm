module Traveller.SectorData exposing (SIHex, SISector, SectorData, SurveyIndexData, codecSIHex, codecSISector, codecSectorData, codecSurveyIndexData)

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


type alias SIHex =
    { coordinate : HexId
    , si : Int
    }


codecSIHex : Codec.Codec SIHex
codecSIHex =
    Codec.object SIHex
        |> Codec.field "coordinate" .coordinate codecHexId
        |> Codec.field "SI" .si Codec.int
        |> Codec.buildObject


type alias SISector =
    { x : Int
    , y : Int
    , hexes : List SIHex
    }


codecSISector : Codec.Codec SISector
codecSISector =
    Codec.object SISector
        |> Codec.field "x" .x Codec.int
        |> Codec.field "y" .y Codec.int
        |> Codec.field "hexes" .hexes (Codec.list codecSIHex)
        |> Codec.buildObject


type alias SurveyIndexData =
    { sectors : List SISector
    }


codecSurveyIndexData : Codec.Codec SurveyIndexData
codecSurveyIndexData =
    Codec.map SurveyIndexData .sectors (Codec.list codecSISector)
