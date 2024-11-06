module Traveller.SolarSystem exposing (SolarSystem, codec)

import Codec
import Traveller.HexId exposing (HexId, codecHexId)
import Traveller.StellarObject exposing (StarData, codecStarData, codecStellarObject)


type alias SolarSystem =
    { coordinates : HexId
    , primaryStar : StarData
    , gasGiants : Int
    , planetoidBelts : Int
    , terrestrialPlanets : Int
    }


codec : Codec.Codec SolarSystem
codec =
    Codec.object SolarSystem
        |> Codec.field "coordinates" .coordinates codecHexId
        |> Codec.field "primaryStar" .primaryStar codecStarData
        |> Codec.field "gasGiants" .gasGiants Codec.int
        |> Codec.field "planetoidBelts" .planetoidBelts Codec.int
        |> Codec.field "terrestrialPlanets" .terrestrialPlanets Codec.int
        |> Codec.buildObject
