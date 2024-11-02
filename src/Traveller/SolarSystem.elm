module Traveller.SolarSystem exposing (SolarSystem, codecSolarSystem)

import Codec
import Traveller.HexId exposing (HexId, codecHexId)
import Traveller.StellarObject exposing (StarData, codecStarData, StellarObject, codecStellarObject)


type alias SolarSystem =
    { stars : List StarData
    , coordinates : HexId
    , primaryStar : StarData
    , gasGiants : Int
    , planetoidBelts : Int
    , terrestrialPlanets : Int
    }


codecSolarSystem : Codec.Codec SolarSystem
codecSolarSystem =
    Codec.object SolarSystem
        |> Codec.field "stars" .stars (Codec.list codecStarData)
        |> Codec.field "coordinates" .coordinates codecHexId
        |> Codec.field "primaryStar" .primaryStar codecStarData
        |> Codec.field "gasGiants" .gasGiants Codec.int
        |> Codec.field "planetoidBelts" .planetoidBelts Codec.int
        |> Codec.field "terrestrialPlanets" .terrestrialPlanets Codec.int
        |> Codec.buildObject
