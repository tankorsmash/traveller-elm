module Traveller.SolarSystem exposing (SolarSystem, codecSolarSystem)

import Codec
import Traveller.HexId exposing (HexId, codecHexId)
import Traveller.Star exposing (Star, codecStar)


type alias SolarSystem =
    { stars : List Star
    , coordinates : HexId
    , primaryStar : Star
    , gasGiants : Int
    , planetoidBelts : Int
    , terrestrialPlanets : Int
    }


codecSolarSystem : Codec.Codec SolarSystem
codecSolarSystem =
    Codec.object SolarSystem
        |> Codec.field "stars" .stars (Codec.list codecStar)
        |> Codec.field "coordinates" .coordinates codecHexId
        |> Codec.field "primaryStar" .primaryStar codecStar
        |> Codec.field "gasGiants" .gasGiants Codec.int
        |> Codec.field "planetoidBelts" .planetoidBelts Codec.int
        |> Codec.field "terrestrialPlanets" .terrestrialPlanets Codec.int
        |> Codec.buildObject
