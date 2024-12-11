module Traveller.SolarSystem exposing (SolarSystem, codec)

import Codec
import Traveller.HexId exposing (HexId, codecHexId, createFromInt, createFromXY, toRowCol, toXY)
import Traveller.StellarObject exposing (StarData, codecStarData, codecStellarObject)


type alias SolarSystem =
    { coordinates : HexId
    , primaryStar : StarData
    , gasGiants : Int
    , planetoidBelts : Int
    , terrestrialPlanets : Int
    , surveyIndex : Int
    , nativeSophont : Bool
    , extinctSophont : Bool
    , sectorX : Int
    , sectorY : Int
    , sectorName : String
    }


type alias RawSolarSystem =
    { x : Int
    , y : Int
    , primaryStar : StarData
    , gasGiants : Int
    , planetoidBelts : Int
    , terrestrialPlanets : Int
    , surveyIndex : Int
    , nativeSophont : Bool
    , extinctSophont : Bool
    , sectorX : Int
    , sectorY : Int
    , sectorName : String
    }


codec : Codec.Codec SolarSystem
codec =
    rawCodec
        |> Codec.andThen rawToFinal finalToRaw


rawToFinal : RawSolarSystem -> Codec.Codec SolarSystem
rawToFinal rawSolarSystem =
    let
        coordinates =
            Traveller.HexId.createFromXY rawSolarSystem
    in
    case coordinates of
        Just hexId ->
            Codec.succeed
                { coordinates = hexId
                , primaryStar = rawSolarSystem.primaryStar
                , gasGiants = rawSolarSystem.gasGiants
                , planetoidBelts = rawSolarSystem.planetoidBelts
                , terrestrialPlanets = rawSolarSystem.terrestrialPlanets
                , surveyIndex = rawSolarSystem.surveyIndex
                , nativeSophont = rawSolarSystem.nativeSophont
                , extinctSophont = rawSolarSystem.extinctSophont
                , sectorX = rawSolarSystem.sectorX
                , sectorY = rawSolarSystem.sectorY
                , sectorName = rawSolarSystem.sectorName
                }

        Nothing ->
            Codec.fail <|
                "Invalid coordinates: "
                    ++ String.fromInt rawSolarSystem.x
                    ++ ", "
                    ++ String.fromInt rawSolarSystem.y
                    ++ "(Not in range 0101 to 3240 maybe?)"


finalToRaw : SolarSystem -> RawSolarSystem
finalToRaw solarSystem =
    let
        { y, x } =
            toXY solarSystem.coordinates
    in
    { x = x
    , y = y
    , primaryStar = solarSystem.primaryStar
    , gasGiants = solarSystem.gasGiants
    , planetoidBelts = solarSystem.planetoidBelts
    , terrestrialPlanets = solarSystem.terrestrialPlanets
    , surveyIndex = solarSystem.surveyIndex
    , nativeSophont = solarSystem.nativeSophont
    , extinctSophont = solarSystem.extinctSophont
    , sectorX = solarSystem.sectorX
    , sectorY = solarSystem.sectorY
    , sectorName = solarSystem.sectorName
    }


rawCodec : Codec.Codec RawSolarSystem
rawCodec =
    Codec.object RawSolarSystem
        |> Codec.field "x" .x Codec.int
        |> Codec.field "y" .y Codec.int
        |> Codec.field "primary_star" .primaryStar codecStarData
        |> Codec.field "gas_giant_count" .gasGiants Codec.int
        |> Codec.field "planetoid_belt_count" .planetoidBelts Codec.int
        |> Codec.field "terrestrial_planet_count" .terrestrialPlanets Codec.int
        |> Codec.field "survey_index" .surveyIndex Codec.int
        |> Codec.field "native_sophont" .nativeSophont Codec.bool
        |> Codec.field "extinct_sophont" .extinctSophont Codec.bool
        |> Codec.field "sector_x" .sectorX Codec.int
        |> Codec.field "sector_y" .sectorY Codec.int
        |> Codec.field "sector_name" .sectorName Codec.string
        |> Codec.buildObject
