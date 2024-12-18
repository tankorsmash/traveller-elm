module Traveller.SolarSystem exposing (SolarSystem, SolarSystemDict, codec)

import Codec
import Dict
import Traveller.HexAddress as HexAddress exposing (HexAddress)
import Traveller.StellarObject exposing (StarData, codecStarData, codecStellarObject)


type alias SolarSystem =
    { address : HexAddress
    , primaryStar : StarData
    , gasGiants : Int
    , planetoidBelts : Int
    , terrestrialPlanets : Int
    , surveyIndex : Int
    , nativeSophont : Bool
    , extinctSophont : Bool
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
        address =
            HexAddress.createFromSolarSystem rawSolarSystem
    in
    Codec.succeed
        { address = address
        , primaryStar = rawSolarSystem.primaryStar
        , gasGiants = rawSolarSystem.gasGiants
        , planetoidBelts = rawSolarSystem.planetoidBelts
        , terrestrialPlanets = rawSolarSystem.terrestrialPlanets
        , surveyIndex = rawSolarSystem.surveyIndex
        , nativeSophont = rawSolarSystem.nativeSophont
        , extinctSophont = rawSolarSystem.extinctSophont
        , sectorName = rawSolarSystem.sectorName
        }


finalToRaw : SolarSystem -> RawSolarSystem
finalToRaw solarSystem =
    let
        { sectorX, sectorY, x, y } =
            solarSystem.address
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
    , sectorX = sectorX
    , sectorY = sectorY
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


type alias SolarSystemDict =
    Dict.Dict String SolarSystem
