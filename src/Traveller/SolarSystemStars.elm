module Traveller.SolarSystemStars exposing (StarSystem, StarType, StarTypeData, getStarTypeData, isBrownDwarfType, starSystemCodec, starTypeCodec)

import Codec exposing (Codec, lazy, list, object)
import Traveller.HexAddress as HexAddress exposing (HexAddress)
import Traveller.StarColour exposing (StarColour, codecStarColour)


type alias StarTypeData =
    { au : Float
    , subtype : Maybe Int
    , companion : Maybe StarType
    , stellarType : String
    , stellarClass : String
    , colour : Maybe StarColour
    , diameter : Maybe Float
    }


isBrownDwarfType : StarTypeData -> Bool
isBrownDwarfType theStar =
    List.any (\a -> a == theStar.stellarType) [ "D", "Y", "T", "L" ]


type StarType
    = StarTypeWrap StarTypeData


getStarTypeData : StarType -> StarTypeData
getStarTypeData (StarTypeWrap starTypeData) =
    starTypeData


starTypeCodec : Codec StarType
starTypeCodec =
    object StarTypeData
        |> Codec.field "au" .au Codec.float
        |> Codec.field "subtype" .subtype (Codec.nullable Codec.int)
        |> Codec.field "companion" .companion (Codec.nullable <| Codec.lazy (\_ -> starTypeCodec))
        |> Codec.field "stellarType" .stellarType Codec.string
        |> Codec.field "stellarClass" .stellarClass Codec.string
        |> Codec.field "colour" .colour (Codec.nullable codecStarColour)
        |> Codec.field "diameter" .diameter (Codec.nullable Codec.float)
        |> Codec.buildObject
        |> Codec.map StarTypeWrap (\(StarTypeWrap data) -> data)


type alias StarSystem =
    { address : HexAddress
    , sectorName : String
    , name : String
    , scanPoints : Int
    , surveyIndex : Int
    , gasGiantCount : Int
    , terrestrialPlanetCount : Int
    , planetoidBeltCount : Int
    , allegiance : Maybe String
    , stars : List StarType
    }


type alias RawStarSystem =
    { name : String
    , originX : Int
    , originY : Int
    , x : Int
    , y : Int
    , sectorX : Int
    , sectorY : Int
    , sectorName : String
    , scanPoints : Int
    , surveyIndex : Int
    , gasGiantCount : Int
    , terrestrialPlanetCount : Int
    , planetoidBeltCount : Int
    , allegiance : Maybe String
    , stars : List StarType
    }


starSystemCodec : Codec.Codec StarSystem
starSystemCodec =
    rawStarSystemCodec
        |> Codec.andThen rawStarSystemToStarSystem starSystemToRawStarSystem


rawStarSystemToStarSystem : RawStarSystem -> Codec.Codec StarSystem
rawStarSystemToStarSystem rawStarSystem =
    Codec.succeed
        { address = { x = rawStarSystem.originX, y = rawStarSystem.originY }
        , sectorName = rawStarSystem.sectorName
        , name = rawStarSystem.name
        , gasGiantCount = rawStarSystem.gasGiantCount
        , planetoidBeltCount = rawStarSystem.planetoidBeltCount
        , terrestrialPlanetCount = rawStarSystem.terrestrialPlanetCount
        , surveyIndex = rawStarSystem.surveyIndex
        , scanPoints = rawStarSystem.scanPoints
        , allegiance = rawStarSystem.allegiance
        , stars = rawStarSystem.stars
        }


starSystemToRawStarSystem : StarSystem -> RawStarSystem
starSystemToRawStarSystem starSystem =
    { originX = 99999999
    , originY = 99999999
    , x = 99999999
    , y = 99999999
    , sectorX = 99999999
    , sectorY = 99999999
    , gasGiantCount = starSystem.gasGiantCount
    , planetoidBeltCount = starSystem.planetoidBeltCount
    , terrestrialPlanetCount = starSystem.terrestrialPlanetCount
    , surveyIndex = starSystem.surveyIndex
    , sectorName = starSystem.sectorName
    , name = starSystem.name
    , allegiance = starSystem.allegiance
    , stars = starSystem.stars
    , scanPoints = starSystem.scanPoints
    }


rawStarSystemCodec : Codec RawStarSystem
rawStarSystemCodec =
    object RawStarSystem
        |> Codec.field "name" .name Codec.string
        |> Codec.field "origin_x" .originX Codec.int
        |> Codec.field "origin_y" .originY Codec.int
        |> Codec.field "x" .x Codec.int
        |> Codec.field "y" .y Codec.int
        |> Codec.field "sector_x" .sectorX Codec.int
        |> Codec.field "sector_y" .sectorY Codec.int
        |> Codec.field "sector_name" .sectorName Codec.string
        |> Codec.field "scan_points" .scanPoints Codec.int
        |> Codec.field "survey_index" .surveyIndex Codec.int
        |> Codec.field "gas_giant_count" .gasGiantCount Codec.int
        |> Codec.field "terrestrial_planet_count" .terrestrialPlanetCount Codec.int
        |> Codec.field "planetoid_belt_count" .planetoidBeltCount Codec.int
        |> Codec.field "allegiance" .allegiance (Codec.nullable Codec.string)
        |> Codec.field "stars" .stars (list starTypeCodec)
        |> Codec.buildObject
