module Traveller.SolarSystemStars exposing (FallibleStarSystem, StarSystem, StarType, StarTypeData, fallibleStarSystemDecoder, getStarTypeData, isBrownDwarfType, starSystemCodec, starTypeCodec)

import Codec exposing (Codec)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
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
    Codec.object StarTypeData
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


type alias FallibleStarSystem =
    { address : HexAddress
    , sectorName : String
    , name : String
    , scanPoints : Int
    , surveyIndex : Int
    , gasGiantCount : Int
    , terrestrialPlanetCount : Int
    , planetoidBeltCount : Int
    , allegiance : Maybe String
    , stars : List (Result Decode.Error StarType)
    }


starSystemCodec : Codec StarSystem
starSystemCodec =
    Codec.object
        (\ox oy sname name sp si ggc tpc ppc al stars ->
            StarSystem { x = ox, y = oy } sname name sp si ggc tpc ppc al stars
        )
        |> Codec.field "origin_x" (.address >> .x) Codec.int
        |> Codec.field "origin_y" (.address >> .y) Codec.int
        |> Codec.field "sector_name" .sectorName Codec.string
        |> Codec.field "name" .name Codec.string
        |> Codec.field "scan_points" .scanPoints Codec.int
        |> Codec.field "survey_index" .surveyIndex Codec.int
        |> Codec.field "gas_giant_count" .gasGiantCount Codec.int
        |> Codec.field "terrestrial_planet_count" .terrestrialPlanetCount Codec.int
        |> Codec.field "planetoid_belt_count" .planetoidBeltCount Codec.int
        |> Codec.field "allegiance" .allegiance (Codec.nullable Codec.string)
        |> Codec.field "stars" .stars (Codec.list starTypeCodec)
        |> Codec.buildObject


{-| all the fields from StarSystem, but with stars as a list of Result Decode.Error StarType

this is so the decoder can handle errors in the starTypeCodec

-}
fallibleStarSystemDecoder : Decoder FallibleStarSystem
fallibleStarSystemDecoder =
    let
        decodeOrCatchError : Decode.Value -> Result Decode.Error StarType
        decodeOrCatchError =
            Decode.decodeValue (Codec.decoder starTypeCodec)
    in
    Decode.succeed
        (\ox oy sname name sp si ggc tpc ppc al stars ->
            FallibleStarSystem { x = ox, y = oy } sname name sp si ggc tpc ppc al stars
        )
        |> required "origin_x" Decode.int
        |> required "origin_y" Decode.int
        |> required "sector_name" Decode.string
        |> required "name" Decode.string
        |> required "scan_points" Decode.int
        |> required "survey_index" Decode.int
        |> required "gas_giant_count" Decode.int
        |> required "terrestrial_planet_count" Decode.int
        |> required "planetoid_belt_count" Decode.int
        |> required "allegiance" (Decode.nullable Decode.string)
        |> required "stars"
            (Decode.list
                (Decode.map decodeOrCatchError Decode.value)
            )
