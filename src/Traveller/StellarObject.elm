module Traveller.StellarObject exposing (GasGiantData, InnerStarData, PlanetoidBeltData, PlanetoidData, SharedPData, StarData(..), StellarObject(..), codecStarData, codecStellarObject, getInnerStarData, getProfileString, getStarData, getStellarOrbit, isBrownDwarf)

import Codec exposing (Codec)
import Json.Decode as JsDecode
import Traveller.Moon as Moon exposing (Moon)
import Traveller.Point as Point exposing (StellarPoint)
import Traveller.StarColour exposing (StarColour, codecStarColour)


type alias SharedPData =
    { orbitPosition : StellarPoint
    , inclination : Float
    , eccentricity : Float
    , effectiveHZCODeviation : Float
    , size : String
    , orbit : Float
    , period : Float
    , composition : String
    , retrograde : Bool
    , trojanOffset : Maybe Float
    , axialTilt : Float
    , moons : List Moon
    , biomassRating : Int
    , biocomplexityCode : Int
    , biodiversityRating : Int
    , compatibilityRating : Int
    , resourceRating : Int
    , nativeSophont : Bool
    , extinctSophont : Bool
    , hasRing : Bool
    , albedo : Float
    , density : Maybe Float
    , greenhouse : Maybe Float
    , meanTemperature : Maybe Float
    , habitabilityRating : Maybe Int
    , orbitSequence : String
    , uwp : String
    , diameter : Float
    , gravity : Maybe Float
    , mass : Maybe Float
    , escapeVelocity : Maybe Float
    , safeJumpTime : String
    , orbitType : Int
    , au : Float
    }


type alias PlanetoidData =
    { orbitPosition : StellarPoint
    , inclination : Float
    , eccentricity : Float
    , effectiveHZCODeviation : Float
    , size : String
    , orbit : Float
    , period : Maybe Float
    , composition : String
    , retrograde : Bool
    , trojanOffset : Maybe Float
    , axialTilt : Float
    , moons : List Moon
    , biomassRating : Int
    , biocomplexityCode : Int
    , biodiversityRating : Int
    , compatibilityRating : Int
    , resourceRating : Int
    , nativeSophont : Bool
    , extinctSophont : Bool
    , hasRing : Bool
    , albedo : Float
    , density : Maybe Float
    , greenhouse : Maybe Float
    , meanTemperature : Maybe Float
    , orbitSequence : String
    , uwp : String
    , diameter : Float
    , gravity : Maybe Float
    , mass : Maybe Float
    , escapeVelocity : Maybe Float
    , safeJumpTime : String
    , orbitType : Int
    , au : Float

    -- , -- maybe not required for Planetoid?
    --   code : Maybe String
    }


type alias GasGiantData =
    { orbitPosition : StellarPoint
    , inclination : Float
    , eccentricity : Float
    , effectiveHZCODeviation : Float
    , code : String
    , diameter : Float
    , mass : Maybe Float
    , orbit : Float
    , moons : List Moon
    , hasRing : Bool
    , trojanOffset : Maybe Float
    , axialTilt : Float
    , period : Float
    , orbitSequence : String
    , safeJumpTime : String
    , orbitType : Int
    , au : Float
    }


type alias PlanetoidBeltData =
    { orbitPosition : StellarPoint
    , inclination : Float
    , eccentricity : Float
    , effectiveHZCODeviation : Float
    , orbit : Float
    , mType : Float
    , sType : Float
    , cType : Float
    , oType : Float
    , span : Float
    , bulk : Float
    , resourceRating : Float
    , period : Float
    , orbitSequence : String
    , uwp : String
    , safeJumpTime : String
    , orbitType : Int
    , au : Float
    }


type alias InnerStarData =
    { orbitPosition : StellarPoint
    , inclination : Float
    , eccentricity : Float
    , effectiveHZCODeviation : Float
    , stellarClass : String
    , stellarType : String
    , subtype : Maybe Int
    , orbitType : Int
    , mass : Maybe Float
    , diameter : Maybe Float
    , temperature : Maybe Int
    , age : Float
    , colour : Maybe StarColour
    , companion : Maybe StarData
    , orbit : Float
    , period : Float
    , baseline : Int
    , stellarObjects : List StellarObject
    , orbitSequence : String
    , safeJumpTime : String
    , au : Float
    , jumpShadow : Maybe Float
    }


isBrownDwarf : InnerStarData -> Bool
isBrownDwarf theStar =
    List.any (\a -> a == theStar.stellarType) [ "D", "Y", "T", "L" ]


{-| Since StarData is recursive, we need to use a Type for it, instead of an
alias
-}
type StarData
    = -- StarDataWrap is the type variant that contains the InnerStarData
      StarDataWrap
        -- InnerStarData is the actual data
        InnerStarData


type alias StellarOrbit =
    { orbitPosition : StellarPoint
    , orbitType : Int
    , orbit : Float
    , au : Float
    , orbitSequence : String
    }


getProfileString : StellarObject -> String
getProfileString stellarObject =
    case stellarObject of
        GasGiant gasGiantData ->
            gasGiantData.code

        TerrestrialPlanet terrestrialData ->
            terrestrialData.uwp

        PlanetoidBelt planetoidBeltData ->
            planetoidBeltData.uwp

        Planetoid planetoidData ->
            planetoidData.uwp

        Star (StarDataWrap starDataConfig) ->
            starDataConfig.stellarType
                ++ (starDataConfig.subtype |> Maybe.map String.fromInt |> Maybe.withDefault "")
                ++ " "
                ++ starDataConfig.stellarClass


extractStellarOrbit orbit =
    { orbitPosition = orbit.orbitPosition
    , orbitType = orbit.orbitType
    , orbit = orbit.orbit
    , au = orbit.au
    , orbitSequence = orbit.orbitSequence
    }


getSafeJumpTime : StellarObject -> String
getSafeJumpTime stellarObject =
    case stellarObject of
        GasGiant gasGiantData ->
            gasGiantData.safeJumpTime

        TerrestrialPlanet terrestrialData ->
            terrestrialData.safeJumpTime

        PlanetoidBelt planetoidBeltData ->
            planetoidBeltData.safeJumpTime

        Planetoid planetoidData ->
            planetoidData.safeJumpTime

        Star (StarDataWrap starDataConfig) ->
            starDataConfig.safeJumpTime


getStellarOrbit : StellarObject -> StellarOrbit
getStellarOrbit stellarObject =
    case stellarObject of
        GasGiant giantData ->
            extractStellarOrbit giantData

        TerrestrialPlanet terrestrialData ->
            extractStellarOrbit terrestrialData

        PlanetoidBelt planetoidBelt ->
            extractStellarOrbit planetoidBelt

        Planetoid planetoid ->
            extractStellarOrbit planetoid

        Star (StarDataWrap innerStarData) ->
            extractStellarOrbit innerStarData


getInnerStarData : StarData -> InnerStarData
getInnerStarData (StarDataWrap starDataConfig) =
    starDataConfig


type StellarObject
    = GasGiant GasGiantData
    | TerrestrialPlanet SharedPData
    | PlanetoidBelt PlanetoidBeltData
    | Planetoid SharedPData
    | Star StarData


{-| Returns the StarData if the StellarObject is a Star, otherwise Nothing
-}
getStarData : StellarObject -> Maybe StarData
getStarData stellarObject =
    case stellarObject of
        Star starData ->
            Just starData

        _ ->
            Nothing


{-| Returns the GasGiantData if the StellarObject is a GasGiant, otherwise Nothing
-}
getGasGiantData : StellarObject -> Maybe GasGiantData
getGasGiantData stellarObject =
    case stellarObject of
        GasGiant gasGiantData ->
            Just gasGiantData

        _ ->
            Nothing


{-| Returns the TerrestrialData if the StellarObject is a TerrestrialPlanet, otherwise Nothing
-}
getTerrestrialData : StellarObject -> Maybe SharedPData
getTerrestrialData stellarObject =
    case stellarObject of
        TerrestrialPlanet terrestrialData ->
            Just terrestrialData

        _ ->
            Nothing


{-| Returns the PlanetoidBeltData if the StellarObject is a PlanetoidBelt, otherwise Nothing
-}
getPlanetoidBeltData : StellarObject -> Maybe PlanetoidBeltData
getPlanetoidBeltData stellarObject =
    case stellarObject of
        PlanetoidBelt planetoidBeltData ->
            Just planetoidBeltData

        _ ->
            Nothing


{-| Returns the PlanetoidData if the StellarObject is a Planetoid, otherwise Nothing
-}
getPlanetoidData : StellarObject -> Maybe SharedPData
getPlanetoidData stellarObject =
    case stellarObject of
        Planetoid planetoidData ->
            Just planetoidData

        _ ->
            Nothing


codecPlanetoidBeltData : Codec PlanetoidBeltData
codecPlanetoidBeltData =
    Codec.object PlanetoidBeltData
        |> Codec.field "orbitPosition" .orbitPosition Point.codec
        |> Codec.field "inclination" .inclination Codec.float
        |> Codec.field "eccentricity" .eccentricity Codec.float
        |> Codec.field "effectiveHZCODeviation" .effectiveHZCODeviation Codec.float
        |> Codec.field "orbit" .orbit Codec.float
        |> Codec.field "mType" .mType Codec.float
        |> Codec.field "sType" .sType Codec.float
        |> Codec.field "cType" .cType Codec.float
        |> Codec.field "oType" .oType Codec.float
        |> Codec.field "span" .span Codec.float
        |> Codec.field "bulk" .bulk Codec.float
        |> Codec.field "resourceRating" .resourceRating Codec.float
        |> Codec.field "period" .period Codec.float
        |> Codec.field "orbitSequence" .orbitSequence Codec.string
        |> Codec.field "uwp" .uwp Codec.string
        |> Codec.field "safeJumpTime" .safeJumpTime Codec.string
        |> Codec.field "orbitType" .orbitType Codec.int
        |> Codec.field "au" .au Codec.float
        |> Codec.buildObject


codecGasGiantData : Codec GasGiantData
codecGasGiantData =
    Codec.object GasGiantData
        |> Codec.field "orbitPosition" .orbitPosition Point.codec
        |> Codec.field "inclination" .inclination Codec.float
        |> Codec.field "eccentricity" .eccentricity Codec.float
        |> Codec.field "effectiveHZCODeviation" .effectiveHZCODeviation Codec.float
        |> Codec.field "code" .code Codec.string
        |> Codec.field "diameter" .diameter Codec.float
        |> Codec.field "mass" .mass (Codec.nullable Codec.float)
        |> Codec.field "orbit" .orbit Codec.float
        |> Codec.field "moons" .moons (Codec.list Moon.codec)
        |> Codec.field "hasRing" .hasRing Codec.bool
        |> Codec.field "trojanOffset" .trojanOffset (Codec.nullable Codec.float)
        |> Codec.field "axialTilt" .orbit Codec.float
        |> Codec.field "period" .orbit Codec.float
        |> Codec.field "orbitSequence" .orbitSequence Codec.string
        |> Codec.field "safeJumpTime" .safeJumpTime Codec.string
        |> Codec.field "orbitType" .orbitType Codec.int
        |> Codec.field "au" .au Codec.float
        |> Codec.buildObject


codecSharedPData : Codec SharedPData
codecSharedPData =
    Codec.object SharedPData
        |> Codec.field "orbitPosition" .orbitPosition Point.codec
        |> Codec.field "inclination" .inclination Codec.float
        |> Codec.field "eccentricity" .eccentricity Codec.float
        |> Codec.field "effectiveHZCODeviation" .effectiveHZCODeviation Codec.float
        |> Codec.field "size"
            .size
            (Codec.oneOf Codec.string
                [ -- force int to string
                  Codec.int |> Codec.andThen (String.fromInt >> Codec.succeed) (String.toInt >> Maybe.withDefault 999999)
                ]
            )
        |> Codec.field "orbit" .orbit Codec.float
        |> Codec.field "period" .period Codec.float
        |> Codec.field "composition" .composition Codec.string
        |> Codec.field "retrograde" .retrograde Codec.bool
        |> Codec.field "trojanOffset" .trojanOffset (Codec.nullable Codec.float)
        |> Codec.field "axialTilt" .axialTilt Codec.float
        |> Codec.field "moons" .moons (Codec.list (Codec.lazy (\_ -> Moon.codec)))
        |> Codec.field "biomassRating" .biomassRating Codec.int
        |> Codec.field "biocomplexityCode" .biocomplexityCode Codec.int
        |> Codec.field "biodiversityRating" .biodiversityRating Codec.int
        |> Codec.field "compatibilityRating" .compatibilityRating Codec.int
        |> Codec.field "resourceRating" .resourceRating Codec.int
        |> Codec.field "nativeSophont" .nativeSophont Codec.bool
        |> Codec.field "extinctSophont" .extinctSophont Codec.bool
        |> Codec.field "hasRing" .hasRing Codec.bool
        |> Codec.field "albedo" .albedo Codec.float
        |> Codec.optionalField "density" .density Codec.float
        |> Codec.optionalField "greenhouse" .greenhouse Codec.float
        |> Codec.field "meanTemperature" .meanTemperature (Codec.nullable Codec.float)
        |> Codec.optionalField "habitabilityRating" .habitabilityRating Codec.int
        |> Codec.field "orbitSequence" .orbitSequence Codec.string
        |> Codec.field "uwp" .uwp Codec.string
        |> Codec.field "diameter" .diameter Codec.float
        |> Codec.field "gravity" .gravity (Codec.nullable Codec.float)
        |> Codec.field "mass" .mass (Codec.nullable Codec.float)
        |> Codec.field "escapeVelocity" .escapeVelocity (Codec.nullable Codec.float)
        |> Codec.field "safeJumpTime" .safeJumpTime Codec.string
        |> Codec.field "orbitType" .orbitType Codec.int
        |> Codec.field "au" .au Codec.float
        |> Codec.buildObject


decodeStellarObject : JsDecode.Decoder StellarObject
decodeStellarObject =
    JsDecode.oneOf
        [ JsDecode.map GasGiant (Codec.decoder codecGasGiantData)
        , JsDecode.map TerrestrialPlanet (Codec.decoder codecSharedPData)
        , JsDecode.map PlanetoidBelt (Codec.decoder codecPlanetoidBeltData)
        , JsDecode.map Planetoid (Codec.decoder codecSharedPData)
        , JsDecode.map Star (Codec.decoder codecStarData)
        ]


codecStarData : Codec StarData
codecStarData =
    Codec.object InnerStarData
        |> Codec.field "orbitPosition" .orbitPosition Point.codec
        |> Codec.field "inclination" .inclination Codec.float
        |> Codec.field "eccentricity" .eccentricity Codec.float
        |> Codec.field "effectiveHZCODeviation" .effectiveHZCODeviation Codec.float
        |> Codec.field "stellarClass" .stellarClass Codec.string
        |> Codec.field "stellarType" .stellarType Codec.string
        |> Codec.field "subtype" .subtype (Codec.nullable Codec.int)
        |> Codec.field "orbitType" .orbitType Codec.int
        |> Codec.field "mass" .mass (Codec.nullable Codec.float)
        |> Codec.field "diameter" .diameter (Codec.nullable Codec.float)
        |> Codec.field "temperature" .temperature (Codec.nullable Codec.int)
        |> Codec.field "age" .age Codec.float
        |> Codec.optionalField "colour" .colour codecStarColour
        |> Codec.field "companion" .companion (Codec.nullable <| Codec.lazy (\_ -> codecStarData))
        |> Codec.field "orbit" .orbit Codec.float
        |> Codec.field "period" .period Codec.float
        |> Codec.field "baseline" .baseline Codec.int
        |> Codec.field "stellarObjects" .stellarObjects (Codec.list (Codec.lazy (\_ -> codecStellarObject)))
        |> Codec.field "orbitSequence" .orbitSequence Codec.string
        |> Codec.field "safeJumpTime" .safeJumpTime Codec.string
        |> Codec.field "au" .au Codec.float
        |> Codec.field "jumpShadow" .jumpShadow (Codec.nullable Codec.float)
        |> Codec.buildObject
        |> Codec.map StarDataWrap (\(StarDataWrap data) -> data)


encodeStellarObject : StellarObject -> Codec.Value
encodeStellarObject stellarObject =
    case stellarObject of
        GasGiant data ->
            Codec.encodeToValue codecGasGiantData data

        TerrestrialPlanet data ->
            Codec.encodeToValue codecSharedPData data

        PlanetoidBelt data ->
            Codec.encodeToValue codecPlanetoidBeltData data

        Planetoid data ->
            Codec.encodeToValue codecSharedPData data

        Star data ->
            Codec.encodeToValue codecStarData data


codecStellarObject : Codec StellarObject
codecStellarObject =
    Codec.build
        encodeStellarObject
        decodeStellarObject
