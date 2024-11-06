module Traveller.StellarObject exposing (GasGiantData, PlanetoidBeltData, PlanetoidData, StarData(..), StarDataConfig, StellarObject(..), TerrestrialData, codecStarData, codecStellarObject, getStarDataConfig, getStellarOrbit, starColourRGB)

import Codec exposing (Codec)
import Json.Decode as JsDecode
import Json.Decode.Pipeline as Decode
import Json.Encode as JsEncode
import Parser exposing ((|.), (|=), Parser)
import Parser.Extras as Parser
import Random.Char as Codec
import Traveller.Atmosphere as Atmosphere exposing (StellarAtmosphere)
import Traveller.Hydrographics as Hydrographics exposing (StellarHydrographics)
import Traveller.Moon as Moon exposing (Moon)
import Traveller.Orbit as Orbit exposing (StellarOrbit)
import Traveller.Point as Point exposing (StellarPoint)
import Traveller.Population as Population exposing (StellarPopulation)


type StarColour
    = Blue
    | BlueWhite
    | White
    | YellowWhite
    | Yellow
    | LightOrange
    | OrangeRed
    | Red
    | Brown
    | DeepDimRed


codecStarColour : Codec StarColour
codecStarColour =
    Codec.enum Codec.string
        [ ( "Blue", Blue )
        , ( "Blue White", BlueWhite )
        , ( "White", White )
        , ( "Yellow White", YellowWhite )
        , ( "Yellow", Yellow )
        , ( "Light Orange", LightOrange )
        , ( "Orange Red", OrangeRed )
        , ( "Red", Red )
        , ( "Brown", Brown )
        , ( "Deep Dim Red", DeepDimRed )
        ]


starColourRGB : StarColour -> String
starColourRGB colour =
    case colour of
        Blue ->
            "#000077"

        BlueWhite ->
            "#87cefa"

        White ->
            "#FFFFFF"

        YellowWhite ->
            "#ffffe0"

        Yellow ->
            "#ffff00"

        LightOrange ->
            "#ffbf00"

        OrangeRed ->
            "#ff4500"

        Red ->
            "#ff0000"

        Brown ->
            "#f4a460"

        DeepDimRed ->
            "#800000"


type alias TerrestrialData =
    { orbitPosition : StellarPoint
    , inclination : Float
    , eccentricity : Float
    , effectiveHZCODeviation : Float
    , size : Int
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
    , effectiveHZCODeviation : Maybe Float
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


type alias StarDataConfig =
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
    , colour : StarColour
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


type StarData
    = StarData StarDataConfig


type alias StellarOrbit =
    { orbitPosition : StellarPoint
    , orbitType : Int
    , orbit : Float
    , au : Float
    }


extractStellarOrbit orbit =
    { orbitPosition = orbit.orbitPosition, orbitType = orbit.orbitType, orbit = orbit.orbit, au = orbit.au }


getStellarOrbit : StellarObject -> StellarOrbit
getStellarOrbit stellarObject =
    case stellarObject of
        GasGiant giantData ->
            extractStellarOrbit giantData

        TerrestrialPlanet p ->
            extractStellarOrbit p

        PlanetoidBelt p ->
            extractStellarOrbit p

        Planetoid p ->
            extractStellarOrbit p

        Star (StarData s) ->
            extractStellarOrbit s


getStarDataConfig : StarData -> StarDataConfig
getStarDataConfig (StarData starDataConfig) =
    starDataConfig


type StellarObject
    = GasGiant GasGiantData
    | TerrestrialPlanet TerrestrialData
    | PlanetoidBelt PlanetoidBeltData
    | Planetoid PlanetoidData
    | Star StarData


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
        |> Codec.field "effectiveHZCODeviation" .effectiveHZCODeviation (Codec.nullable Codec.float)
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


codecTerrestrialData : Codec TerrestrialData
codecTerrestrialData =
    Codec.object TerrestrialData
        |> Codec.field "orbitPosition" .orbitPosition Point.codec
        |> Codec.field "inclination" .inclination Codec.float
        |> Codec.field "eccentricity" .eccentricity Codec.float
        |> Codec.field "effectiveHZCODeviation" .effectiveHZCODeviation Codec.float
        |> Codec.field "size" .size Codec.int
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


codecPlanetoidData : Codec PlanetoidData
codecPlanetoidData =
    Codec.object PlanetoidData
        |> Codec.field "orbitPosition" .orbitPosition Point.codec
        |> Codec.field "inclination" .inclination Codec.float
        |> Codec.field "eccentricity" .eccentricity Codec.float
        |> Codec.field "effectiveHZCODeviation" .effectiveHZCODeviation Codec.float
        |> Codec.field "size" .size Codec.string
        |> Codec.field "orbit" .orbit Codec.float
        |> Codec.field "period" .period (Codec.nullable Codec.float)
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
        |> Codec.field "orbitSequence" .orbitSequence Codec.string
        |> Codec.field "uwp" .uwp Codec.string
        |> Codec.field "diameter" .diameter Codec.float
        |> Codec.field "gravity" .gravity (Codec.nullable Codec.float)
        |> Codec.field "mass" .mass (Codec.nullable Codec.float)
        |> Codec.field "escapeVelocity" .escapeVelocity (Codec.nullable Codec.float)
        |> Codec.field "safeJumpTime" .safeJumpTime Codec.string
        |> Codec.field "orbitType" .orbitType Codec.int
        -- |> Codec.optionalField "code" .code Codec.string
        |> Codec.field "au" .au Codec.float
        |> Codec.buildObject


decodeStellarObjectX : JsDecode.Decoder StellarObject
decodeStellarObjectX =
    JsDecode.oneOf
        [ JsDecode.map GasGiant (Codec.decoder codecGasGiantData)
        , JsDecode.map TerrestrialPlanet (Codec.decoder codecTerrestrialData)
        , JsDecode.map PlanetoidBelt (Codec.decoder codecPlanetoidBeltData)
        , JsDecode.map Planetoid (Codec.decoder codecPlanetoidData)
        , JsDecode.map Star (Codec.decoder codecStarData)
        ]


codecStarData : Codec StarData
codecStarData =
    Codec.object StarDataConfig
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
        |> Codec.field "colour" .colour codecStarColour
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
        |> Codec.map StarData (\(StarData data) -> data)


encodeStellarObjectX : StellarObject -> Codec.Value
encodeStellarObjectX stellarObjectX =
    case stellarObjectX of
        GasGiant data ->
            Codec.encodeToValue codecGasGiantData data

        TerrestrialPlanet data ->
            Codec.encodeToValue codecTerrestrialData data

        PlanetoidBelt data ->
            Codec.encodeToValue codecPlanetoidBeltData data

        Planetoid data ->
            Codec.encodeToValue codecPlanetoidData data

        Star data ->
            Codec.encodeToValue codecStarData data


codecStellarObject : Codec StellarObject
codecStellarObject =
    Codec.build
        encodeStellarObjectX
        decodeStellarObjectX
