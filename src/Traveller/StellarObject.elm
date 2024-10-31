module Traveller.StellarObject exposing (StellarObject(..), codecStellarObject, sampleStellarObject)

import Codec exposing (Codec)
import Json.Decode as JsDecode
import Json.Encode as JsEncode
import Parser exposing ((|.), (|=), Parser)
import Parser.Extras as Parser
import Traveller.Atmosphere exposing (StellarAtmosphere, codecStellarAtmosphere)
import Traveller.Hydrographics exposing (StellarHydrographics, codecStellarHydrographics)
import Traveller.Orbit exposing (StellarOrbit, codecStellarOrbit)
import Traveller.Point exposing (StellarPoint, codecStellarPoint)
import Traveller.Population exposing (StellarPopulation, codecStellarPopulation)
import Traveller.StellarMoon exposing (StellarMoon(..), codecStellarMoon)


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


{-| the data structure for a stellar object.
It needs to be separate from StellarObject so that it can nest within itself
-}
type alias StellarData =
    { orbitPosition : StellarPoint
    , inclination : Float
    , eccentricity : Float
    , effectiveHZCODeviation : Maybe Float
    , size : Maybe String
    , orbit : StellarOrbit
    , period : Maybe Float
    , composition : Maybe String
    , retrograde : Maybe Bool
    , trojanOffset : Maybe Float
    , axialTilt : Maybe Float
    , moons : Maybe (List StellarMoon)
    , biomassRating : Maybe Int
    , biocomplexityCode : Maybe Int
    , biodiversityRating : Maybe Int
    , compatibilityRating : Maybe Int
    , resourceRating : Maybe Int
    , currentNativeSophont : Maybe Bool
    , extinctNativeSophont : Maybe Bool
    , hasRing : Maybe Bool
    , orbitType : Int
    , atmosphere : Maybe StellarAtmosphere
    , hydrographics : Maybe StellarHydrographics
    , population : Maybe StellarPopulation
    , governmentCode : Maybe Int
    , lawLevelCode : Maybe Int
    , starPort : Maybe String
    , techLevel : Maybe Int
    , tradeCodes : Maybe (List String)
    , albedo : Maybe Float
    , density : Maybe Float
    , greenhouse : Maybe Int
    , meanTemperature : Maybe Float
    , orbitSequence : String
    , uwp : Maybe String
    , code : Maybe String
    , jumpShadow : Maybe Float
    , safeJumpTime : String
    }


type StellarObject
    = -- needs to be a type instead of an alias, because its recursive
      StellarObject StellarData


buildStellarObject =
    \orbPos incl ecc effHZCODev size orbit period comp retro troj axTilt moons biomass bioComplex bioDiversity compat resource currentNative extinctNative hasRing orbitType atm hydro pop gov law starPort tech tradeCodes albedo dens green meanTemp orbitSeq uwp code jumpShadow safeJumpTime ->
        -- StellarObject
        { orbitPosition = orbPos
        , inclination = incl
        , eccentricity = ecc
        , effectiveHZCODeviation = effHZCODev
        , size = size
        , orbit = orbit
        , period = period
        , composition = comp
        , retrograde = retro
        , trojanOffset = troj
        , axialTilt = axTilt
        , moons = moons
        , biomassRating = biomass
        , biocomplexityCode = bioComplex
        , biodiversityRating = bioDiversity
        , compatibilityRating = compat
        , resourceRating = resource
        , currentNativeSophont = currentNative
        , extinctNativeSophont = extinctNative
        , hasRing = hasRing
        , orbitType = orbitType
        , atmosphere = atm
        , hydrographics = hydro
        , population = pop
        , governmentCode = gov
        , lawLevelCode = law
        , starPort = starPort
        , techLevel = tech
        , tradeCodes = tradeCodes
        , albedo = albedo
        , density = dens
        , greenhouse = green
        , meanTemperature = meanTemp
        , orbitSequence = orbitSeq
        , uwp = uwp
        , code = code
        , jumpShadow = jumpShadow
        , safeJumpTime = safeJumpTime
        }


codecStellarObject : Codec StellarObject
codecStellarObject =
    Codec.object
        buildStellarObject
        |> Codec.field "orbitPosition" .orbitPosition codecStellarPoint
        |> Codec.field "inclination" .inclination Codec.float
        |> Codec.field "eccentricity" .eccentricity Codec.float
        |> Codec.field "effectiveHZCODeviation" .effectiveHZCODeviation (Codec.nullable Codec.float)
        |> Codec.maybeField "size" .size Codec.string
        |> Codec.field "orbit" .orbit codecStellarOrbit
        |> Codec.nullableField "period" .period Codec.float
        |> Codec.maybeField "composition" .composition Codec.string
        |> Codec.maybeField "retrograde" .retrograde Codec.bool
        |> Codec.maybeField "trojanOffset" .trojanOffset Codec.float
        |> Codec.maybeField "axialTilt" .axialTilt Codec.float
        |> Codec.maybeField "moons" .moons (Codec.list (Codec.lazy (\_ -> codecStellarMoon)))
        |> Codec.maybeField "biomassRating" .biomassRating Codec.int
        |> Codec.maybeField "biocomplexityCode" .biocomplexityCode Codec.int
        |> Codec.maybeField "biodiversityRating" .biodiversityRating Codec.int
        |> Codec.maybeField "compatibilityRating" .compatibilityRating Codec.int
        |> Codec.maybeField "resourceRating" .resourceRating Codec.int
        |> Codec.maybeField "currentNativeSophont" .currentNativeSophont Codec.bool
        |> Codec.maybeField "extinctNativeSophont" .extinctNativeSophont Codec.bool
        |> Codec.maybeField "hasRing" .hasRing Codec.bool
        |> Codec.field "orbitType" .orbitType Codec.int
        |> Codec.maybeField "atmosphere" .atmosphere codecStellarAtmosphere
        |> Codec.maybeField "hydrographics" .hydrographics codecStellarHydrographics
        |> Codec.maybeField "population" .population codecStellarPopulation
        |> Codec.maybeField "governmentCode" .governmentCode Codec.int
        |> Codec.maybeField "lawLevelCode" .lawLevelCode Codec.int
        |> Codec.maybeField "starPort" .starPort Codec.string
        |> Codec.maybeField "techLevel" .techLevel Codec.int
        |> Codec.maybeField "tradeCodes" .tradeCodes (Codec.list Codec.string)
        |> Codec.maybeField "albedo" .albedo Codec.float
        |> Codec.maybeField "density" .density Codec.float
        |> Codec.maybeField "greenhouse" .greenhouse Codec.int
        |> Codec.maybeField "meanTemperature" .meanTemperature Codec.float
        |> Codec.field "orbitSequence" .orbitSequence Codec.string
        |> Codec.maybeField "uwp" .uwp Codec.string
        |> Codec.maybeField "code" .code Codec.string
        |> Codec.optionalField "jumpShadow" .jumpShadow Codec.float
        |> Codec.field "safeJumpTime" .safeJumpTime Codec.string
        |> Codec.buildObject
        |> -- Codec.map needs a way to go from object, and a way to go back to object
           Codec.map StellarObject (\(StellarObject data) -> data)


sampleStellarObject : String
sampleStellarObject =
    """
   {
      "orbitPosition": {
        "x": 6402486.238018146,
        "y": 18065208.668342955
      },
      "inclination": 6,
      "eccentricity": 0.2138419510306176,
      "effectiveHZCODeviation": 9.347833368041591,
      "size": "S",
      "orbit": 0.32029549592764617,
      "period": 0.04372400061536713,
      "composition": "Exotic Ice",
      "retrograde": false,
      "trojanOffset": null,
      "axialTilt": 55.26778554821404,
      "moons": [],
      "biomassRating": 0,
      "biocomplexityCode": 0,
      "biodiversityRating": 0,
      "compatibilityRating": 0,
      "resourceRating": 2,
      "currentNativeSophont": false,
      "extinctNativeSophont": false,
      "hasRing": false,
      "orbitType": 13,
      "atmosphere": {
        "code": 0,
        "irritant": false,
        "taint": {
          "subtype": "",
          "code": "",
          "severity": 0,
          "persistence": 0
        },
        "characteristic": "",
        "bar": 0.9165077659702823,
        "gasType": null,
        "density": "Standard",
        "hazardCode": null
      },
      "hydrographics": {
        "code": 0,
        "distribution": 0
      },
      "population": {
        "code": 0,
        "concentrationRating": null
      },
      "governmentCode": 0,
      "lawLevelCode": 0,
      "starPort": "X",
      "techLevel": 0,
      "tradeCodes": [],
      "albedo": 0.67,
      "density": 0.01944642549312882,
      "greenhouse": 0,
      "meanTemperature": 644.0929526494541,
      "orbitSequence": "A V"
    }
    """


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
    , moons : List StellarMoon
    , biomassRating : Int
    , biocomplexityCode : Int
    , biodiversityRating : Int
    , compatibilityRating : Int
    , resourceRating : Int
    , currentNativeSophont : Bool
    , extinctNativeSophont : Bool
    , hasRing : Bool
    , albedo : Float
    , density : Float
    , greenhouse : Float
    , meanTemperature : Float
    , nativeSophont : Bool
    , extinctSophont : Bool
    , habitableRating : Int
    , orbitSequence : String
    , uwp : String
    , diameter : Float
    , gravity : Float
    , mass : Float
    , escapeVelocity : Float
    , safeJumpTime : String
    }


type alias PlanetoidData =
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
    , moons : List StellarMoon
    , biomassRating : Int
    , biocomplexityCode : Int
    , biodiversityRating : Int
    , compatibilityRating : Int
    , resourceRating : Int
    , currentNativeSophont : Bool
    , extinctNativeSophont : Bool
    , hasRing : Bool
    , albedo : Float
    , density : Float
    , greenhouse : Float
    , meanTemperature : Float
    , orbitSequence : String
    , uwp : String
    , diameter : Float
    , gravity : Maybe Float
    , mass : Maybe Float
    , escapeVelocity : Maybe Float
    , safeJumpTime : String
    }


type alias GasGiantData =
    { orbitPosition : StellarPoint
    , inclination : Float
    , eccentricity : Float
    , effectiveHZCODeviation : Maybe Float
    , code : String
    , diameter : Float
    , mass : Float
    , orbit : Float
    , moons : List StellarMoon
    , hasRing : Bool
    , trojanOffset : Maybe Float
    , axialTilt : Float
    , period : Float
    , orbitSequence : String
    , safeJumpTime : String
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
    }


type alias StarDataConfig =
    { orbitPosition : StellarPoint
    , inclination : Int
    , eccentricity : Float
    , effectiveHZCODeviation : Float
    , stellarClass : String
    , stellarType : String
    , subtype : Maybe Int
    , orbitType : Int
    , mass : Float
    , diameter : Float
    , temperature : Int
    , age : Float
    , colour : StarColour
    , companion : Maybe StarData
    , orbit : Float
    , period : Float
    , baseline : Int
    , stellarObjects : List StellarObject
    , orbitSequence : String
    , jump : Float
    }


type StarData
    = StarData StarDataConfig


type StellarObjectX
    = GasGiant GasGiantData
    | TerrestrialPlanet TerrestrialData
    | PlanetoidBelt PlanetoidBeltData
    | Planetoid PlanetoidData
    | Star StarData


codecPlanetoidBeltData : Codec PlanetoidBeltData
codecPlanetoidBeltData =
    Codec.object
        PlanetoidBeltData
        |> Codec.field "orbitPosition" .orbitPosition codecStellarPoint
        |> Codec.field "inclination" .inclination Codec.float
        |> Codec.field "eccentricity" .eccentricity Codec.float
        |> Codec.field "effectiveHZCODeviation" .effectiveHZCODeviation Codec.float
        |> Codec.field "orbit" .orbit Codec.float
        |> Codec.field "mType" .orbit Codec.float
        |> Codec.field "sType" .orbit Codec.float
        |> Codec.field "cType" .orbit Codec.float
        |> Codec.field "oType" .orbit Codec.float
        |> Codec.field "span" .orbit Codec.float
        |> Codec.field "bulk" .orbit Codec.float
        |> Codec.field "resourceRating" .orbit Codec.float
        |> Codec.field "period" .orbit Codec.float
        |> Codec.field "orbitSequence" .orbitSequence Codec.string
        |> Codec.field "uwp" .orbitSequence Codec.string
        |> Codec.field "safeJumpTime" .orbitSequence Codec.string
        |> Codec.buildObject


codecGasGiantData : Codec GasGiantData
codecGasGiantData =
    Codec.object
        GasGiantData
        |> Codec.field "orbitPosition" .orbitPosition codecStellarPoint
        |> Codec.field "inclination" .inclination Codec.float
        |> Codec.field "eccentricity" .eccentricity Codec.float
        |> Codec.field "effectiveHZCODeviation" .effectiveHZCODeviation (Codec.nullable Codec.float)
        |> Codec.field "code" .code Codec.string
        |> Codec.field "diameter" .diameter Codec.float
        |> Codec.field "mass" .mass Codec.float
        |> Codec.field "orbit" .orbit Codec.float
        |> Codec.field "moons" .moons (Codec.list (Codec.lazy (\_ -> codecStellarMoon)))
        |> Codec.field "hasRing" .hasRing Codec.bool
        |> Codec.field "trojanOffset" .trojanOffset (Codec.nullable Codec.float)
        |> Codec.field "axialTilt" .orbit Codec.float
        |> Codec.field "period" .orbit Codec.float
        |> Codec.field "orbitSequence" .orbitSequence Codec.string
        |> Codec.field "safeJumpTime" .safeJumpTime Codec.string
        |> Codec.buildObject


codecTerrestrialData : Codec TerrestrialData
codecTerrestrialData =
    Codec.object
        TerrestrialData
        |> Codec.field "orbitPosition" .orbitPosition codecStellarPoint
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
        |> Codec.field "moons" .moons (Codec.list (Codec.lazy (\_ -> codecStellarMoon)))
        |> Codec.field "biomassRating" .biomassRating Codec.int
        |> Codec.field "biocomplexityCode" .biocomplexityCode Codec.int
        |> Codec.field "biodiversityRating" .biodiversityRating Codec.int
        |> Codec.field "compatibilityRating" .compatibilityRating Codec.int
        |> Codec.field "resourceRating" .resourceRating Codec.int
        |> Codec.field "currentNativeSophont" .currentNativeSophont Codec.bool
        |> Codec.field "extinctNativeSophont" .extinctNativeSophont Codec.bool
        |> Codec.field "hasRing" .hasRing Codec.bool
        |> Codec.field "albedo" .albedo Codec.float
        |> Codec.field "density" .density Codec.float
        |> Codec.field "greenhouse" .greenhouse Codec.float
        |> Codec.field "meanTemperature" .meanTemperature Codec.float
        |> Codec.field "nativeSophont" .nativeSophont Codec.bool
        |> Codec.field "extinctSophont" .extinctSophont Codec.bool
        |> Codec.field "habitableRating" .habitableRating Codec.int
        |> Codec.field "orbitSequence" .orbitSequence Codec.string
        |> Codec.field "uwp" .uwp Codec.string
        |> Codec.field "diameter" .diameter Codec.float
        |> Codec.field "gravity" .gravity Codec.float
        |> Codec.field "mass" .mass Codec.float
        |> Codec.field "escapeVelocity" .escapeVelocity Codec.float
        |> Codec.field "safeJumpTime" .safeJumpTime Codec.string
        |> Codec.buildObject


codecPlanetoidData : Codec PlanetoidData
codecPlanetoidData =
    Codec.object
        PlanetoidData
        |> Codec.field "orbitPosition" .orbitPosition codecStellarPoint
        |> Codec.field "inclination" .inclination Codec.float
        |> Codec.field "eccentricity" .eccentricity Codec.float
        |> Codec.field "effectiveHZCODeviation" .effectiveHZCODeviation Codec.float
        |> Codec.field "size" .size Codec.string
        |> Codec.field "orbit" .orbit Codec.float
        |> Codec.field "period" .period Codec.float
        |> Codec.field "composition" .composition Codec.string
        |> Codec.field "retrograde" .retrograde Codec.bool
        |> Codec.field "trojanOffset" .trojanOffset (Codec.nullable Codec.float)
        |> Codec.field "axialTilt" .axialTilt Codec.float
        |> Codec.field "moons" .moons (Codec.list (Codec.lazy (\_ -> codecStellarMoon)))
        |> Codec.field "biomassRating" .biomassRating Codec.int
        |> Codec.field "biocomplexityCode" .biocomplexityCode Codec.int
        |> Codec.field "biodiversityRating" .biodiversityRating Codec.int
        |> Codec.field "compatibilityRating" .compatibilityRating Codec.int
        |> Codec.field "resourceRating" .resourceRating Codec.int
        |> Codec.field "currentNativeSophont" .currentNativeSophont Codec.bool
        |> Codec.field "extinctNativeSophont" .extinctNativeSophont Codec.bool
        |> Codec.field "hasRing" .hasRing Codec.bool
        |> Codec.field "albedo" .albedo Codec.float
        |> Codec.field "density" .density Codec.float
        |> Codec.field "greenhouse" .greenhouse Codec.float
        |> Codec.field "meanTemperature" .meanTemperature Codec.float
        |> Codec.field "orbitSequence" .orbitSequence Codec.string
        |> Codec.field "uwp" .uwp Codec.string
        |> Codec.field "diameter" .diameter Codec.float
        |> Codec.field "gravity" .gravity (Codec.nullable Codec.float)
        |> Codec.field "mass" .mass (Codec.nullable Codec.float)
        |> Codec.field "escapeVelocity" .escapeVelocity (Codec.nullable Codec.float)
        |> Codec.field "safeJumpTime" .safeJumpTime Codec.string
        |> Codec.buildObject


codecStarData : Codec StarData
codecStarData =
    Codec.object
        StarDataConfig
        |> Codec.field "orbitPosition" .orbitPosition codecStellarPoint
        |> Codec.field "inclination" .inclination Codec.int
        |> Codec.field "eccentricity" .eccentricity Codec.float
        |> Codec.field "effectiveHZCODeviation" .effectiveHZCODeviation Codec.float
        |> Codec.field "stellarClass" .stellarClass Codec.string
        |> Codec.field "stellarType" .stellarType Codec.string
        |> Codec.field "subtype" .subtype (Codec.nullable Codec.int)
        |> Codec.field "orbitType" .orbitType Codec.int
        |> Codec.field "mass" .mass Codec.float
        |> Codec.field "diameter" .diameter Codec.float
        |> Codec.field "temperature" .temperature Codec.int
        |> Codec.field "age" .age Codec.float
        |> Codec.field "colour" .colour codecStarColour
        |> Codec.nullableField "companion" .companion (Codec.lazy (\_ -> codecStarData))
        |> Codec.field "orbit" .orbit Codec.float
        |> Codec.field "period" .period Codec.float
        |> Codec.field "baseline" .baseline Codec.int
        |> Codec.field "stellarObjects" .stellarObjects (Codec.list codecStellarObject)
        |> Codec.field "orbitSequence" .orbitSequence Codec.string
        |> Codec.field "jump" .jump Codec.float
        |> Codec.buildObject
        |> Codec.map StarData (\(StarData data) -> data)
