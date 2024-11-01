module Traveller.StellarObject exposing (StellarObject(..), codecStellarObject, decodeStellarObjectX, sampleStellarObject)

import Codec exposing (Codec)
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Traveller.Atmosphere exposing (StellarAtmosphere, codecStellarAtmosphere)
import Traveller.Hydrographics exposing (StellarHydrographics, codecStellarHydrographics)
import Traveller.Orbit exposing (StellarOrbit, codecStellarOrbit)
import Traveller.Point exposing (StellarPoint, codecStellarPoint)
import Traveller.Population exposing (StellarPopulation, codecStellarPopulation)
import Traveller.StellarMoon exposing (MoonData, decodeMoonData)


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
    , moons : Maybe (List MoonData)
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



--codecStellarObject : Decode.Decoder StellarObject
--codecStellarObject =
--    Decode.succeed StellarObject
--        |> Codec.field "orbitPosition" .orbitPosition codecStellarPoint
--        |> Codec.field "inclination" .inclination Codec.float
--        |> Codec.field "eccentricity" .eccentricity Codec.float
--        |> Codec.field "effectiveHZCODeviation" .effectiveHZCODeviation (Codec.nullable Codec.float)
--        |> Codec.maybeField "size" .size Codec.string
--        |> Codec.field "orbit" .orbit codecStellarOrbit
--        |> Codec.nullableField "period" .period Codec.float
--        |> Codec.maybeField "composition" .composition Codec.string
--        |> Codec.maybeField "retrograde" .retrograde Codec.bool
--        |> Codec.maybeField "trojanOffset" .trojanOffset Codec.float
--        |> Codec.maybeField "axialTilt" .axialTilt Codec.float
--        |> Codec.maybeField "moons" .moons (Decode.list decodeMoonData)
--        |> Codec.maybeField "biomassRating" .biomassRating Codec.int
--        |> Codec.maybeField "biocomplexityCode" .biocomplexityCode Codec.int
--        |> Codec.maybeField "biodiversityRating" .biodiversityRating Codec.int
--        |> Codec.maybeField "compatibilityRating" .compatibilityRating Codec.int
--        |> Codec.maybeField "resourceRating" .resourceRating Codec.int
--        |> Codec.maybeField "currentNativeSophont" .currentNativeSophont Codec.bool
--        |> Codec.maybeField "extinctNativeSophont" .extinctNativeSophont Codec.bool
--        |> Codec.maybeField "hasRing" .hasRing Codec.bool
--        |> Codec.field "orbitType" .orbitType Codec.int
--        |> Codec.maybeField "atmosphere" .atmosphere codecStellarAtmosphere
--        |> Codec.maybeField "hydrographics" .hydrographics codecStellarHydrographics
--        |> Codec.maybeField "population" .population codecStellarPopulation
--        |> Codec.maybeField "governmentCode" .governmentCode Codec.int
--        |> Codec.maybeField "lawLevelCode" .lawLevelCode Codec.int
--        |> Codec.maybeField "starPort" .starPort Codec.string
--        |> Codec.maybeField "techLevel" .techLevel Codec.int
--        |> Codec.maybeField "tradeCodes" .tradeCodes (Codec.list Codec.string)
--        |> Codec.maybeField "albedo" .albedo Codec.float
--        |> Codec.maybeField "density" .density Codec.float
--        |> Codec.maybeField "greenhouse" .greenhouse Codec.int
--        |> Codec.maybeField "meanTemperature" .meanTemperature Codec.float
--        |> Codec.field "orbitSequence" .orbitSequence Codec.string
--        |> Codec.maybeField "uwp" .uwp Codec.string
--        |> Codec.maybeField "code" .code Codec.string
--        |> Codec.optionalField "jumpShadow" .jumpShadow Codec.float
--        |> Codec.field "safeJumpTime" .safeJumpTime Codec.string
--        |> Codec.buildObject
--        |> -- Codec.map needs a way to go from object, and a way to go back to object
--           Codec.map StellarObject (\(StellarObject data) -> data)
--


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
    , moons : List MoonData
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
    , moons : List MoonData
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
    , moons : List MoonData
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
    , stellarObjects : List StellarObjectX
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


decodePlanetoidBeltData : Decode.Decoder PlanetoidBeltData
decodePlanetoidBeltData =
    Decode.succeed PlanetoidBeltData
        |> Decode.required "orbitPosition" (Codec.decoder codecStellarPoint)
        |> Decode.required "inclination" Decode.float
        |> Decode.required "eccentricity" Decode.float
        |> Decode.required "effectiveHZCODeviation" Decode.float
        |> Decode.required "orbit" Decode.float
        |> Decode.required "mType" Decode.float
        |> Decode.required "sType" Decode.float
        |> Decode.required "cType" Decode.float
        |> Decode.required "oType" Decode.float
        |> Decode.required "span" Decode.float
        |> Decode.required "bulk" Decode.float
        |> Decode.required "resourceRating" Decode.float
        |> Decode.required "period" Decode.float
        |> Decode.required "orbitSequence" Decode.string
        |> Decode.required "uwp" Decode.string
        |> Decode.required "safeJumpTime" Decode.string


decodeGasGiantData : Decode.Decoder GasGiantData
decodeGasGiantData =
    Decode.succeed GasGiantData
        |> Decode.required "orbitPosition" (Codec.decoder codecStellarPoint)
        |> Decode.required "inclination" Decode.float
        |> Decode.required "eccentricity" Decode.float
        |> Decode.required "effectiveHZCODeviation" (Decode.nullable Decode.float)
        |> Decode.required "code" Decode.string
        |> Decode.required "diameter" Decode.float
        |> Decode.required "mass" Decode.float
        |> Decode.required "orbit" Decode.float
        |> Decode.required "moons" (Decode.list decodeMoonData)
        |> Decode.required "hasRing" Decode.bool
        |> Decode.required "trojanOffset" (Decode.nullable Decode.float)
        |> Decode.required "axialTilt" Decode.float
        |> Decode.required "period" Decode.float
        |> Decode.required "orbitSequence" Decode.string
        |> Decode.required "safeJumpTime" Decode.string



--codecGasGiantData : Codec GasGiantData
--codecGasGiantData =
--    Codec.object
--        GasGiantData
--        |> Codec.field "orbitPosition" .orbitPosition codecStellarPoint
--        |> Codec.field "inclination" .inclination Codec.float
--        |> Codec.field "eccentricity" .eccentricity Codec.float
--        |> Codec.field "effectiveHZCODeviation" .effectiveHZCODeviation (Codec.nullable Codec.float)
--        |> Codec.field "code" .code Codec.string
--        |> Codec.field "diameter" .diameter Codec.float
--        |> Codec.field "mass" .mass Codec.float
--        |> Codec.field "orbit" .orbit Codec.float
--        |> Codec.field "moons" .moons (Codec.list decodeMoonData)
--        |> Codec.field "hasRing" .hasRing Codec.bool
--        |> Codec.field "trojanOffset" .trojanOffset (Codec.nullable Codec.float)
--        |> Codec.field "axialTilt" .orbit Codec.float
--        |> Codec.field "period" .orbit Codec.float
--        |> Codec.field "orbitSequence" .orbitSequence Codec.string
--        |> Codec.field "safeJumpTime" .safeJumpTime Codec.string
--        |> Codec.buildObject


decodeTerrestrialData : Decode.Decoder TerrestrialData
decodeTerrestrialData =
    Decode.succeed TerrestrialData
        |> Decode.required "orbitPosition" (Codec.decoder codecStellarPoint)
        |> Decode.required "inclination" Decode.float
        |> Decode.required "eccentricity" Decode.float
        |> Decode.required "effectiveHZCODeviation" Decode.float
        |> Decode.required "size" Decode.int
        |> Decode.required "orbit" Decode.float
        |> Decode.required "period" Decode.float
        |> Decode.required "composition" Decode.string
        |> Decode.required "retrograde" Decode.bool
        |> Decode.required "trojanOffset" (Decode.nullable Decode.float)
        |> Decode.required "axialTilt" Decode.float
        |> Decode.required "moons" (Decode.list decodeMoonData)
        |> Decode.required "biomassRating" Decode.int
        |> Decode.required "biocomplexityCode" Decode.int
        |> Decode.required "biodiversityRating" Decode.int
        |> Decode.required "compatibilityRating" Decode.int
        |> Decode.required "resourceRating" Decode.int
        |> Decode.required "currentNativeSophont" Decode.bool
        |> Decode.required "extinctNativeSophont" Decode.bool
        |> Decode.required "hasRing" Decode.bool
        |> Decode.required "albedo" Decode.float
        |> Decode.required "density" Decode.float
        |> Decode.required "greenhouse" Decode.float
        |> Decode.required "meanTemperature" Decode.float
        |> Decode.required "nativeSophont" Decode.bool
        |> Decode.required "extinctSophont" Decode.bool
        |> Decode.required "habitableRating" Decode.int
        |> Decode.required "orbitSequence" Decode.string
        |> Decode.required "uwp" Decode.string
        |> Decode.required "diameter" Decode.float
        |> Decode.required "gravity" Decode.float
        |> Decode.required "mass" Decode.float
        |> Decode.required "escapeVelocity" Decode.float
        |> Decode.required "safeJumpTime" Decode.string


codecPlanetoidData : Decode.Decoder PlanetoidData
codecPlanetoidData =
    Decode.succeed
        PlanetoidData
        |> Decode.required "orbitPosition" (Codec.decoder codecStellarPoint)
        |> Decode.required "inclination" Decode.float
        |> Decode.required "eccentricity" Decode.float
        |> Decode.required "effectiveHZCODeviation" Decode.float
        |> Decode.required "size" Decode.string
        |> Decode.required "orbit" Decode.float
        |> Decode.required "period" Decode.float
        |> Decode.required "composition" Decode.string
        |> Decode.required "retrograde" Decode.bool
        |> Decode.required "trojanOffset" (Decode.nullable Decode.float)
        |> Decode.required "axialTilt" Decode.float
        |> Decode.required "moons" (Decode.list decodeMoonData)
        |> Decode.required "biomassRating" Decode.int
        |> Decode.required "biocomplexityCode" Decode.int
        |> Decode.required "biodiversityRating" Decode.int
        |> Decode.required "compatibilityRating" Decode.int
        |> Decode.required "resourceRating" Decode.int
        |> Decode.required "currentNativeSophont" Decode.bool
        |> Decode.required "extinctNativeSophont" Decode.bool
        |> Decode.required "hasRing" Decode.bool
        |> Decode.required "albedo" Decode.float
        |> Decode.required "density" Decode.float
        |> Decode.required "greenhouse" Decode.float
        |> Decode.required "meanTemperature" Decode.float
        |> Decode.required "orbitSequence" Decode.string
        |> Decode.required "uwp" Decode.string
        |> Decode.required "diameter" Decode.float
        |> Decode.required "gravity" (Decode.nullable Decode.float)
        |> Decode.required "mass" (Decode.nullable Decode.float)
        |> Decode.required "escapeVelocity" (Decode.nullable Decode.float)
        |> Decode.required "safeJumpTime" Decode.string


decodeStellarObjectX : Decode.Decoder StellarObjectX
decodeStellarObjectX =
    Decode.oneOf
        [ Decode.map GasGiant <| decodeGasGiantData
        , Decode.map TerrestrialPlanet <| decodeTerrestrialData
        , Decode.map PlanetoidBelt <| decodePlanetoidBeltData
        , Decode.map Planetoid <| codecPlanetoidData
        , Decode.map Star <| (Decode.map StarData <| codecStarData)
        ]


codecStarData : Decode.Decoder StarDataConfig
codecStarData =
    Decode.succeed StarDataConfig
        |> Decode.required "orbitPosition" (Codec.decoder codecStellarPoint)
        |> Decode.required "inclination" Decode.int
        |> Decode.required "eccentricity" Decode.float
        |> Decode.required "effectiveHZCODeviation" Decode.float
        |> Decode.required "stellarClass" Decode.string
        |> Decode.required "stellarType" Decode.string
        |> Decode.required "subtype" (Decode.nullable Decode.int)
        |> Decode.required "orbitType" Decode.int
        |> Decode.required "mass" Decode.float
        |> Decode.required "diameter" Decode.float
        |> Decode.required "temperature" Decode.int
        |> Decode.required "age" Decode.float
        |> Decode.required "colour" (Codec.decoder codecStarColour)
        |> Decode.optional "companion"
            (Decode.maybe
                (Decode.lazy
                    (\_ -> Decode.map StarData <| codecStarData)
                )
            )
            Nothing
        |> Decode.required "orbit" Decode.float
        |> Decode.required "period" Decode.float
        |> Decode.required "baseline" Decode.int
        |> Decode.required "stellarObjects"
            (Decode.list
                (Decode.lazy
                    (\_ -> Decode.map StellarObjectx <| decodeStellarObjectX)
                )
            )
        |> Decode.required "orbitSequence" Decode.string
        |> Decode.required "jump" Decode.float
