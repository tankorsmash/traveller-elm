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
    , moons : Maybe (List StellarObject)
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
    }


type StellarObject
    = -- needs to be a type instead of an alias, because its recursive
      StellarObject StellarData


buildStellarObject =
    \orbPos incl ecc effHZCODev size orbit period comp retro troj axTilt moons biomass bioComplex bioDiversity compat resource currentNative extinctNative hasRing orbitType atm hydro pop gov law starPort tech tradeCodes albedo dens green meanTemp orbitSeq ->
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
        |> Codec.maybeField "moons" .moons (Codec.list (Codec.lazy (\_ -> codecStellarObject)))
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
