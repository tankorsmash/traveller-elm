module Traveller.StellarMoon exposing (StellarMoon(..), codecStellarMoon, sampleStellarMoon)

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


sampleStellarMoon : String
sampleStellarMoon =
    """
{
                  "orbitPosition": {
                    "x": 0,
                    "y": 0
                  },
                  "inclination": 0,
                  "eccentricity": 0.0612665390566092,
                  "effectiveHZCODeviation": 0,
                  "orbit": {
                    "zone": "middle",
                    "orbit": 14.302082608154684
                  },
                  "size": "S",
                  "period": null,
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
                    "bar": 0,
                    "gasType": null,
                    "density": "None",
                    "hazardCode": null
                  },
                  "hydrographics": {
                    "code": 0,
                    "distribution": null
                  },
                  "governmentCode": 0,
                  "population": {
                    "code": 0,
                    "concentrationRating": null
                  },
                  "lawLevelCode": 0,
                  "biomassRating": 0,
                  "axialTilt": 33.06570464524518
                }
                """


{-| the data structure for a stellar object.
It needs to be separate from StellarObject so that it can nest within itself
-}
type alias StellarMoonData =
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
    , biomassRating : Maybe Int
    , biocomplexityCode : Maybe Int
    , biodiversityRating : Maybe Int
    , compatibilityRating : Maybe Int
    , resourceRating : Maybe Int
    , currentNativeSophont : Maybe Bool
    , extinctNativeSophont : Maybe Bool
    , hasRing : Maybe Bool
    -- , orbitType : Int
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

    }


type StellarMoon
    = -- needs to be a type instead of an alias, because its recursive
      StellarMoon StellarMoonData


buildStellarMoon =
    \orbPos incl ecc effHZCODev size orbit period comp retro troj axTilt  biomass bioComplex bioDiversity compat resource currentNative extinctNative hasRing  atm hydro pop gov law starPort tech tradeCodes albedo dens green meanTemp ->
        -- StellarMoonData
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
        , biomassRating = biomass
        , biocomplexityCode = bioComplex
        , biodiversityRating = bioDiversity
        , compatibilityRating = compat
        , resourceRating = resource
        , currentNativeSophont = currentNative
        , extinctNativeSophont = extinctNative
        , hasRing = hasRing
        -- , orbitType = orbitType
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

        }


codecStellarMoon : Codec StellarMoon
codecStellarMoon =
    Codec.object
        buildStellarMoon
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
        |> Codec.maybeField "biomassRating" .biomassRating Codec.int
        |> Codec.maybeField "biocomplexityCode" .biocomplexityCode Codec.int
        |> Codec.maybeField "biodiversityRating" .biodiversityRating Codec.int
        |> Codec.maybeField "compatibilityRating" .compatibilityRating Codec.int
        |> Codec.maybeField "resourceRating" .resourceRating Codec.int
        |> Codec.maybeField "currentNativeSophont" .currentNativeSophont Codec.bool
        |> Codec.maybeField "extinctNativeSophont" .extinctNativeSophont Codec.bool
        |> Codec.maybeField "hasRing" .hasRing Codec.bool
        -- |> Codec.field "orbitType" .orbitType Codec.int
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
        |> Codec.buildObject
        |> -- Codec.map needs a way to go from object, and a way to go back to object
           Codec.map StellarMoon (\(StellarMoon data) -> data)
