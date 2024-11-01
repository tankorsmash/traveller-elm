module Traveller.StellarMoon exposing (Moon(..), codecMoon, sampleStellarMoon)

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
type alias MoonData =
    { orbitPosition : StellarPoint
    , inclination : Float
    , eccentricity : Float
    , effectiveHZCODeviation : Float
    , orbit : StellarOrbit
    , size : Maybe String
    , period : Maybe Float
    , biomassRating : Int
    , axialTilt : Float
    , safeJumpDistance : String
    }


type Moon
    = -- needs to be a type instead of an alias, because its recursive
      Moon MoonData


buildMoon =
    \orbPos incl ecc effHZCODev orbit size period biomass axialTilt sjd ->
        { orbitPosition = orbPos
        , inclination = incl
        , eccentricity = ecc
        , effectiveHZCODeviation = effHZCODev
        , orbit = orbit
        , size = size
        , period = period
        , biomassRating = biomass
        , axialTilt = axialTilt
        , safeJumpDistance = sjd
        }


codecMoon : Codec Moon
codecMoon =
    Codec.object
        buildMoon
        |> Codec.field "orbitPosition" .orbitPosition codecStellarPoint
        |> Codec.field "inclination" .inclination Codec.float
        |> Codec.field "eccentricity" .eccentricity Codec.float
        |> Codec.field "effectiveHZCODeviation" .effectiveHZCODeviation Codec.float
        |> Codec.field "orbit" .orbit codecStellarOrbit
        |> Codec.nullableField "size" .size Codec.string
        |> Codec.nullableField "period" .period Codec.float
        |> Codec.maybeField "biomassRating" .biomassRating Codec.int
        |> Codec.maybeField "axialTilt" .axialTilt Codec.float
        |> Codec.maybeField "safeJumpDistance" .safeJumpDistance Codec.string
        |> Codec.buildObject
        |> -- Codec.map needs a way to go from object, and a way to go back to object
           Codec.map Moon (\(Moon data) -> data)
