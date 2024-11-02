module Traveller.Moon exposing (Moon, codec)

import Codec exposing (Codec)
import Traveller.Orbit exposing (StellarOrbit, codecStellarOrbit)
import Traveller.Point exposing (StellarPoint, codecStellarPoint)


{-| the data structure for a stellar object.
It needs to be separate from StellarObject so that it can nest within itself
-}
type alias Moon =
    { orbitPosition : StellarPoint
    , inclination : Float
    , eccentricity : Float
    , effectiveHZCODeviation : Float
    , orbit : StellarOrbit
    , size : String
    , period : Float
    , biomassRating : Int
    , axialTilt : Float
    , safeJumpDistance : String
    }


codec : Codec Moon
codec =
    Codec.object Moon
        |> Codec.field "orbitPosition" .orbitPosition codecStellarPoint
        |> Codec.field "inclination" .inclination Codec.float
        |> Codec.field "eccentricity" .eccentricity Codec.float
        |> Codec.field "effectiveHZCODeviation" .effectiveHZCODeviation Codec.float
        |> Codec.field "orbit" .orbit codecStellarOrbit
        |> Codec.field "size" .size Codec.string
        |> Codec.field "period" .period Codec.float
        |> Codec.field "biomassRating" .biomassRating Codec.int
        |> Codec.field "axialTilt" .axialTilt Codec.float
        |> Codec.field "safeJumpDistance" .safeJumpDistance Codec.string
        |> Codec.buildObject
