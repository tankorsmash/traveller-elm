module Traveller.Moon exposing (Moon, codec)

import Codec exposing (Codec)
import Traveller.Orbit as Orbit exposing (StellarOrbit)
import Traveller.Point as Point exposing (StellarPoint)


{-| the data structure for a stellar object.
It needs to be separate from StellarObject so that it can nest within itself
-}
type alias Moon =
    { orbitPosition : StellarPoint
    , inclination : Float
    , eccentricity : Float
    , effectiveHZCODeviation : Float
    , orbit : Maybe StellarOrbit
    , size : String
    , period : Maybe Float
    , biomassRating : Int
    , axialTilt : Float
    , safeJumpDistance : Maybe String
    }


codecMoonSize : Codec String
codecMoonSize =
    Codec.oneOf Codec.string [ Codec.int |> Codec.map String.fromInt (String.toInt >> Maybe.withDefault 0) ]


codec : Codec Moon
codec =
    Codec.object Moon
        |> Codec.field "orbitPosition" .orbitPosition Point.codec
        |> Codec.field "inclination" .inclination Codec.float
        |> Codec.field "eccentricity" .eccentricity Codec.float
        |> Codec.field "effectiveHZCODeviation" .effectiveHZCODeviation Codec.float
        |> Codec.field "orbit" .orbit (Codec.nullable Orbit.codec)
        |> Codec.field "size" .size codecMoonSize
        |> Codec.optionalNullableField "period" .period Codec.float
        |> Codec.field "biomassRating" .biomassRating Codec.int
        |> Codec.field "axialTilt" .axialTilt Codec.float
        |> Codec.optionalField "safeJumpDistance" .safeJumpDistance Codec.string
        |> Codec.buildObject
