module Traveller.Hydrographics exposing (StellarHydrographics, codec, surfaceDistributionDescription)

import Codec exposing (Codec)


type alias StellarHydrographics =
    { code : Int, distribution : Int }


codec : Codec StellarHydrographics
codec =
    Codec.object StellarHydrographics
        |> Codec.field "code" .code Codec.int
        |> Codec.field "distribution" .distribution Codec.int
        |> Codec.buildObject


surfaceDistributionDescription : Int -> String
surfaceDistributionDescription code =
    case code of
        0 ->
            "Extremely Dispersed (Many minor and small bodies: no major bodies)"

        1 ->
            "Very Dispersed (Mostly minor bodies: 5–10% of the surface coverage is in major bodies)"

        2 ->
            "Dispersed (Mostly minor bodies: 10–20% of the surface coverage is in major bodies)"

        3 ->
            "Scattered (Roughly 20–30% or less of the surface coverage is in major bodies)"

        4 ->
            "Slightly Scattered (Roughly 30–40% or less of the surface coverage is in major bodies)"

        5 ->
            "Mixed (Mix of major and minor bodies: roughly 40–60% of body coverage is major)"

        6 ->
            "Slightly Skewed (Roughly 60–70% of surface coverage is in major bodies)"

        7 ->
            "Skewed (Roughly 70–80% of surface coverage is in major bodies)"

        8 ->
            "Concentrated (Mostly major bodies: 80–90% of the surface coverage is in major bodies)"

        9 ->
            "Very Concentrated (Single very large major body: 90–95% of body coverage is in one body)"

        10 ->
            "Extremely Concentrated (Single very large major body: 95% or more of body coverage is in one body)"

        _ ->
            "Unknown surface distribution code"
