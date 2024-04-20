module Traveller.Hydrographics exposing (StellarHydrographics, codecStellarHydrographics)

import Codec exposing (Codec)
import Json.Decode as JsDecode
import Json.Encode as JsEncode
import Parser exposing ((|.), (|=), Parser)
import Parser.Extras as Parser


type alias StellarHydrographics =
    { code : Int, distribution : Int }


codecStellarHydrographics : Codec StellarHydrographics
codecStellarHydrographics =
    Codec.object StellarHydrographics
        |> Codec.field "code" .code Codec.int
        |> Codec.field "distribution" .distribution Codec.int
        |> Codec.buildObject
