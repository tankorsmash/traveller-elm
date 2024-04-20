module Traveller.Point exposing (StellarPoint, codecStellarPoint)

import Codec exposing (Codec)
import Json.Decode as JsDecode
import Json.Encode as JsEncode
import Parser exposing ((|.), (|=), Parser)
import Parser.Extras as Parser


type alias StellarPoint =
    { x : Float, y : Float }


codecStellarPoint : Codec StellarPoint
codecStellarPoint =
    Codec.object StellarPoint
        |> Codec.field "x" .x Codec.float
        |> Codec.field "y" .y Codec.float
        |> Codec.buildObject
