module Traveller.StellarTaint exposing (StellarTaint, codecStellarTaint)

import Codec exposing (Codec)
import Json.Decode as JsDecode
import Json.Encode as JsEncode
import Parser exposing ((|.), (|=), Parser)
import Parser.Extras as Parser


type alias StellarTaint =
    { subtype : String, code : String, severity : Int, persistence : Int }


codecStellarTaint : Codec StellarTaint
codecStellarTaint =
    Codec.object StellarTaint
        |> Codec.field "subtype" .subtype Codec.string
        |> Codec.field "code" .code Codec.string
        |> Codec.field "severity" .severity Codec.int
        |> Codec.field "persistence" .persistence Codec.int
        |> Codec.buildObject
