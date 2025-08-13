module Traveller.StellarTaint exposing (StellarTaint, codecStellarTaint, taintSubtypeDescription)

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


taintSubtypeDescription : String -> String
taintSubtypeDescription code =
    case String.toUpper (String.trim code) of
        "L" ->
            "Low Oxygen"

        "R" ->
            "Radioactivity"

        "B" ->
            "Biologic"

        "G" ->
            "Gas Mix"

        "P" ->
            "Particulates"

        "S" ->
            "Sulphur Compounds"

        "H" ->
            "High Oxygen"

        _ ->
            "N/A"
