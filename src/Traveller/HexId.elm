module Traveller.HexId exposing (HexId, RawHexId, codecHexId, createFromInt, hexId, createFromTwoInts)

import Codec exposing (Codec)
import Json.Decode as JsDecode
import Json.Encode as JsEncode
import Parser exposing ((|.), (|=), Parser)
import Parser.Extras as Parser


{-| needs to be a plain int for hashing in a Dict, otherwise we'd use full HexIds
-}
type alias RawHexId =
    Int


type alias HexId =
    { value : Int, raw : String }


{-| Assumes the int passed in is valid for a hexId
ie 100 would be turned into HexId 0100 -}
createFromInt : Int -> HexId
createFromInt value =
    { value = value, raw = String.fromInt value }

createFromTwoInts : Int -> Int -> HexId
createFromTwoInts left right =
    { value = left * 100 + right, raw = ""}


hexId : Parser HexId
hexId =
    Parser.andThen
        (\rawString ->
            case String.toInt rawString of
                Just validValue ->
                    Parser.succeed { raw = rawString, value = validValue }

                Nothing ->
                    Parser.problem "Not a valid hexId"
        )
    <|
        (Parser.getChompedString <|
            Parser.succeed ()
                |. Parser.chompWhile
                    Char.isDigit
        )


codecHexId : Codec.Codec HexId
codecHexId =
    Codec.string
        |> Codec.map
            (\rawString ->
                { raw = rawString
                , value = String.toInt rawString |> Maybe.withDefault -99999
                }
            )
            .raw
