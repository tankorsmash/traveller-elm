module Traveller.HexId exposing (HexId, RawHexId, codecHexId, createFromInt, hexId, toXY, toRowCol)

import Codec exposing (Codec)
import Json.Decode as JsDecode
import Json.Encode as JsEncode
import Parser exposing ((|.), (|=), Parser)
import Parser.Extras as Parser


toRowCol : HexId -> ( Int, Int )
toRowCol hexId_ =
    let
        row =
            modBy 100 hexId_.value

        col =
            hexId_.value // 100
    in
    ( row, col )


toXY : HexId -> { x : Int, y : Int }
toXY hexId_ =
    let
        ( row, col ) =
            toRowCol hexId_
    in
    { x = col, y = row }


{-| needs to be a plain int for hashing in a Dict, otherwise we'd use full HexIds
-}
type alias RawHexId =
    Int


type alias HexId =
    { value : Int, raw : String }


createFromInt : Int -> HexId
createFromInt value =
    { value = value, raw = String.fromInt value }


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
