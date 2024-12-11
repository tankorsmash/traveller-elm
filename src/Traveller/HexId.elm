module Traveller.HexId exposing (HexId, RawHexId, codecHexId, createFromInt, createFromXY, hexIdParser, hexIdToString, one, ten, toRowCol, toXY)

import Codec exposing (Codec)
import Parser exposing ((|.), (|=), Parser)


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


{-| known-valid HexId for 0101
-}
one : HexId
one =
    { value = 101, raw = "101" }


{-| known-valid HexId for 1010
-}
ten : HexId
ten =
    { value = 1010, raw = "1010" }


createFromXY : { a | x : Int, y : Int } -> Maybe HexId
createFromXY { x, y } =
    createFromInt <| x * 100 + y


createFromInt : Int -> Maybe HexId
createFromInt value =
    -- TODO : 3158 isnt handled by this check
    if value <= 101 || value >= 3248 then
        Nothing

    else
        Just
            { value = value, raw = String.fromInt value }


hexIdParser : Parser HexId
hexIdParser =
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


hexIdToString : HexId -> String
hexIdToString hexId =
    hexId.raw
