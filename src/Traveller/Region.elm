module Traveller.Region exposing (Region, RegionDict, codec)

import Codec exposing (Codec)
import Color exposing (Color)
import Color.Convert exposing (colorToHex, hexToColor)
import Dict
import Traveller.HexAddress as HexAddress exposing (HexAddress)


type alias Region =
    { id : Int
    , colour : Color
    , name : String
    , labelPosition : HexAddress
    , parsecs : List HexAddress
    }


type alias RegionDict =
    Dict.Dict Int Region


codecColour : Codec Color
codecColour =
    Codec.string
        |> Codec.andThen
            (\s ->
                case hexToColor s of
                    Ok color ->
                        Codec.succeed color

                    Err errString ->
                        Codec.fail errString
            )
            (\c -> colorToHex c)


codec : Codec Region
codec =
    Codec.object Region
        |> Codec.field "id" .id Codec.int
        |> Codec.field "colour" .colour codecColour
        |> Codec.field "name" .name Codec.string
        |> Codec.field "label_position" .labelPosition HexAddress.codec
        |> Codec.field "parsecs" .parsecs (Codec.list HexAddress.codec)
        |> Codec.buildObject
