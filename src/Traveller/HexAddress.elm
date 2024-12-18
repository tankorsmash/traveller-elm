module Traveller.HexAddress exposing (HexAddress, add, createFromSolarSystem, hexLabel, toKey)


type alias HexAddress =
    { sectorX : Int
    , sectorY : Int
    , x : Int
    , y : Int
    }


add :
    { hexVal : Int, delta : Int, max : Int }
    -> { hexVal : Int, sectorDelta : Int }
add { hexVal, delta, max } =
    let
        rangeMin : Int
        rangeMin =
            1

        rangeMax =
            max

        rangeSize =
            rangeMax - rangeMin + 1

        total =
            hexVal + delta

        counter =
            ceiling <| (toFloat rangeMin - toFloat total) / toFloat rangeSize

        newHexVal =
            modBy rangeSize (modBy rangeSize (total - rangeMin) + rangeSize) + rangeMin
    in
    { sectorDelta = -1 * counter, hexVal = newHexVal }


createFromSolarSystem : { a | sectorX : Int, sectorY : Int, x : Int, y : Int } -> HexAddress
createFromSolarSystem { sectorX, sectorY, x, y } =
    { sectorX = sectorX
    , sectorY = sectorY
    , x = x
    , y = y
    }


toKey : HexAddress -> String
toKey { sectorX, sectorY, x, y } =
    String.fromInt sectorX ++ "." ++ String.fromInt sectorY ++ "." ++ String.fromInt x ++ "." ++ String.fromInt y


hexLabel : HexAddress -> String
hexLabel { x, y } =
    (String.fromInt y |> String.pad 2 '0') ++ (String.fromInt x |> String.pad 2 '0')
