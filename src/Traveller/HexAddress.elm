module Traveller.HexAddress exposing (AfterChange, Delta, HexAddress, addVal, create, createFromSolarSystem, hexLabel, shiftAddressBy, toKey)


type alias HexAddress =
    { sectorX : Int
    , sectorY : Int
    , x : Int
    , y : Int
    }


type alias Delta =
    { hexVal : Int, delta : Int, max : Int }


type alias AfterChange =
    { hexVal : Int, sectorDelta : Int }


addVal : Delta -> AfterChange
addVal { hexVal, delta, max } =
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


shiftAddressBy :
    { deltaX : Int, deltaY : Int }
    -> { maxX : Int, maxY : Int }
    -> HexAddress
    -> HexAddress
shiftAddressBy { deltaX, deltaY } { maxX, maxY } hexAddress =
    let
        newXOffset =
            addVal { hexVal = hexAddress.x, max = maxX, delta = deltaX }

        newYOffset =
            addVal { hexVal = hexAddress.y, max = maxY, delta = deltaY }
    in
    { sectorX = hexAddress.sectorX + newXOffset.sectorDelta
    , sectorY = hexAddress.sectorY - newYOffset.sectorDelta
    , x = newXOffset.hexVal
    , y = newYOffset.hexVal
    }


createFromSolarSystem : { a | sectorX : Int, sectorY : Int, x : Int, y : Int } -> HexAddress
createFromSolarSystem { sectorX, sectorY, x, y } =
    create { sectorX = sectorX, sectorY = sectorY, x = x, y = y }


create : { sectorX : Int, sectorY : Int, x : Int, y : Int } -> HexAddress
create { sectorX, sectorY, x, y } =
    { sectorX = sectorX
    , sectorY = sectorY
    , x = x
    , y = y
    }


toKey : HexAddress -> String
toKey { sectorX, sectorY, x, y } =
    String.fromInt sectorX
        ++ "."
        ++ String.fromInt sectorY
        ++ "."
        ++ String.fromInt x
        ++ "."
        ++ String.fromInt y


hexLabel : HexAddress -> String
hexLabel { x, y } =
    (String.fromInt x |> String.pad 2 '0')
        ++ (String.fromInt y |> String.pad 2 '0')
