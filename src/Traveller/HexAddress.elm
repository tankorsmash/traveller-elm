module Traveller.HexAddress exposing (AfterChange, Delta, HexAddress, addVal, between, create, createFromSolarSystem, createFromStarSystem, hexLabel, shiftAddressBy, toKey, toSectorKey, toString, universalHexX, universalHexY)


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


createFromStarSystem : { a | sectorX : Int, sectorY : Int, x : Int, y : Int } -> HexAddress
createFromStarSystem { sectorX, sectorY, x, y } =
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


toString : HexAddress -> String
toString { sectorX, sectorY, x, y } =
    "Sector: "
        ++ String.fromInt sectorX
        ++ ", "
        ++ String.fromInt sectorY
        ++ " Hex: "
        ++ String.fromInt x
        ++ ", "
        ++ String.fromInt y


{-| Returns the universal x coordinate of a hex address, so it can be compared to all others across sectors.
-}
universalHexX hexAddr =
    hexAddr.x + hexAddr.sectorX * 32


{-| Returns the universal y coordinate of a hex address, so it can be compared to all others across sectors.
-}
universalHexY hexAddr =
    hexAddr.sectorY * 40 - hexAddr.y


toSectorKey : HexAddress -> String
toSectorKey { sectorX, sectorY } =
    String.fromInt sectorX
        ++ "."
        ++ String.fromInt sectorY


{-| Returns a list of hex addresses between two hex addresses, inclusive.
-}
between : { maxX : Int, maxY : Int } -> HexAddress -> HexAddress -> List HexAddress
between hexRules firstAddr secondAddr =
    let
        ( numCols, numRows ) =
            ( hexRules.maxX, hexRules.maxY )

        ( minX, minY ) =
            ( firstAddr.x, firstAddr.y )

        ( maxX, maxY ) =
            ( secondAddr.x, secondAddr.y )

        ( minSectorX, minSectorY ) =
            ( firstAddr.sectorX, firstAddr.sectorY )

        ( maxSectorX, maxSectorY ) =
            ( secondAddr.sectorX, secondAddr.sectorY )

        hexesInSector sectorX sectorY =
            let
                startHexX =
                    if sectorX == minSectorX then
                        minX

                    else
                        1

                endHexX =
                    if sectorX == maxSectorX then
                        maxX

                    else
                        numCols

                startHexY =
                    if sectorY == minSectorY then
                        minY

                    else
                        1

                endHexY =
                    if sectorY == maxSectorY then
                        maxY

                    else
                        numRows
            in
            List.range startHexY endHexY
                |> List.map
                    (\y ->
                        List.range startHexX endHexX
                            |> List.map (\x -> { sectorX = sectorX, sectorY = sectorY, x = x, y = y })
                    )
    in
    -- for every sector X in ranges and for every sector Y in range,
    -- create a hex address for every sector
    List.range minSectorX maxSectorX
        |> List.concatMap
            (\sectorX ->
                List.range maxSectorY minSectorY
                    |> List.reverse
                    |> List.concatMap
                        (\sectorY ->
                            hexesInSector sectorX sectorY
                        )
            )
        |> List.concat


hexLabel : HexAddress -> String
hexLabel { x, y } =
    (String.fromInt x |> String.pad 2 '0')
        ++ (String.fromInt y |> String.pad 2 '0')
