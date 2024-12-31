module Traveller.HexAddress exposing (AfterChange, Delta, HexAddress, addVal, between, create, createFromSolarSystem, createFromStarSystem, hexLabel, shiftAddressBy, toKey)


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


{-| Returns a list of hex addresses between two hex addresses, inclusive.
-}
between : { maxX : Int, maxY : Int } -> HexAddress -> HexAddress -> List HexAddress
between hexRules firstAddr secondAddr =
    let
        ( numCols, numRows ) =
            ( hexRules.maxX, hexRules.maxY )

        {- Returns the universal x coordinate of a hex address, so it can be compared to all others across sectors. -}
        universalHexX hexAddr =
            hexAddr.x + hexAddr.sectorX * numCols

        {- Returns the universal y coordinate of a hex address, so it can be compared to all others across sectors. -}
        universalHexY hexAddr =
            hexAddr.y + hexAddr.sectorY * numRows

        ( minX, minY ) =
            ( if universalHexX firstAddr < universalHexX secondAddr then
                firstAddr.x

              else
                secondAddr.x
            , if universalHexY firstAddr < universalHexY secondAddr then
                firstAddr.y

              else
                secondAddr.y
            )

        ( maxX, maxY ) =
            ( if universalHexX firstAddr > universalHexX secondAddr then
                firstAddr.x

              else
                secondAddr.x
            , if universalHexY firstAddr > universalHexY secondAddr then
                firstAddr.y

              else
                secondAddr.y
            )

        ( minSectorX, minSectorY ) =
            ( min firstAddr.sectorX secondAddr.sectorX
            , min firstAddr.sectorY secondAddr.sectorY
            )

        ( maxSectorX, maxSectorY ) =
            ( max firstAddr.sectorX secondAddr.sectorX
            , max firstAddr.sectorY secondAddr.sectorY
            )

        hexesInSector sectorX sectorY =
            let
                startX =
                    if sectorX == minSectorX then
                        minX

                    else
                        1

                endX =
                    if sectorX == maxSectorX then
                        maxX

                    else
                        numCols

                startY =
                    if sectorY == minSectorY then
                        minY

                    else
                        1

                endY =
                    if sectorY == maxSectorY then
                        maxY

                    else
                        numRows
            in
            List.range startY endY
                |> List.map
                    (\y ->
                        List.range startX endX
                            |> List.map
                                (\x ->
                                    { sectorX = sectorX
                                    , sectorY = sectorY
                                    , x = x
                                    , y = y
                                    }
                                )
                    )
    in
    -- for every sector X in ranges and for every sector Y in range,
    -- create a hex address for every sector
    List.range minSectorX maxSectorX
        |> List.concatMap
            (\sectorX ->
                List.range minSectorY maxSectorY
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
