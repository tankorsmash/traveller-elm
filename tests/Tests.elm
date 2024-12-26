module Tests exposing (..)

import Expect
import Fuzz
import Test exposing (Test, describe)
import Traveller
import Traveller.HexAddress as HexAddress exposing (HexAddress)


suite : Test
suite =
    let
        hexAddressOne =
            HexAddress.create
                { sectorX = 1
                , sectorY = 1
                , x = 1
                , y = 1
                }
    in
    describe "HexAddress"
        [ Test.test "adding 0 does nothing" <|
            \() ->
                let
                    sourceHexAddress =
                        hexAddressOne

                    targetHexAddress =
                        hexAddressOne

                    res =
                        HexAddress.shiftAddressBy
                            { deltaX = 0, deltaY = 0 }
                            { maxX = Traveller.numHexCols, maxY = Traveller.numHexRows }
                            sourceHexAddress
                in
                Expect.equal res targetHexAddress
        , Test.test "adding 1 to each works" <|
            \() ->
                let
                    sourceHexAddress =
                        hexAddressOne

                    targetHexAddress =
                        HexAddress.create
                            { hexAddressOne
                                | x = 2
                                , y = 2
                            }

                    res =
                        HexAddress.shiftAddressBy
                            { deltaX = 1, deltaY = 1 }
                            { maxX = Traveller.numHexCols, maxY = Traveller.numHexRows }
                            sourceHexAddress
                in
                Expect.equal res targetHexAddress
        , Test.fuzz2
            (Fuzz.intRange 1 (Traveller.numHexCols - 1))
            (Fuzz.intRange 1 (Traveller.numHexRows - 1))
            "adding within the same sector doesnt increment the sector"
          <|
            \deltaX deltaY ->
                let
                    sourceHexAddress =
                        hexAddressOne

                    targetHexAddress =
                        HexAddress.create
                            { hexAddressOne
                                | x = hexAddressOne.x + deltaX
                                , y = hexAddressOne.y + deltaY
                            }

                    res =
                        HexAddress.shiftAddressBy
                            { deltaX = deltaX, deltaY = deltaY }
                            { maxX = Traveller.numHexCols, maxY = Traveller.numHexRows }
                            sourceHexAddress
                in
                Expect.equal res targetHexAddress
        , Test.fuzz2
            (Fuzz.intRange -10000 10000)
            (Fuzz.intRange -10000 10000)
            "shifting an address never returns a hex with 0 in either hex coordinates"
          <|
            \deltaX deltaY ->
                let
                    sourceHexAddress =
                        hexAddressOne

                    res =
                        HexAddress.shiftAddressBy
                            { deltaX = deltaX, deltaY = deltaY }
                            { maxX = Traveller.numHexCols, maxY = Traveller.numHexRows }
                            sourceHexAddress
                in
                { x = res.x, y = res.y }
                    |> Expect.all
                        [ .x >> Expect.notEqual 0
                        , .y >> Expect.notEqual 0
                        ]
        , Test.test "`between` with same everything returns nothing" <|
            \() ->
                let
                    sourceHexAddress =
                        hexAddressOne

                    targetHexAddress =
                        hexAddressOne

                    hexesBetween =
                        HexAddress.between sourceHexAddress targetHexAddress
                in
                Expect.equalLists hexesBetween []
        ]
