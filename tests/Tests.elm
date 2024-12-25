module Tests exposing (..)

import Expect
import Fuzz
import Test exposing (Test, describe)
import Traveller
import Traveller.HexAddress as HexAddress exposing (HexAddress)


suite : Test
suite =
    describe "HexAddress"
        [ Test.test "adding 0 does nothing" <|
            \() ->
                let
                    sourceHexAddress =
                        HexAddress.create
                            { sectorX = 1
                            , sectorY = 1
                            , x = 1
                            , y = 1
                            }

                    targetHexAddress =
                        HexAddress.create
                            { sectorX = 1
                            , sectorY = 1
                            , x = 1
                            , y = 1
                            }

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
                        HexAddress.create
                            { sectorX = 1
                            , sectorY = 1
                            , x = 1
                            , y = 1
                            }

                    targetHexAddress =
                        HexAddress.create
                            { sectorX = 1
                            , sectorY = 1
                            , x = 2
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
                        HexAddress.create
                            { sectorX = 1
                            , sectorY = 1
                            , x = 1
                            , y = 1
                            }

                    targetHexAddress =
                        HexAddress.create
                            { sectorX = 1
                            , sectorY = 1
                            , x = 1 + deltaX
                            , y = 1 + deltaY
                            }

                    res =
                        HexAddress.shiftAddressBy
                            { deltaX = deltaX, deltaY = deltaY }
                            { maxX = Traveller.numHexCols, maxY = Traveller.numHexRows }
                            sourceHexAddress
                in
                Expect.equal res targetHexAddress
        ]
