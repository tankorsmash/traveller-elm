module Tests exposing (..)

import Expect
import Fuzz
import MD5
import Set
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

        hexRules =
            { maxX = Traveller.numHexCols, maxY = Traveller.numHexRows }
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
        , Test.fuzz2
            (Fuzz.intRange -10 10)
            (Fuzz.intRange -10 10)
            "HexAddress.between'ing an address never returns a hex with 0 in either hex coordinates"
          <|
            \deltaX deltaY ->
                let
                    sourceHexAddress =
                        hexAddressOne

                    otherHexAddress =
                        HexAddress.shiftAddressBy
                            { deltaX = deltaX, deltaY = deltaY }
                            { maxX = Traveller.numHexCols, maxY = Traveller.numHexRows }
                            sourceHexAddress

                    hexRange =
                        HexAddress.between hexRules sourceHexAddress otherHexAddress
                in
                hexRange
                    |> (\hexes ->
                            ( hexes |> List.map .x |> List.sum
                            , hexes |> List.map .y |> List.sum
                            )
                       )
                    |> Expect.all
                        [ Tuple.first >> Expect.notEqual 0 >> Expect.onFail "one of the x's was 0"
                        , Tuple.second >> Expect.notEqual 0 >> Expect.onFail "one of the y's was 0"
                        ]
        , Test.test "`between` with same input twice returns itself" <|
            \() ->
                let
                    sourceHexAddress =
                        hexAddressOne

                    targetHexAddress =
                        hexAddressOne

                    hexesBetween =
                        HexAddress.between hexRules sourceHexAddress targetHexAddress
                in
                case hexesBetween of
                    [] ->
                        Expect.fail "empty hex range"

                    [ hex ] ->
                        Expect.equal sourceHexAddress hex

                    _ ->
                        Expect.fail "too many hexes in range"
        , Test.test "`between` with 1 space horizontally works" <|
            \() ->
                let
                    sourceHexAddress =
                        hexAddressOne

                    targetHexAddress =
                        { hexAddressOne | x = 3 }

                    hexesBetween =
                        HexAddress.between hexRules sourceHexAddress targetHexAddress
                in
                Expect.equal 3 <| List.length hexesBetween
        , Test.test "`between` with 3x3 works" <|
            \() ->
                let
                    sourceHexAddress =
                        hexAddressOne

                    targetHexAddress =
                        { hexAddressOne | x = 3, y = 3 }

                    hexesBetween =
                        HexAddress.between hexRules sourceHexAddress targetHexAddress

                    hashAddr : HexAddress -> String
                    hashAddr addr =
                        MD5.fromBytes [ addr.sectorX, addr.sectorY, addr.x, addr.y ]
                            |> List.map String.fromInt
                            |> String.join ""

                    countUniqueHexes : List HexAddress -> Int
                    countUniqueHexes hexes =
                        hexes
                            |> List.map hashAddr
                            |> Set.fromList
                            |> Set.size
                in
                Expect.all
                    [ Expect.equal 9 << List.length
                    , \hexes ->
                        Expect.equal (List.length hexes) (countUniqueHexes hexes)
                    ]
                    hexesBetween
        , Test.test "`between` across sectors right at the border returns two different sector hexes" <|
            \() ->
                let
                    sourceHexAddress =
                        { hexAddressOne | x = 31, y = 1, sectorX = 1 }

                    targetHexAddress =
                        { hexAddressOne | x = 1, y = 1, sectorX = 2 }

                    hexesBetween =
                        HexAddress.between hexRules sourceHexAddress targetHexAddress
                in
                Expect.all
                    [ Expect.equal 3 << List.length
                    , List.filter (\hex -> hex.sectorX == 1)
                        >> List.length
                        >> Expect.equal 2
                        >> Expect.onFail "expected 2 hexes in sector 1"
                    ]
                    hexesBetween
        ]
