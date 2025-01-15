module Tests exposing (..)

import Expect
import Fuzz
import MD5
import Set
import Test exposing (Test, describe)
import Traveller
import Traveller.HexAddress as HexAddress exposing (HexAddress, universalHexX)


{-| replaces any zeroes with a 1 instead.
-}
fuzzNoZeroes : Fuzz.Fuzzer Int -> Fuzz.Fuzzer Int
fuzzNoZeroes intFuzzer =
    intFuzzer
        |> Fuzz.map
            (\x ->
                if x == 0 then
                    1

                else
                    x
            )


fuzzAddTen : Fuzz.Fuzzer Int -> Fuzz.Fuzzer Int
fuzzAddTen intFuzzer =
    intFuzzer
        |> Fuzz.map ((+) 10)


suite : Test
suite =
    let
        hexAddressOne =
            { x = 1
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
                            sourceHexAddress
                in
                Expect.equal res targetHexAddress
        , Test.test "adding 1 to each works" <|
            \() ->
                let
                    sourceHexAddress =
                        hexAddressOne

                    targetHexAddress =
                        { x = 2
                        , y = 2
                        }

                    res =
                        HexAddress.shiftAddressBy
                            { deltaX = 1, deltaY = 1 }
                            sourceHexAddress
                in
                Expect.equal res targetHexAddress
        , Test.test "subtracting 1 across sectors works" <|
            \() ->
                let
                    sourceHexAddress =
                        hexAddressOne

                    expectedHexAddress =
                        { x = 32
                        , y = 40
                        }

                    res =
                        HexAddress.shiftAddressBy
                            { deltaX = -1, deltaY = -1 }
                            sourceHexAddress
                in
                Expect.equal expectedHexAddress res
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
                        { hexAddressOne
                            | x = hexAddressOne.x + deltaX
                            , y = hexAddressOne.y + deltaY
                        }

                    res =
                        HexAddress.shiftAddressBy
                            { deltaX = deltaX, deltaY = deltaY }
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
                            sourceHexAddress
                in
                { x = res.x, y = res.y }
                    |> Expect.all
                        [ .x >> Expect.notEqual 0
                        , .y >> Expect.notEqual 0
                        ]
        , Test.test "shifting an address by 30 across sectors works" <|
            \_ ->
                let
                    sourceHexAddress =
                        { hexAddressOne | x = 1, y = 11 }

                    expectedHexAddress =
                        { x = 31
                        , y = 1
                        }

                    res =
                        HexAddress.shiftAddressBy
                            { deltaX = 30, deltaY = 30 }
                            sourceHexAddress
                in
                Expect.equal expectedHexAddress res
        , Test.test "foo" <|
            \_ ->
                let
                    sourceHexAddress =
                        { x = 22
                        , y = 38
                        }

                    expectedHexAddress =
                        { x = 20
                        , y = 28
                        }

                    res =
                        HexAddress.shiftAddressBy
                            { deltaX = 30, deltaY = 30 }
                            sourceHexAddress
                in
                Expect.equal expectedHexAddress res
        , Test.fuzz2
            (Fuzz.pair
                (Fuzz.intRange 1 20)
                (Fuzz.intRange 1 20)
            )
            (Fuzz.pair
                (Fuzz.intRange 0 10)
                (Fuzz.intRange 0 10)
            )
            "HexAddress.between'ing an address never returns a hex with 0 in either hex coordinates"
          <|
            \( startingX, startingY ) ( deltaX, deltaY ) ->
                let
                    sourceHexAddress =
                        { hexAddressOne | x = startingX, y = startingY }

                    otherHexAddress =
                        HexAddress.shiftAddressBy
                            { deltaX = deltaX, deltaY = deltaY }
                            sourceHexAddress

                    hexRange : List HexAddress
                    hexRange =
                        HexAddress.between sourceHexAddress otherHexAddress
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
                        HexAddress.between sourceHexAddress targetHexAddress
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
                        HexAddress.between sourceHexAddress targetHexAddress
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
                        HexAddress.between sourceHexAddress targetHexAddress

                    hashAddr : HexAddress -> String
                    hashAddr addr =
                        MD5.fromBytes [ addr.x, addr.y ]
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
        , Test.test "ensure universal to the right is larger than one to the left" <|
            \() ->
                let
                    leftHexAddr =
                        { hexAddressOne | x = 22, y = 1 }

                    rightHexAddr =
                        { leftHexAddr | x = leftHexAddr.x + 1 }

                    leftX =
                        leftHexAddr.x

                    rightX =
                        rightHexAddr.x
                in
                Expect.greaterThan leftX rightX
        , Test.test "`between` across sectors right at the border returns two different sector hexes" <|
            \() ->
                let
                    sourceHexAddress =
                        { hexAddressOne | x = 31, y = 1 }

                    targetHexAddress =
                        { hexAddressOne | x = 1, y = 1 }

                    hexesBetween =
                        HexAddress.between sourceHexAddress targetHexAddress
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
