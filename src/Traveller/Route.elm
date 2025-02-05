module Traveller.Route exposing (..)

import Codec exposing (Codec)
import Traveller.HexAddress exposing (HexAddress)


type alias Route =
    { address : HexAddress
    , shipId : Int
    , year : Int
    , day : Int
    }


type alias RouteList =
    List Route


codecRoute : Codec.Codec Route
codecRoute =
    Codec.object (\x y shipId year day -> Route { x = x, y = y } shipId year day)
        |> Codec.field "origin_x" (.address >> .x) Codec.int
        |> Codec.field "origin_y" (.address >> .y) Codec.int
        |> Codec.field "ship_id" .shipId Codec.int
        |> Codec.field "year" .year Codec.int
        |> Codec.field "day" .day Codec.int
        |> Codec.buildObject
