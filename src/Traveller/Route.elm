module Traveller.Route exposing (..)

import Codec exposing (Codec)


type alias Route =
    { x : Int
    , y : Int
    , shipId : Int
    , year : Int
    , day : Int
    }


codecRoute : Codec Route
codecRoute =
    Codec.object Route
        |> Codec.field "x" .x Codec.int
        |> Codec.field "y" .y Codec.int
        |> Codec.field "ship_id" .shipId Codec.int
        |> Codec.field "year" .year Codec.int
        |> Codec.field "day" .day Codec.int
        |> Codec.buildObject


type alias RouteList =
    List Route
