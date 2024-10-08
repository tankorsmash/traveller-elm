module Traveller.Star exposing (CompanionStar, Star, StarColour, codecStar, sampleSystemText, starColourRGB)

import Codec exposing (Codec)
import Json.Decode as JsDecode
import Json.Encode as JsEncode
import Parser exposing ((|.), (|=), Parser)
import Parser.Extras as Parser
import Traveller.Point exposing (StellarPoint, codecStellarPoint)
import Traveller.StellarObject exposing (StellarObject, codecStellarObject)


sampleSystemText : String
sampleSystemText =
    """
{
  "orbitPosition": {
    "x": 0,
    "y": 0
  },
  "inclination": 0,
  "eccentricity": 0,
  "effectiveHZCODeviation": 0,
  "stellarClass": "V",
  "stellarType": "G",
  "totalObjects": 10,
  "subtype": 4,
  "orbitType": 0,
  "mass": 1.1,
  "diameter": 1.1,
  "temperature": 6000,
  "age": 0.01,
  "colour": "Yellow",
  "companion": null,
  "orbit": 0,
  "period": 0,
  "baseline": 10,
  "emptyOrbits": 0,
  "spread": 0.34843644203935276,
  "availableOrbits": [
    [
      0.03,
      20
    ]
  ],
  "stellarObjects": [],
  "occupiedOrbits": [
    0.3784364420393528,
    0.7268728840787055,
    1.0753093261180582,
    1.2495275471377345,
    1.4934330565652814,
    1.8767131428085695,
    2.2599932290518576,
    2.7129606037030163,
    3.2356152667620455,
    3.584051708801398
  ],
  "orbitSequence": "A",
  "jump": 3.0392399646555845
}
"""


type StarColour
    = Blue
    | BlueWhite
    | White
    | YellowWhite
    | Yellow
    | LightOrange
    | OrangeRed
    | Red
    | Brown
    | DeepDimRed


codecStarColour : Codec StarColour
codecStarColour =
    Codec.enum Codec.string
        [ ( "Blue", Blue )
        , ( "Blue White", BlueWhite )
        , ( "White", White )
        , ( "Yellow White", YellowWhite )
        , ( "Yellow", Yellow )
        , ( "Light Orange", LightOrange )
        , ( "Orange Red", OrangeRed )
        , ( "Red", Red )
        , ( "Brown", Brown )
        , ( "Deep Dim Red", DeepDimRed )
        ]


starColourRGB : StarColour -> String
starColourRGB colour =
    case colour of
        Blue ->
            "#000077"

        BlueWhite ->
            "#87cefa"

        White ->
            "#FFFFFF"

        YellowWhite ->
            "#ffffe0"

        Yellow ->
            "#ffff00"

        LightOrange ->
            "#ffbf00"

        OrangeRed ->
            "#ff4500"

        Red ->
            "#ff0000"

        Brown ->
            "#f4a460"

        DeepDimRed ->
            "#800000"


type alias Star =
    { orbitPosition : StellarPoint
    , inclination : Int
    , eccentricity : Float
    , effectiveHZCODeviation : Maybe Float
    , stellarClass : String
    , stellarType : String
    , totalObjects : Int
    , subtype : Maybe Int
    , orbitType : Int
    , mass : Float
    , diameter : Float
    , temperature : Int
    , age : Float
    , colour : Maybe StarColour
    , companion : Maybe CompanionStar
    , orbit : Float
    , period : Float
    , baseline : Int
    , emptyOrbits : Int
    , spread : Float
    , availableOrbits : List ( Float, Float )
    , stellarObjects : List StellarObject
    , occupiedOrbits : List Float
    , orbitSequence : String
    , jump : Float
    }


type alias CompanionStar =
    { orbitPosition : StellarPoint
    , inclination : Int
    , eccentricity : Float
    , effectiveHZCODeviation : Maybe Float
    , stellarClass : String
    , stellarType : String
    , totalObjects : Int
    , subtype : Maybe Int
    , orbitType : Int
    , mass : Maybe Float
    , diameter : Maybe Float
    , temperature : Int
    , age : Float
    , colour : Maybe StarColour
    , orbit : Float
    , period : Maybe Float
    , baseline : Int
    , emptyOrbits : Int
    , spread : Float
    , availableOrbits : List ( Float, Float )
    , stellarObjects : List StellarObject
    , occupiedOrbits : List Float
    , orbitSequence : String
    , jump : Maybe Float
    }


codecStar : Codec Star
codecStar =
    Codec.object Star
        |> Codec.field "orbitPosition" .orbitPosition codecStellarPoint
        |> Codec.field "inclination" .inclination Codec.int
        |> Codec.field "eccentricity" .eccentricity Codec.float
        |> Codec.field "effectiveHZCODeviation" .effectiveHZCODeviation (Codec.nullable Codec.float)
        |> Codec.field "stellarClass" .stellarClass Codec.string
        |> Codec.field "stellarType" .stellarType Codec.string
        |> Codec.field "totalObjects" .totalObjects Codec.int
        |> Codec.field "subtype" .subtype (Codec.nullable Codec.int)
        |> Codec.field "orbitType" .orbitType Codec.int
        |> Codec.field "mass" .mass Codec.float
        |> Codec.field "diameter" .diameter Codec.float
        |> Codec.field "temperature" .temperature Codec.int
        |> Codec.field "age" .age Codec.float
        |> Codec.optionalField "colour" .colour codecStarColour
        |> Codec.optionalNullableField "companion" .companion codecCompanionStar
        |> Codec.field "orbit" .orbit Codec.float
        |> Codec.field "period" .period Codec.float
        |> Codec.field "baseline" .baseline Codec.int
        |> Codec.field "emptyOrbits" .emptyOrbits Codec.int
        |> Codec.field "spread" .spread Codec.float
        |> Codec.field "availableOrbits" .availableOrbits (Codec.list (Codec.tuple Codec.float Codec.float))
        |> Codec.field "stellarObjects" .stellarObjects (Codec.list codecStellarObject)
        |> Codec.field "occupiedOrbits" .occupiedOrbits (Codec.list Codec.float)
        |> Codec.field "orbitSequence" .orbitSequence Codec.string
        |> Codec.field "jump" .jump Codec.float
        |> Codec.buildObject


codecCompanionStar : Codec CompanionStar
codecCompanionStar =
    Codec.object CompanionStar
        |> Codec.field "orbitPosition" .orbitPosition codecStellarPoint
        |> Codec.field "inclination" .inclination Codec.int
        |> Codec.field "eccentricity" .eccentricity Codec.float
        |> Codec.field "effectiveHZCODeviation" .effectiveHZCODeviation (Codec.nullable Codec.float)
        |> Codec.field "stellarClass" .stellarClass Codec.string
        |> Codec.field "stellarType" .stellarType Codec.string
        |> Codec.field "totalObjects" .totalObjects Codec.int
        |> Codec.field "subtype" .subtype (Codec.nullable Codec.int)
        |> Codec.field "orbitType" .orbitType Codec.int
        |> Codec.optionalField "mass" .mass Codec.float
        |> Codec.optionalField "diameter" .diameter Codec.float
        |> Codec.field "temperature" .temperature Codec.int
        |> Codec.field "age" .age Codec.float
        |> Codec.optionalField "colour" .colour codecStarColour
        |> Codec.field "orbit" .orbit Codec.float
        |> Codec.nullableField "period" .period Codec.float
        |> Codec.field "baseline" .baseline Codec.int
        |> Codec.field "emptyOrbits" .emptyOrbits Codec.int
        |> Codec.field "spread" .spread Codec.float
        |> Codec.field "availableOrbits" .availableOrbits (Codec.list (Codec.tuple Codec.float Codec.float))
        |> Codec.field "stellarObjects" .stellarObjects (Codec.list codecStellarObject)
        |> Codec.field "occupiedOrbits" .occupiedOrbits (Codec.list Codec.float)
        |> Codec.field "orbitSequence" .orbitSequence Codec.string
        |> Codec.optionalField "jump" .jump Codec.float
        |> Codec.buildObject
