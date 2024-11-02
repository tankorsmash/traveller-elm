module Traveller.Orbit exposing (StellarOrbit(..), codec)

import Codec exposing (Codec)
import Json.Decode as JsDecode
import Json.Encode as JsEncode
import Parser exposing ((|.), (|=), Parser)
import Parser.Extras as Parser


type StellarOrbit
    = SimpleOrbit Float
    | ComplexOrbit { zone : String, orbit : Float }


codec : Codec StellarOrbit
codec =
    Codec.build
        (\stellarOrbit ->
            case stellarOrbit of
                SimpleOrbit orbit ->
                    JsEncode.float orbit

                ComplexOrbit complexData ->
                    JsEncode.object
                        [ ( "zone", JsEncode.string complexData.zone )
                        , ( "orbit", JsEncode.float complexData.orbit )
                        ]
        )
        (JsDecode.oneOf
            [ JsDecode.map SimpleOrbit JsDecode.float
            , JsDecode.map2
                (\zone_ orbit -> ComplexOrbit { zone = zone_, orbit = orbit })
                (JsDecode.field "zone" JsDecode.string)
                (JsDecode.field "orbit" JsDecode.float)
            ]
        )
