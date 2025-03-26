module Traveller.Atmosphere exposing (Atmosphere(..), StellarAtmosphere, atmosphere, atmosphereDescription, codec)

import Codec exposing (Codec)
import Json.Decode as JsDecode
import Json.Encode as JsEncode
import Parser exposing ((|.), (|=), Parser)
import Parser.Extras as Parser
import Traveller.StellarTaint exposing (StellarTaint, codecStellarTaint)


type alias StellarAtmosphere =
    { code : Int
    , irritant : Bool
    , taint : StellarTaint
    , characteristic : String
    , bar : Float
    , gasType : Maybe String
    , density : String
    , hazardCode : Maybe String
    }


codec : Codec StellarAtmosphere
codec =
    Codec.object StellarAtmosphere
        |> Codec.field "code" .code Codec.int
        |> Codec.field "irritant" .irritant Codec.bool
        |> Codec.field "taint" .taint codecStellarTaint
        |> Codec.field "characteristic" .characteristic Codec.string
        |> Codec.field "bar" .bar Codec.float
        |> Codec.field "gasType" .gasType (Codec.maybe Codec.string)
        |> Codec.field "density" .density Codec.string
        |> Codec.field "hazardCode" .hazardCode (Codec.maybe Codec.string)
        |> Codec.buildObject


type Atmosphere
    = ZeroVacuum
    | OneTrace
    | TwoVeryThinTainted
    | ThreeVeryThin
    | FourThinTainted
    | FiveThin
    | SixStandard
    | SevenStandardTainted
    | EightDense
    | NineDenseTainted
    | ATenExotic
    | BElevenCorrosive
    | CTwelveInsidious
    | DThirteenDenseHigh
    | EFourteenEllipsoid
    | FFifteenThinLow
    | GSixteen
    | HSeventeen


atmosphere : Parser Atmosphere
atmosphere =
    Parser.oneOf
        [ Parser.succeed ZeroVacuum |. Parser.symbol "0"
        , Parser.succeed OneTrace |. Parser.symbol "1"
        , Parser.succeed TwoVeryThinTainted |. Parser.symbol "2"
        , Parser.succeed ThreeVeryThin |. Parser.symbol "3"
        , Parser.succeed FourThinTainted |. Parser.symbol "4"
        , Parser.succeed FiveThin |. Parser.symbol "5"
        , Parser.succeed SixStandard |. Parser.symbol "6"
        , Parser.succeed SevenStandardTainted |. Parser.symbol "7"
        , Parser.succeed EightDense |. Parser.symbol "8"
        , Parser.succeed NineDenseTainted |. Parser.symbol "9"
        , Parser.succeed ATenExotic |. Parser.symbol "A"
        , Parser.succeed BElevenCorrosive |. Parser.symbol "B"
        , Parser.succeed CTwelveInsidious |. Parser.symbol "C"
        , Parser.succeed DThirteenDenseHigh |. Parser.symbol "D"
        , Parser.succeed EFourteenEllipsoid |. Parser.symbol "E"
        , Parser.succeed FFifteenThinLow |. Parser.symbol "F"
        , Parser.succeed GSixteen |. Parser.symbol "G"
        , Parser.succeed HSeventeen |. Parser.symbol "H"
        ]


atmosphereDescription : Atmosphere -> String
atmosphereDescription code =
    case code of
        ZeroVacuum ->
            "Vacuum"

        OneTrace ->
            "Trace"

        TwoVeryThinTainted ->
            "Very Thin, Tainted"

        ThreeVeryThin ->
            "Very Thin"

        FourThinTainted ->
            "Thin, Tainted"

        FiveThin ->
            "Thin"

        SixStandard ->
            "Standard"

        SevenStandardTainted ->
            "Standard, Tainted"

        EightDense ->
            "Dense"

        NineDenseTainted ->
            "Dense, Tainted"

        ATenExotic ->
            "Exotic"

        BElevenCorrosive ->
            "Corrosive"

        CTwelveInsidious ->
            "Insidious"

        DThirteenDenseHigh ->
            "Dense, High"

        EFourteenEllipsoid ->
            "Ellipsoid"

        FFifteenThinLow ->
            "Thin, Low"

        GSixteen ->
            "Gas, Helium"

        HSeventeen ->
            "Gas, Hydrogen"
