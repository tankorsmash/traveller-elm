module Traveller.Parser exposing
    ( Allegiance(..)
    , Base(..)
    , CulturalExtension(..)
    , EconomicExtension(..)
    , Government(..)
    , Hydrosphere(..)
    , ImportanceExtension(..)
    , LawLevel(..)
    , Nobility(..)
    , PBG
    , PlanetarySize(..)
    , RefereeData
    , Remark(..)
    , SpectralClassification(..)
    , SpectralType(..)
    , Starport(..)
    , StellarData(..)
    , SystemName
    , T5TabEntry
    , TechLevel(..)
    , UWP
    , Zone(..)
    , allegiance
    , base
    , culturalExtension
    , debugLogErrors
    , economicExtension
    , government
    , hydrosphere
    , importanceExtension
    , lawLevel
    , nobility
    , parse
    , parseBool
    , parseExample
    , parseRefereeCsv
    , parseRefereeData
    , parseSingleDigit
    , pbg
    , planetarySize
    , remark
    , sampleRefereeSystemsText
    , sampleRefereeText
    , singleTab
    , starport
    , stellarData
    , t5TabEntry
    , techLevel
    , uwp
    , zone
    )

import Codec exposing (Codec)
import Json.Decode as JsDecode
import Json.Encode as JsEncode
import Parser exposing ((|.), (|=), Parser)
import Parser.Extras as Parser
import Traveller.Atmosphere exposing (Atmosphere, atmosphere)
import Traveller.EHex exposing (EHex, eHex)
import Traveller.HexId exposing (HexId, hexId)
import Traveller.Population exposing (Population, population)
import Traveller.StellarObject exposing (codecStellarObject)



{-
   tab separated values
   Hex    Name    UWP    Bases    Remarks    Zone    PBG    Allegiance    Stars    {Ix}    (Ex)    [Cx]    Nobility    W
   0606    K3 V/M8 V    ???????-?                004        K3 V M8 V     {}    ()    []        5
   0608    M1 V    ???????-?                004        M1 V     {}    ()    []        5
-}
{-
   spec


   Field    Details    Example
   Hex    Location in sector (XXYY)    0133
   Name    MainWorld name    Emape
   UWP    starport, size, atmosphere, hydrographics, population, government, law level, tech level    B564500-B
   Bases    N = Naval Base, S = Scout Base, etc.    N
   Remarks    Ag = Agricultural, In = Industrial, etc.    Ag Ni Pr Da
   Zone    A = Amber, R = Red    A
   PBG    Population multiplier, Belts, Gas giants    503
   Allegiance    Im = Imperial, Zh = Zhodani, etc.    Im
   Stars    Morgan-Keenan stellar classifications, space separated    M0 V
   {Ix}    Importance extension    { 2 }
   (Ex)    Economic extension    (A46+2)
   [Cx]    Cultural extension    [1716]
   Nobility    Imperial noble rank(s) assigned to    BcC
   W    Number of worlds in the system    6

-}


type alias SystemName =
    String


{-| Universal World Profile

Description (Specifications)
The UWP is an encoding scheme consisting of 8 alphanumeric codes in a specific sequence. Each digit encodes a specific characteristic of the individual world. The encoding allows for a compact notation about many of the most important characteristics. This allows the UWP, and the data it encodes, to be included in a compact file format and, in some cases, directly on the map.

The encoding fields in order and meanings. Please see the individual articles on these characteristics for the detailed meanings of their values.

Starport
The starport is the center of trade for the system, and a port for any visiting starships. The value encoded is the quality of the port and ranges from A (the best and most extensive) to E (the worst, little more than a spot of cleared ground), and X. The secondary set of spaceport ratings includes F, G, H, and Y.
Size
Encoding indicates the diameter of the world in thousands of miles. The size is also an indicator of surface gravity and likely atmospheric density.
Atmosphere
Encoding indicates in general the atmospheric density, and presence of dangerous taints requiring additional breathing gear. EHex values over C indicate special conditions for the atmosphere.
Hydrosphere
Encoding indicated percentage of the world surface covered with oceans of water. For some atmosphere types (A, B, C) the fluid may be something other than water. The presence of water indicates the possibility of wilderness refueling on the planet surface.
Population
Encoding is the order of magnitude of the number of sophonts living on the world or in the entire system. For example a population code of 3 indicated there will be Thousands (103) sophonts on the world.
Government
Encoding indicates a specific type of government which rules over the world or system. Please see the article to decipher which specific government exists.
Law Level
Encoding indicates a general level of density of the legal system and law enforcement with values ranging from 0 to F. Low values indicate relatively few laws and lax enforcement, with higher values indicating more complex legal systems and more assertive enforcement of the laws.
Technology Level
Encodes a general level of ability with technology. Technology level encompasses both a understanding of technology and the infrastructure capability to manufacture and distribute the items of technology. Lower levels indicate a poorer understanding of the technology and lack the ability to manufacture items.

-}
type alias UWP =
    { starport : Starport
    , size : PlanetarySize
    , atmosphere : Atmosphere
    , hydrosphere : Hydrosphere
    , population : Population
    , government : Government
    , lawLevel : LawLevel
    , techLevel : TechLevel
    }


uwp : Parser UWP
uwp =
    Parser.succeed UWP
        |= starport
        |= planetarySize
        |= atmosphere
        |= hydrosphere
        |= population
        |= government
        |= lawLevel
        |. Parser.chompIf (\c -> c == '-')
        |= techLevel



{-

   Selected Base Codes
   Code    Type    Allegiance    Imperial?    Remarks
   A    Naval Base and Scout Base    Third Imperium    Imperial    Imperial Institution
   B    Naval Base and Way Station    Third Imperium    Imperial    Imperial Institution
   C    Vargr Corsair Base    Vargr Extents (Vargr)    Non-Imperial    Vargr Institution
   D    Naval Depot (Depot)    Third Imperium    Imperial    Imperial Institution
   E    Hiver Embassy Center    Hive Federation (Hiver)    Non-Imperial    Hiver Institution
   F    Military and Naval Base    Non-Imperial Minor or Major Race    Non-Imperial    Generic code for all non-Imperial bases not represented by other codes.
   G    Vargr Naval Base    Vargr Extents (Vargr)    Non-Imperial    Vargr Institution
   H    Vargr Naval Base and Corsair Base    Vargr Extents (Vargr)    Non-Imperial    Vargr Institution
   J    Naval Base    Non-Imperial Minor or Major Race    Non-Imperial    Generic code for all non-Imperial bases not represented by other codes.
   K    K'kree Naval Base    Two Thousand Worlds (K'kree)    Non-Imperial    K'kree Institution
   L    Hiver Naval Base    Hive Federation (Hiver)    Non-Imperial    Hiver Institution
   M    Military Base    Non-Imperial Minor or Major Race    Non-Imperial    Generic code for all non-Imperial bases not represented by other codes.
   N    Naval Base    Third Imperium    Imperial    Imperial Institution
   O    K'kree Naval Outpost    Two Thousand Worlds (K'kree)    Non-Imperial    K'kree Institution
   P    Droyne Naval Base    Droyne Oytrip Yatroy (Droyne)    Non-Imperial    Droyne Institution
   Q    Droyne Military Garrison    Droyne Oytrip Yatroy (Droyne)    Non-Imperial    Droyne Institution
   R    Aslan Clan Base    Aslan Hierate (Aslan)    Non-Imperial    Aslan Institution
   S    Scout Base    Third Imperium    Imperial    Imperial Institution
   T    Aslan Tlaukhu Base    Aslan Hierate (Aslan)    Non-Imperial    Aslan Institution
   U    Aslan Tlaukhu and Clan Base    Aslan Hierate (Aslan)    Non-Imperial    Aslan Institution
   V    Scout/Exploration Base    Non-Imperial Minor or Major Race    Non-Imperial    Generic code for all non-Imperial bases not represented by other codes.
   W    Way Station    Third Imperium    Imperial    Imperial Institution
   X    Zhodani Relay Station    Zhodani Consulate (Zhodani)    Non-Imperial    Zhodani Institution
   Y    Zhodani Depot    Zhodani Consulate (Zhodani)    Non-Imperial    Zhodani Institution
   Z    Zhodani Naval and Military Base    Zhodani Consulate (Zhodani)    Non-Imperial    Zhodani Institution

-}


type Base
    = NavalBaseScout
    | NavalBaseWayStation
    | VargarCorsairBase
    | NavalDepot
    | HiverEmbassyCenter
    | MilitaryNavalBase
    | VargarNavalBase
    | VargarNavalBaseCorsairBase
    | KkreeNavalBase
    | HiverNavalBase
    | MilitaryBase
    | DroyneNavalBase
    | DroyneMilitaryGarrison
    | AslanClanBase
    | ScoutBase
    | AslanTlaukhuBase
    | AslanTlaukhuClanBase
    | ScoutExplorationBase
    | WayStation
    | ZhodaniRelayStation
    | ZhodaniDepot
    | ZhodaniNavalMilitaryBase


base : Parser.Parser Base
base =
    Parser.oneOf
        [ Parser.succeed NavalBaseScout |. Parser.symbol "A"
        , Parser.succeed NavalBaseWayStation |. Parser.symbol "B"
        , Parser.succeed VargarCorsairBase |. Parser.symbol "C"
        , Parser.succeed NavalDepot |. Parser.symbol "D"
        , Parser.succeed HiverEmbassyCenter |. Parser.symbol "E"
        , Parser.succeed MilitaryNavalBase |. Parser.symbol "F"
        , Parser.succeed VargarNavalBase |. Parser.symbol "G"
        , Parser.succeed VargarNavalBaseCorsairBase |. Parser.symbol "H"
        , Parser.succeed KkreeNavalBase |. Parser.symbol "K"
        , Parser.succeed HiverNavalBase |. Parser.symbol "L"
        , Parser.succeed MilitaryBase |. Parser.symbol "M"
        , Parser.succeed DroyneNavalBase |. Parser.symbol "N"
        , Parser.succeed DroyneMilitaryGarrison |. Parser.symbol "O"
        , Parser.succeed AslanClanBase |. Parser.symbol "P"
        , Parser.succeed ScoutBase |. Parser.symbol "S"
        , Parser.succeed AslanTlaukhuBase |. Parser.symbol "T"
        , Parser.succeed AslanTlaukhuClanBase |. Parser.symbol "U"
        , Parser.succeed ScoutExplorationBase |. Parser.symbol "V"
        , Parser.succeed WayStation |. Parser.symbol "W"
        , Parser.succeed ZhodaniRelayStation |. Parser.symbol "X"
        , Parser.succeed ZhodaniDepot |. Parser.symbol "Y"
        , Parser.succeed ZhodaniNavalMilitaryBase |. Parser.symbol "Z"
        ]



{-

   Starport Classifications
   Class A
   Excellent quality installation. Refined fuel available. Annual maintenance overhaul available. Shipyard capable of both starship and non-starship construction present.
   Class B
   Good quality installation. Refined fuel available. Annual maintenance overhaul available. Shipyard capable of constructing non-starships present.
   Class C
   Routine quality installation. Only unrefined fuel available. Reasonable repair facilities are present.
   Class D
   Poor quality installation. Only unrefined fuel available. No repair or shipyard facilities present.
   Class E
   Frontier installation. Essentially a bare spot of bedrock with no fuel, facilities, or bases present.
   Class X
   No starport. Class X starports are generally indicative of an interdiction. No provision is made for any starship landings and such landings are probably prohibited.
-}


type Starport
    = ClassAExcellent
    | ClassBGood
    | ClassCRoutine
    | ClassDPoor
    | ClassEFrontier
    | ClassXNoStarport


starport : Parser Starport
starport =
    Parser.oneOf
        [ Parser.succeed ClassAExcellent |. Parser.symbol "A"
        , Parser.succeed ClassBGood |. Parser.symbol "B"
        , Parser.succeed ClassCRoutine |. Parser.symbol "C"
        , Parser.succeed ClassDPoor |. Parser.symbol "D"
        , Parser.succeed ClassEFrontier |. Parser.symbol "E"
        , Parser.succeed ClassXNoStarport |. Parser.symbol "X"
        ]



{-
   Planetary Size
   Code    Description    Diameter (Km)    Horizon (meters)    Expected
   Gravity    Remarks
   R    Planetary Ring    Multiple < 1    —    Microgravity
   (0.01 G or less)    Planetary Ring, Planetoid
   D    Debris    1+ < 200    tens (maximum)    Microgravity
   (0.01 G or less)    Debris Field, Planetoid
   0    Asteroid / Planetary Belt    Multiple < 200    tens (maximum)    Microgravity
   (0.01 G or less)    Planetoid Belt, Planetoid
   S    Very Small    200-799    hundreds    Very Low Gravity
   (400 km, 0.02g - 0.03g)    Very Small World
   1    Small    800-2399    2000    Very Low Gravity
   (1,600 km, 0.05g - 0.09g)    Small World
   2    Small (e.g. Luna)    2,400-3,999    2500    Low Gravity
   (3,200 km, 0.10g - 0.17g)    Small World
   3    Small (e.g. Mercury)    4,000-5,599    3000    Low Gravity
   (4,800 km, 0.24g - 0.34g)    Small World
   4    Medium (e.g. Mars)    5,600-7,199    3600    Low Gravity
   (6,400 km, 0.32g - 0.46g)    Meso World
   5    Medium    7,200-8,799    4000    Standard Gravity
   (8,000 km, 0.40g - 0.57g)    Meso World
   6    Medium    8,800-10,399    4400    Standard Gravity
   (9,600 km, 0.60g - 0.81g)    Meso World
   7    Large    10,400-11,999    4800    Standard Gravity
   (11,200 km, 0.70g - 0.94g)    Macro World
   8    Large (e.g. Venus / Terra)    12,000-13,599    5100    Standard Gravity
   (12,800 km, 0.80g - 1.08g)    Macro World
   9    Large    13,600-15,199    5400    Standard Gravity
   (14,400 km, 1.03g - 1.33g)    Macro World
   A (10)    Huge    15,200-16,799    5700    Standard Gravity
   (16,000 km, 1.14g - 1.48g)    Big World
   B (11)    Huge    16,800 - 18,399    6000    High Gravity
   (17,600 km, 1.49g - 1.89g)    Big World
   C (12)    Huge    18,400+    6300    High Gravity
   (19,400 km, 1.9g - 2.0g)    Big World
   SGG    Small Gas Giant (no further codes)    40,000-120,000    —    Macrogravity
   (2.01 G or higher)    Gas Giant
   LGG    Large Gas Giant (no further codes)    120,000-240,000+    —    Macrogravity
   (2.01 G or higher)    Gas Giant

-}


type PlanetarySize
    = RPlanetaryRing
    | DDebris
    | ZeroAsteroidPlanetaryBelt
    | SVerySmall
    | OneSmall
    | TwoSmall
    | ThreeSmall
    | FourMedium
    | FiveMedium
    | SixMedium
    | SevenLarge
    | EightLarge
    | NineLarge
    | ATenHuge
    | BElevenHuge
    | CTwelveHuge
    | SGGSmallGasGiant
    | LGGLargeGasGiant


{-| Parse a planetary size. the referree sheet only has single digit stuff, but just in case.
-}
planetarySize : Parser PlanetarySize
planetarySize =
    Parser.oneOf
        [ Parser.succeed RPlanetaryRing |. Parser.symbol "R"
        , Parser.succeed DDebris |. Parser.symbol "D"
        , Parser.succeed ZeroAsteroidPlanetaryBelt |. Parser.symbol "0"
        , Parser.succeed SVerySmall |. Parser.symbol "S"
        , Parser.succeed OneSmall |. Parser.symbol "1"
        , Parser.succeed TwoSmall |. Parser.symbol "2"
        , Parser.succeed ThreeSmall |. Parser.symbol "3"
        , Parser.succeed FourMedium |. Parser.symbol "4"
        , Parser.succeed FiveMedium |. Parser.symbol "5"
        , Parser.succeed SixMedium |. Parser.symbol "6"
        , Parser.succeed SevenLarge |. Parser.symbol "7"
        , Parser.succeed EightLarge |. Parser.symbol "8"
        , Parser.succeed NineLarge |. Parser.symbol "9"
        , Parser.succeed ATenHuge |. Parser.symbol "A"
        , Parser.succeed BElevenHuge |. Parser.symbol "B"
        , Parser.succeed CTwelveHuge |. Parser.symbol "C"
        , Parser.succeed SGGSmallGasGiant |. Parser.symbol "SGG"
        , Parser.succeed LGGLargeGasGiant |. Parser.symbol "LGG"
        ]



{- World Atmosphere Classification Codes Table
   As a generality, small worlds have insufficient gravity to maintain an atmosphere and tend to have thin or trace atmospheres while larger worlds grow increasingly dense. Corrosive and insidious worlds tend to be at extreme ends of the tolerable temperature spectrum.

   Atmospheric Code Descriptions
   Symbol    Code    Specific Description    General Description    Pressure (Atm)    Remarks
   ATM-0-T5-Fan-Andy-Bigwood 22-Oct-2019a.jpg    0    Vacuum    Vacuum    < 0.001    Vacuum requires a vacc suit. The atmosphere has a pressure of less than 0.001 atmospheres, which requires the use of a vacc suit.
   ATM-1-T5-Fan-Andy-Bigwood 22-Oct-2019a.jpg    1    Trace    Vacuum    0.001-0.09    The atmosphere has a pressure of less than 0.1 atmospheres, which requires the use of a vacc suit.
   ATM-2-T5-Fan-Andy-Bigwood 22-Oct-2019a.jpg    2    Very Thin / Tainted    Vacuum    0.10-0.42    Very Thin tainted requires a filter respirator combination
   ATM-3-T5-Fan-Andy-Bigwood 22-Oct-2019a.jpg    3    Very Thin    Vacuum    0.10-0.42    Very Thin requires a respirator. The atmosphere has a pressure of 0.1 to 0.42 atmospheres, which requires the use of a respirator to ensure sufficient oxygen.
   ATM-4-T5-Fan-Andy-Bigwood 22-Oct-2019a.jpg    4    Thin / Tainted    Thin    0.43-0.70    Tainted requires a filter mask. The atmosphere contains an unusual taint such as such as disease, a hazardous gas mix, pollutants, or sulfur compounds which requires the use of a filter mask.
   ATM-5-T5-Fan-Andy-Bigwood 22-Oct-2019a.jpg    5    Thin    Thin    0.43-0.70    Shirtsleeve World: No survival gear required. The atmosphere has a pressure of 0 43 to 0.70 atmospheres. The atmosphere is a standard oxygen/nitrogen mix, which is breathable without assistance.
   ATM-6-T5-Fan-Andy-Bigwood 22-Oct-2019a.jpg    6    Standard    Standard    0.71-1.49    Shirtsleeve World: No survival gear required. The atmosphere has a pressure of 0.71 to 1.49 atmospheres. The atmosphere is a standard oxygen/nitrogen mix, which is breathable without assistance.
   ATM-7-T5-Fan-Andy-Bigwood 22-Oct-2019a.jpg    7    Standard / Tainted    Standard    0.71-1.49    Tainted requires a filter mask.
   ATM-8-T5-Fan-Andy-Bigwood 22-Oct-2019a.jpg    8    Dense    Dense    1.50-2.49    Shirtsleeve World: No survival gear required. The atmosphere has a pressure of 1.50 to 2.49 atmospheres The atmosphere is a standard oxygen/nitrogen mix, which is breathable without assistance.
   ATM-9-T5-Fan-Andy-Bigwood 22-Oct-2019a.jpg    9    Dense / Tainted    Dense    1.50-2.49    Tainted requires a filter mask.
   ATM-A-T5-Fan-Andy-Bigwood 22-Oct-2019a.jpg    A (10)    Exotic    Exotic , Conventional    varies    An unusual gas mix which requires the use of oxygen tanks, but protective suits are not needed.
   ATM-B-T5-Fan-Andy-Bigwood 22-Oct-2019a.jpg    B (11)    Corrosive    Exotic , Conventional    varies    A concentrated gas mix or unusual temperature creates a corrosive environment, which requires the use of a Hostile environment suit or vacc suit.
   Atm-C-T5-Fan-Andy-Bigwood 22-Oct-2019a.jpg    C (12)    Insidious    Exotic, Conventional    varies    The atmosphere is similar to a corrosive atmosphere, but extreme conditions cause the corrosive effects to defeat any protective measures in 2 to 12 hours.
   ATM-D-T5-Fan-Andy-Bigwood 22-Oct-2019a.jpg    D (13)    Dense, high    Exotic, Unusual    2.5 or greater [3]    Typically no survival gear required under many conditions. Pressure at or below sea level is too great to support life but is breathable at higher altitudes.
   ATM-E-T5-Fan-Andy-Bigwood 22-Oct-2019a.jpg    E (14) †    Ellipsoid †    Exotic, Unusual    0.5 or less [4]    Typically no survival gear required under many conditions. The world’s surface is ellipsoidal, not spherical. Because the atmosphere remains spherical, surface atmospheric pressure ranges from very high at the middle to very low at the ends. Breathable bands may exist at some point within the range of pressure.
   ATM-F-T5-Fan-Andy-Bigwood 22-Oct-2019a.jpg    F (15) †    Thin, low †    Exotic, Unusual    varies    Typically no survival gear required under some conditions. This world is large and massive, with a thin atmosphere which settles to the lowest levels of the terrain. The atmosphere is un-breathable at most altitudes except the very low ones (…as in depressions or deep valleys).
   † - NOTE: In MgT and T5, Atm type E is "Thin, Low", and Atm type F is "Unusual", which includes (but is not limited to) Ellipsoidal atmospheres.

-}
{-
   Hydrographic Code Table
   Hydrographic Codes
   Symbol    Code    Description    % Surface Water    Remarks
   Hydro-Code-0 Fan-Andy-Bigwood 13-Nov-2019.png    0    Desert World    0 - 5    It is a super arid (anhydrous) environment.
   Hydro-Code-1 Fan-Andy-Bigwood 13-Nov-2019.png    1    Dry World    6 - 15    It is an arid (near anhydrous) environment.
   Hydro-Code-2 Fan-Andy-Bigwood 13-Nov-2019.png    2    Dry World    16 - 25    It is a standard (wet) environment.
   Hydro-Code-3 Fan-Andy-Bigwood 13-Nov-2019.png    3    Wet World    26 - 35    It is a normative (wet) environment.
   Hydro-Code-4 Fan-Andy-Bigwood 13-Nov-2019.png    4    Wet World    36 - 45    It is a normative (wet) environment.
   Hydro-Code-5 Fan-Andy-Bigwood 13-Nov-2019.png    5    Average Wet World    46 - 55    It is a normative (wet) environment.
   Hydro-Code-6 Fan-Andy-Bigwood 13-Nov-2019.png    6    Wet World    56 - 65    It is a normative (wet) environment.
   Hydro-Code-7 Fan-Andy-Bigwood 13-Nov-2019.png    7    Wet World    66 - 75    It is a normative (wet) environment.
   Hydro-Code-8 Fan-Andy-Bigwood 13-Nov-2019.png    8    Very Wet World    76 - 85    It is a standard (wet) environment.
   Hydro-Code-9 Fan-Andy-Bigwood 13-Nov-2019.png    9    Very Wet World    86 - 95    It is a marine (near superhydrous) environment.
   Hydro-Code-A Fan-Andy-Bigwood 13-Nov-2019.png    A    Water World    96 - 100    It is a supermarine (superhydrous) environment.
-}


type Hydrosphere
    = ZeroDesertWorld
    | OneDryWorld
    | TwoDryWorld
    | ThreeWetWorld
    | FourWetWorld
    | FiveAverageWetWorld
    | SixWetWorld
    | SevenWetWorld
    | EightVeryWetWorld
    | NineVeryWetWorld
    | AWaterWorld


hydrosphere : Parser Hydrosphere
hydrosphere =
    Parser.oneOf
        [ Parser.succeed ZeroDesertWorld |. Parser.symbol "0"
        , Parser.succeed OneDryWorld |. Parser.symbol "1"
        , Parser.succeed TwoDryWorld |. Parser.symbol "2"
        , Parser.succeed ThreeWetWorld |. Parser.symbol "3"
        , Parser.succeed FourWetWorld |. Parser.symbol "4"
        , Parser.succeed FiveAverageWetWorld |. Parser.symbol "5"
        , Parser.succeed SixWetWorld |. Parser.symbol "6"
        , Parser.succeed SevenWetWorld |. Parser.symbol "7"
        , Parser.succeed EightVeryWetWorld |. Parser.symbol "8"
        , Parser.succeed NineVeryWetWorld |. Parser.symbol "9"
        , Parser.succeed AWaterWorld |. Parser.symbol "A"
        ]



{- Government codes

   General Government Codes & Descriptions
   Worlds inhabited by Humaniti or non-Human Sophonts similar to Humaniti are generally classified with the following codes:

   General Government Codes & Descriptions
   Symbol    Code    Name    Description    Power Source / Sructure
   Gov-Code-0 Fan-Andy-Bigwood 13-Nov-2019.png    0    No Government Structure    In many cases, tribal, clan or family bonds predominate    Democracy / Anarchy or Confederation
   Gov-Code-1 Fan-Andy-Bigwood 13-Nov-2019.png    1    Company/Corporation    Government by a company managerial elite, citizens are company employees.    Oligarchy / Unitary State
   Gov-Code-2 Fan-Andy-Bigwood 13-Nov-2019.png    2    Participating Democracy    Government by advice and consent of the citizen.    Democracy / Confederation or Federation
   Gov-Code-3 Fan-Andy-Bigwood 13-Nov-2019.png    3    Self-perpetuating Oligarchy    Government by a restricted minority, with little or no input from the masses.    Oligarchy / Unitary State
   Gov-Code-4 Fan-Andy-Bigwood 13-Nov-2019.png    4    Representative Democracy    Government by elected representatives.    Democracy / Federation or Unitary State
   Gov-Code-5 Fan-Andy-Bigwood 13-Nov-2019.png    5    Feudal Technocracy    Government by specific individuals for those who agree to be ruled. Relationships are based on the performance of technical activities which are mutually-beneficial.    Oligarchy / Federation or Unitary State
   Gov-Code-6 Fan-Andy-Bigwood 13-Nov-2019.png    6    Captive Government/Colony    Government by a leadership answerable to an outside group, a colony or conquered area.    Autocracy / Federation
   Gov-Code-7 Fan-Andy-Bigwood 13-Nov-2019.png    7    Balkanization    No central ruling authority exists. Rival governments compete for control.    Anarchy / Confederation
   Gov-Code-8 Fan-Andy-Bigwood 13-Nov-2019.png    8    Civil Service Bureaucracy    Government by agencies employing individuals selected for their expertise.    Oligarchy / Unitary State
   Gov-Code-9 Fan-Andy-Bigwood 13-Nov-2019.png    9    Impersonal Bureaucracy    Government by agencies which are insulated from the governed.    Oligarchy / Unitary State
   Gov-Code-A Fan-Andy-Bigwood 13-Nov-2019.png    A (10)    Charismatic Dictator    Government by a single leader enjoying the confidence of the citizens.    Autocracy / Unitary State
   Gov-Code-B Fan-Andy-Bigwood 13-Nov-2019.png    B (11)    Non-Charismatic Leader    A previous charismatic dictator has been replaced by a leader through normal channels.    Autocracy / Unitary State
   Gov-Code-C Fan-Andy-Bigwood 13-Nov-2019.png    C (12)    Charismatic Oligarchy    Government by a select group, organization, or class enjoying overwhelming confidence of the citizenry.    Oligarchy / Unitary State
   Gov-Code-D Fan-Andy-Bigwood 13-Nov-2019.png    D (13)    Religious Dictatorship    Government by a religious minority which has little regard for the needs of the citizenry.    Autocracy / Unitary State
   Gov-Code-E Fan-Andy-Bigwood 13-Nov-2019.png    E (14)    Religious Autocracy    Government by a single religious leader having absolute power over the citizenry.    Autocracy / Unitary State
   Gov-Code-F Fan-Andy-Bigwood 13-Nov-2019.png    F (15)    Totalitarian Oligarchy    Government by an all-powerful minority which maintains absolute control through widespread coercion and oppression.    Oligarchy / Unitary State

-}


type Government
    = ZeroNoGovernmentStructure
    | OneCompanyCorporation
    | TwoParticipatingDemocracy
    | ThreeSelfPerpetuatingOligarchy
    | FourRepresentativeDemocracy
    | FiveFeudalTechnocracy
    | SixCaptiveGovernmentColony
    | SevenBalkanization
    | EightCivilServiceBureaucracy
    | NineImpersonalBureaucracy
    | ACharismaticDictator
    | BNonCharismaticLeader
    | CCharismaticOligarchy
    | DReligiousDictatorship
    | EReligiousAutocracy
    | FTotalitarianOligarchy


government : Parser Government
government =
    Parser.oneOf
        [ Parser.succeed ZeroNoGovernmentStructure |. Parser.symbol "0"
        , Parser.succeed OneCompanyCorporation |. Parser.symbol "1"
        , Parser.succeed TwoParticipatingDemocracy |. Parser.symbol "2"
        , Parser.succeed ThreeSelfPerpetuatingOligarchy |. Parser.symbol "3"
        , Parser.succeed FourRepresentativeDemocracy |. Parser.symbol "4"
        , Parser.succeed FiveFeudalTechnocracy |. Parser.symbol "5"
        , Parser.succeed SixCaptiveGovernmentColony |. Parser.symbol "6"
        , Parser.succeed SevenBalkanization |. Parser.symbol "7"
        , Parser.succeed EightCivilServiceBureaucracy |. Parser.symbol "8"
        , Parser.succeed NineImpersonalBureaucracy |. Parser.symbol "9"
        , Parser.succeed ACharismaticDictator |. Parser.symbol "A"
        , Parser.succeed BNonCharismaticLeader |. Parser.symbol "B"
        , Parser.succeed CCharismaticOligarchy |. Parser.symbol "C"
        , Parser.succeed DReligiousDictatorship |. Parser.symbol "D"
        , Parser.succeed EReligiousAutocracy |. Parser.symbol "E"
        , Parser.succeed FTotalitarianOligarchy |. Parser.symbol "F"
        ]



{- Law Level Codes
   UWP Code Table
   UWP Law Level Codes
   Code    Description    Prohibited Weapons/Controls (including those of earlier codes)
   0 (Zero)    No Law    No Prohibitions
   1    Low Law    Body Pistols, Explosives, Nuclear Weapons, Poison Gas,
   2    Low Law    Portable Energy Weapons
   3    Low Law    Machine Guns, Automatic Weapons, Flamethrowers, military weapons
   4    Moderate Law    Light Assault Weapons, Submachine guns, All Energy weapons.
   5    Moderate Law    Personal Concealable Firearms, EMP weapons, Radiation weapons, Non-automatic fire Gauss weapons
   6    Moderate Law    All firearms except Shotguns
   7    Moderate Law    Shotguns
   8    High Law    Blade Weapons Controlled, open display of weapons
   9    High Law    weapons outside home
   A (10)    Extreme Law    Weapon possession
   B (11)    Extreme Law    Rigid control of civilian movement
   C (12)    Extreme Law    Unrestricted invasion of privacy
   D (13)    Extreme Law    Paramilitary law enforcement
   E (14)    Extreme Law    Full-fledged police state
   F (15)    Extreme Law    All facets of daily life rigidly controlled
   G (16)    Extreme Law    Severe punishment for petty infractions
   H (17)    Extreme Law    Legalized oppressive practices
   J (18)    Extreme Law    Routinely oppressive and restrictive
-}


type LawLevel
    = ZeroNoLaw
    | OneLowLaw
    | TwoLowLaw
    | ThreeLowLaw
    | FourModerateLaw
    | FiveModerateLaw
    | SixModerateLaw
    | SevenModerateLaw
    | EightHighLaw
    | NineHighLaw
    | AExtremeLaw
    | BExtremeLaw
    | CExtremeLaw
    | DExtremeLaw
    | EExtremeLaw
    | FExtremeLaw
    | GExtremeLaw
    | HExtremeLaw
    | JExtremeLaw


lawLevel : Parser LawLevel
lawLevel =
    Parser.oneOf
        [ Parser.succeed ZeroNoLaw |. Parser.symbol "0"
        , Parser.succeed OneLowLaw |. Parser.symbol "1"
        , Parser.succeed TwoLowLaw |. Parser.symbol "2"
        , Parser.succeed ThreeLowLaw |. Parser.symbol "3"
        , Parser.succeed FourModerateLaw |. Parser.symbol "4"
        , Parser.succeed FiveModerateLaw |. Parser.symbol "5"
        , Parser.succeed SixModerateLaw |. Parser.symbol "6"
        , Parser.succeed SevenModerateLaw |. Parser.symbol "7"
        , Parser.succeed EightHighLaw |. Parser.symbol "8"
        , Parser.succeed NineHighLaw |. Parser.symbol "9"
        , Parser.succeed AExtremeLaw |. Parser.symbol "A"
        , Parser.succeed BExtremeLaw |. Parser.symbol "B"
        , Parser.succeed CExtremeLaw |. Parser.symbol "C"
        , Parser.succeed DExtremeLaw |. Parser.symbol "D"
        , Parser.succeed EExtremeLaw |. Parser.symbol "E"
        , Parser.succeed FExtremeLaw |. Parser.symbol "F"
        , Parser.succeed GExtremeLaw |. Parser.symbol "G"
        , Parser.succeed HExtremeLaw |. Parser.symbol "H"
        , Parser.succeed JExtremeLaw |. Parser.symbol "J"
        ]



{- Tech Level
   Technology Levels
   TL    Era or key development
   0    Neolithic Age
   1    Metal Age
   2    Age of Sail
   3    Industrial Age
   4    Mechanized Age
   5    Broadcast Age
   6    Atomic Age
   7    Space Age
   8    Information Age
   9    Gravitics Age; First Jump Drives
   10    Basic Fusion Age
   11    Fusion Plus Age
   12    Positronics Age
   13    Cloning Age
   14    Geneering Age
   15    Anagathics Age
   16    Artificial Persons Age
   17    Personality Transfer Age
   18    Exotics Age
   19    Antimatter Age
   20    Skip Drive Age
   21    Stasis Age
   22    Planet-scrubber Age
   23    Psychohistory Age
   24    Rosette Age
   25    Psionic Engineering Age
   26    Star Energy Age
   27    Ringworld Age
   28    Reality Engineering Age
   29    Dyson Sphere Age
   30    Remote Technology Age
   31    Pocket Universe Age

-}


type TechLevel
    = ZeroNeolithicAge
    | OneMetalAge
    | TwoAgeOfSail
    | ThreeIndustrialAge
    | FourMechanizedAge
    | FiveBroadcastAge
    | SixAtomicAge
    | SevenSpaceAge
    | EightInformationAge
    | NineGraviticsAge
    | TenBasicFusionAge
    | ElevenFusionPlusAge
    | TwelvePositronicsAge
    | ThirteenCloningAge
    | FourteenGeneeringAge
    | FifteenAnagathicsAge
    | SixteenArtificialPersonsAge
    | SeventeenPersonalityTransferAge
    | EighteenExoticsAge
    | NineteenAntimatterAge
    | TwentySkipDriveAge
    | TwentyOneStasisAge
    | TwentyTwoPlanetScrubberAge
    | TwentyThreePsychohistoryAge
    | TwentyFourRosetteAge
    | TwentyFivePsionicEngineeringAge
    | TwentySixStarEnergyAge
    | TwentySevenRingworldAge
    | TwentyEightRealityEngineeringAge
    | TwentyNineDysonSphereAge
    | ThirtyRemoteTechnologyAge
    | ThirtyOnePocketUniverseAge


techLevel : Parser TechLevel
techLevel =
    Parser.oneOf
        [ Parser.succeed ZeroNeolithicAge |. Parser.symbol "0"
        , Parser.succeed OneMetalAge |. Parser.symbol "1"
        , Parser.succeed TwoAgeOfSail |. Parser.symbol "2"
        , Parser.succeed ThreeIndustrialAge |. Parser.symbol "3"
        , Parser.succeed FourMechanizedAge |. Parser.symbol "4"
        , Parser.succeed FiveBroadcastAge |. Parser.symbol "5"
        , Parser.succeed SixAtomicAge |. Parser.symbol "6"
        , Parser.succeed SevenSpaceAge |. Parser.symbol "7"
        , Parser.succeed EightInformationAge |. Parser.symbol "8"
        , Parser.succeed NineGraviticsAge |. Parser.symbol "9"
        , Parser.succeed TenBasicFusionAge |. Parser.symbol "10"
        , Parser.succeed ElevenFusionPlusAge |. Parser.symbol "11"
        , Parser.succeed TwelvePositronicsAge |. Parser.symbol "12"
        , Parser.succeed ThirteenCloningAge |. Parser.symbol "13"
        , Parser.succeed FourteenGeneeringAge |. Parser.symbol "14"
        , Parser.succeed FifteenAnagathicsAge |. Parser.symbol "15"
        , Parser.succeed SixteenArtificialPersonsAge |. Parser.symbol "16"
        , Parser.succeed SeventeenPersonalityTransferAge |. Parser.symbol "17"
        , Parser.succeed EighteenExoticsAge |. Parser.symbol "18"
        , Parser.succeed NineteenAntimatterAge |. Parser.symbol "19"
        , Parser.succeed TwentySkipDriveAge |. Parser.symbol "20"
        , Parser.succeed TwentyOneStasisAge |. Parser.symbol "21"
        , Parser.succeed TwentyTwoPlanetScrubberAge |. Parser.symbol "22"
        , Parser.succeed TwentyThreePsychohistoryAge |. Parser.symbol "23"
        , Parser.succeed TwentyFourRosetteAge |. Parser.symbol "24"
        , Parser.succeed TwentyFivePsionicEngineeringAge |. Parser.symbol "25"
        , Parser.succeed TwentySixStarEnergyAge |. Parser.symbol "26"
        , Parser.succeed TwentySevenRingworldAge |. Parser.symbol "27"
        , Parser.succeed TwentyEightRealityEngineeringAge |. Parser.symbol "28"
        , Parser.succeed TwentyNineDysonSphereAge |. Parser.symbol "29"
        , Parser.succeed ThirtyRemoteTechnologyAge |. Parser.symbol "30"
        , Parser.succeed ThirtyOnePocketUniverseAge |. Parser.symbol "31"
        ]


type Remark
    = Remark String


{-| Any alpha numeric string
-}
remark : Parser Remark
remark =
    Parser.map Remark <|
        Parser.getChompedString <|
            Parser.succeed ()
                |. Parser.chompIf Char.isAlphaNum
                |. Parser.chompWhile Char.isAlphaNum


type Zone
    = Zone String


{-| Any alpha numeric string
-}
zone : Parser Zone
zone =
    Parser.map Zone <|
        Parser.getChompedString <|
            Parser.succeed ()
                |. Parser.chompWhile Char.isAlphaNum


type alias PBG =
    { populationMultiplier : Int
    , belts : Int
    , gasGiants : Int
    }


parseSingleDigit : (Char -> Bool) -> Parser Int
parseSingleDigit condition =
    Parser.andThen
        (\str ->
            case str of
                Just validInt ->
                    Parser.succeed validInt

                Nothing ->
                    Parser.problem "Not a valid int"
        )
    <|
        Parser.map String.toInt <|
            Parser.getChompedString
                (Parser.succeed () |. Parser.chompIf condition)


pbg : Parser PBG
pbg =
    Parser.succeed (\p b g -> { populationMultiplier = p, belts = b, gasGiants = g })
        |= parseSingleDigit Char.isDigit
        |= parseSingleDigit Char.isDigit
        |= parseSingleDigit Char.isDigit



{- Allegiance codes

   First Survey Common Codes
   1S-Code    Polity    Remarks
   As    Aslan Hierate    The Hierate term is commonly misspelled as "Heirate", though both terms are not incorrect; a hierarchy of clans definitely influences politics within Aslan space, and there is much to do about the legacy of the clan and family, as heirs to past glory.
   Cs    Client State    Are most often the rebellious elements of former Pocket Empires.
   Dr    Droyne World    Known to researchers as the Droyne Oytrip Yatroy.
   Hv    Hive Federation    Sometimes mistakenly called the Hiver Empire or Confederation.
   Im    Imperial    Often simply called the Imperium, despite being the third of its kind.
   Kk    K'kree Two Thousand Worlds empire    Was called the Centaur Empire for a number of years after First Contact.
   Na    Non-Aligned    Independent worlds, mostly World-States are grist for the larger interstellar empires.
   So    Solomani Confederation    The Solomani, once known as the Terrans have had extensive political turnover of governments.
   Zh    Zhodani Consulate    The Zhodani have one of the most stable political systems known to Charted Space.
   Some other codes which have included First Survey standards include:

   Alternate First Survey Common Codes
   1S-Code    Polity    Remarks
   Cw    Chirper World    Some Zhodani star charts use this coding.
   Dr    Droyne World    Droyne world
   Dd    Domain of Deneb    Domain of Deneb
   Fd    Federation of Daibei    Federation of Daibei
   Fi    Federation of Ilelish    Federation of Ilelish
   La    League of Antares    League of Antares
   Li    Lucan's Imperium    Lucan's Imperium
   Ma    Margaret's Stronghold    Margaret's Stronghold
   Rv    Restored Vilani Empire    Restored Ziru Sirka
   St    Strephon's Imperium    Strephon's Imperium
   Wi    Wilds    Unexplored space.
   --    Uncharted Space    Empty, unclaimed system.

-}


type Allegiance
    = AslanHierate
    | ClientState
    | DroyneWorld
    | HiveFederation
    | Imperial
    | KkKkreeTwoThousandWorldsEmpire
    | NonAligned
    | SolomaniConfederation
    | ZhodaniConsulate
    | ChirperWorld
    | DomainOfDeneb
    | FederationOfDaibei
    | FederationOfIlelish
    | LeagueOfAntares
    | LucansImperium
    | MargaretsStronghold
    | RestoredVilaniEmpire
    | StrephonsImperium
    | Wilds
    | UnchartedSpace


allegiance : Parser (Maybe Allegiance)
allegiance =
    Parser.oneOf
        [ Parser.map Just <|
            Parser.oneOf
                [ Parser.succeed AslanHierate |. Parser.symbol "As"
                , Parser.succeed ClientState |. Parser.symbol "Cs"
                , Parser.succeed DroyneWorld |. Parser.symbol "Dr"
                , Parser.succeed HiveFederation |. Parser.symbol "Hv"
                , Parser.succeed Imperial |. Parser.symbol "Im"
                , Parser.succeed KkKkreeTwoThousandWorldsEmpire |. Parser.symbol "Kk"
                , Parser.succeed NonAligned |. Parser.symbol "Na"
                , Parser.succeed SolomaniConfederation |. Parser.symbol "So"
                , Parser.succeed ZhodaniConsulate |. Parser.symbol "Zh"
                , Parser.succeed ChirperWorld |. Parser.symbol "Cw"
                , Parser.succeed DomainOfDeneb |. Parser.symbol "Dd"
                , Parser.succeed FederationOfDaibei |. Parser.symbol "Fd"
                , Parser.succeed FederationOfIlelish |. Parser.symbol "Fi"
                , Parser.succeed LeagueOfAntares |. Parser.symbol "La"
                , Parser.succeed LucansImperium |. Parser.symbol "Li"
                , Parser.succeed MargaretsStronghold |. Parser.symbol "Ma"
                , Parser.succeed RestoredVilaniEmpire |. Parser.symbol "Rv"
                , Parser.succeed StrephonsImperium |. Parser.symbol "St"
                , Parser.succeed Wilds |. Parser.symbol "Wi"
                , Parser.succeed UnchartedSpace |. Parser.symbol "--"
                ]
        , Parser.succeed Nothing |. Parser.symbol ""
        ]



{- Stellar Data

   Stellar Data: The data is provided in 2 parts:

   Morgan-Keenan Spectral Type
   Yerkes Spectral Classification
   Thus, when stellar data is provided in SEC file format as "M0 III A4 D M4 V", that data can be interpreted as:

   M0 III: Luminous Red Giant.
   A4 D: White dwarf star.
   M4 V: Red dwarf star.
   Likewise, you could read "G2 V K2 V" as:

   G2 V: Sol-class "yellow" main-sequence star
   K2 V: Orange-yellow main-sequence star, similar to Beta Centauri.
   Information on Stellar Spectral classifications has been summarized below for convenience.

   Morgan-Keenan Spectral Type
   Class O:: Very hot and very luminous, being bluish in color; in fact, most of their output is in the ultraviolet range. These are the rarest of all main sequence stars, constituting as few as 1 in 3,000,000 in the solar neighborhood O-stars shine with a power over a million times our Sun's output. Because they are so huge, class O stars burn through their hydrogen fuel very quickly, and are the first stars to leave the main sequence.
   Class B:: Extremely luminous and blue. As O and B stars are so powerful, they only live for a very short time, and thus they do not stray far from the area in which they were formed. These stars tend to cluster together in what are called OB associations, which are associated with giant molecular clouds. The Orion OB1 association occupies a large portion of a spiral arm of our galaxy and contains many of the brighter stars of the constellation Orion. They constitute about 1 in 800 main sequence stars in the solar neighborhood.
   Class A:: Amongst the more common naked eye stars, and are white or bluish-white. They comprise about 1 in 160 of the main sequence stars in the solar neighborhood.
   Class F:: These stars' color is white with a slight tinge of yellow. These represent about 1 in 32 of the main sequence stars in the solar neighborhood.
   Class G:: The best known, if only for the reason that Sol (Terra) is of this class. Supergiant stars often swing between O or B (blue) and K or M (red). While they do this, they do not stay for long in the G classification as this is an extremely unstable place for a supergiant to be. G stars represent about 1 in 13 of the main sequence stars in the solar neighborhood.
   Class K:: Orangish stars which are slightly cooler than our Sun. Some K stars are giants and supergiants, such as Arcturus, while others, like Alpha Centauri B, are main sequence stars. These make up 1 in 8 of the main sequence stars in the solar neighborhood.
   Class M:: By far the most common class. At least 80% of the main sequence stars in the solar neighborhood are red dwarfs such as Proxima Centauri. M is also host to most giants and some supergiants such as Antares and Betelgeuse , as well as Mira variables. The late-M group holds hotter brown dwarfs that are above the L spectrum. This is usually in the range of M6.5 to M9.5.
   Yerkes Spectral Classification
   I - VII: These Roman Numeral designations refer to the overall class of star in terms of size.
   0 Hypergiants
   I Supergiants
   Ia-0 Extremely luminous supergiants
   Ia Luminous supergiants, Example: Deneb (spectrum is A2Ia)
   Iab Intermediate luminous supergiants
   Ib Less luminous supergiants; Example: Betelgeuse (spectrum is M2Ib)
   II Bright giants
   III Normal giant stars
   IV Subgiant stars
   V Main sequence stars (dwarfs)
   VI Subdwarf stars (rarely used)
   VII White dwarfs (rarely used)
   Other Classification Codes
   D White Dwarf (rarely used)

-}


type SpectralType
    = SpectralClassO (Maybe Int)
    | SpectralClassB (Maybe Int)
    | SpectralClassA (Maybe Int)
    | SpectralClassF (Maybe Int)
    | SpectralClassG (Maybe Int)
    | SpectralClassK (Maybe Int)
    | SpectralClassM (Maybe Int)
    | SpectralClassY_NonWiki (Maybe Int)


type SpectralClassification
    = SpectralHyperGiant
    | SpectralSuperGiant
    | SpectralSuperGiantExtremelyLuminous
    | SpectralSuperGiantLuminous
    | SpectralSuperGiantIntermediateLuminous
    | SpectralSuperGiantLessLuminous
    | SpectralBrightGiant
    | SpectralNormalGiant
    | SpectralSubGiant
    | SpectralMainSequence
    | SpectralSubDwarf
    | SpectralWhiteDwarf


type StellarData
    = StellarData SpectralType SpectralClassification


stellarData : Parser StellarData
stellarData =
    let
        optionalTemp : Parser (Maybe Int)
        optionalTemp =
            Parser.oneOf
                [ Parser.map Just <| parseSingleDigit Char.isDigit
                , Parser.succeed Nothing |. Parser.symbol ""
                ]

        spectralType =
            [ Parser.succeed SpectralClassO |. Parser.symbol "O" |= optionalTemp
            , Parser.succeed SpectralClassB |. Parser.symbol "B" |= optionalTemp
            , Parser.succeed SpectralClassA |. Parser.symbol "A" |= optionalTemp
            , Parser.succeed SpectralClassF |. Parser.symbol "F" |= optionalTemp
            , Parser.succeed SpectralClassG |. Parser.symbol "G" |= optionalTemp
            , Parser.succeed SpectralClassK |. Parser.symbol "K" |= optionalTemp
            , Parser.succeed SpectralClassM |. Parser.symbol "M" |= optionalTemp
            , Parser.succeed SpectralClassY_NonWiki |. Parser.symbol "Y" |= optionalTemp
            ]

        classification =
            [ Parser.succeed SpectralHyperGiant |. Parser.symbol "0"
            , Parser.succeed SpectralSuperGiant |. Parser.symbol "I"
            , Parser.succeed SpectralSuperGiantExtremelyLuminous |. Parser.symbol "Ia-0"
            , Parser.succeed SpectralSuperGiantLuminous |. Parser.symbol "Ia"
            , Parser.succeed SpectralSuperGiantIntermediateLuminous |. Parser.symbol "Iab"
            , Parser.succeed SpectralSuperGiantLessLuminous |. Parser.symbol "Ib"
            , Parser.succeed SpectralBrightGiant |. Parser.symbol "II"
            , Parser.succeed SpectralNormalGiant |. Parser.symbol "III"
            , Parser.succeed SpectralSubGiant |. Parser.symbol "IV"
            , Parser.succeed SpectralMainSequence |. Parser.symbol "V"
            , Parser.succeed SpectralSubDwarf |. Parser.symbol "VI"
            , Parser.succeed SpectralWhiteDwarf |. Parser.symbol "VII"
            ]
    in
    Parser.succeed identity
        |= Parser.oneOf
            [ Parser.succeed StellarData
                |= Parser.oneOf spectralType
                |. Parser.spaces
                |= Parser.oneOf classification
            ]



{- Important Extension

   The Importance Extension is abbreviated Ix and written in braces ({+/- N}). It is a decimal integer (positive, negative, or zero) ranking the importance of the world within a region.

-}


type ImportanceExtension
    = ImportanceExtension Int


importanceExtension : Parser (Maybe ImportanceExtension)
importanceExtension =
    Parser.succeed identity
        |. Parser.symbol "{"
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.map (Just << ImportanceExtension) Parser.int
            , Parser.succeed Nothing |. Parser.symbol ""
            ]
        |. Parser.spaces
        |. Parser.symbol "}"



{- eHex

   T5 Expanded Hexidecimal System
   Note: The expanded system removes the letters "I" and "O" as they resemble numbers.

   Expanded Hexidecimal System
   #    Symbol    Remarks
   0    0    Standard Indian numeral
   1    1    Standard Arabic numeral
   2    2    Standard Arabic numeral
   3    3    Standard Arabic numeral
   4    4    Standard Arabic numeral
   5    5    Standard Arabic numeral
   6    6    Standard Arabic numeral
   7    7    Standard Arabic numeral
   8    8    Standard Arabic numeral
   9    9    Standard Arabic numeral
   10    A    Original hexidecimal notation from Classic Traveller.
   11    B    Original hexidecimal notation from Classic Traveller.
   12    C    Original hexidecimal notation from Classic Traveller.
   13    D    Original hexidecimal notation from Classic Traveller.
   14    E    Original hexidecimal notation from Classic Traveller.
   15    F    Original hexidecimal notation from Classic Traveller.
   16    G    Original hexidecimal notation from Classic Traveller.
   17    H    Expanded hexidecimal notation from T5.
   I    I    Not included. Too much like a 1 (one).
   18    J    Expanded hexidecimal notation from T5.
   19    K    Expanded hexidecimal notation from T5.
   20    L    Expanded hexidecimal notation from T5.
   21    M    Expanded hexidecimal notation from T5.
   22    N    Expanded hexidecimal notation from T5.
   O    O    Not included. Too much like a 0 (zero).
   23    P    Expanded hexidecimal notation from T5.
   24    Q    Expanded hexidecimal notation from T5.
   25    R    Expanded hexidecimal notation from T5.
   26    S    Expanded hexidecimal notation from T5.
   27    T    Expanded hexidecimal notation from T5.
   28    U    Expanded hexidecimal notation from T5.
   29    V    Expanded hexidecimal notation from T5.
   30    W    Expanded hexidecimal notation from T5.
   31    X    Expanded hexidecimal notation from T5.
   32    Y    Expanded hexidecimal notation from T5.
   33    Z    End of the expanded hexidecimal notation from T5.


-}
{- Economic Extension

   The Economic Extension is abbreviated Ex and written in parentheses ((xxx +/-N)). It describes the strength of a world's economy and provides basic insights into the economy’s structure and capabilities. It is given as three eHex digits representing Resources, Labor and Infrastructure, followed by a decimal integer representing Efficiency written with a leading sign (zero is written as +0).

-}


type EconomicExtension
    = EconomicExtension { resources : EHex, labor : EHex, infrastructure : EHex, efficiency : EHex }


economicExtension : Parser (Maybe EconomicExtension)
economicExtension =
    Parser.succeed identity
        |. Parser.symbol "("
        |= Parser.oneOf
            [ Parser.succeed
                (\res lab infra eff ->
                    Just <|
                        EconomicExtension
                            { resources = res
                            , labor = lab
                            , infrastructure = infra
                            , efficiency = eff
                            }
                )
                |= eHex
                |= eHex
                |= eHex
                |= eHex
            , Parser.succeed Nothing |. Parser.symbol ""
            ]
        |. Parser.symbol ")"



{-
   The Cultural Extension is abbreviated Cx and written in brackets ([xxxx]). It gives insight into the social behavior of the world's population. It is given as four eHex digits representing Homogeneity, Acceptance, Strangeness, and Symbols.


-}


type CulturalExtension
    = CulturalExtension { homogeneity : EHex, acceptance : EHex, strangeness : EHex, symbols : EHex }


culturalExtension : Parser (Maybe CulturalExtension)
culturalExtension =
    Parser.succeed
        identity
        |. Parser.symbol "["
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed
                (\hom acc str sym ->
                    Just <|
                        CulturalExtension
                            { homogeneity = hom
                            , acceptance = acc
                            , strangeness = str
                            , symbols = sym
                            }
                )
                |= eHex
                |= eHex
                |= eHex
                |= eHex
            , Parser.succeed Nothing |. Parser.symbol ""
            ]
        |. Parser.spaces
        |. Parser.symbol "]"


type Nobility
    = Nobility String


nobility : Parser Nobility
nobility =
    Parser.map Nobility <|
        Parser.getChompedString <|
            Parser.succeed ()
                |. Parser.chompWhile Char.isAlphaNum



-- parse =
-- 123


parse =
    -- parseExample
    parseRefereeCsv



-- parseExample : T5Tab


debugLogErrors line deadEnd =
    let
        under =
            List.repeat (deadEnd.col - 1) "_"
                |> String.concat
                |> (\spaces -> spaces ++ "^")

        _ =
            Debug.log ("error\n" ++ String.replace "\t" "," line ++ "\n" ++ under) deadEnd
    in
    "errors, check console"


parseExample : List (Result (List Parser.DeadEnd) T5TabEntry)
parseExample =
    let
        printOutput : ( String, Result (List Parser.DeadEnd) T5TabEntry ) -> Result (List Parser.DeadEnd) T5TabEntry
        printOutput ( line, value ) =
            let
                _ =
                    case value of
                        Err deadEnds ->
                            List.map (debugLogErrors line) deadEnds
                                |> String.concat

                        Ok tabs ->
                            Debug.toString tabs
            in
            value
    in
    sampleRefereeSystemsText
        |> String.replace "\u{000D}\n" "\n"
        |> String.split "\n"
        |> --drop header
           List.drop 1
        |> List.map (\line -> ( line, Parser.run t5TabEntry line ))
        |> List.map printOutput


parseRefereeCsv : List (Result (List Parser.DeadEnd) RefereeData)
parseRefereeCsv =
    let
        printOutput : ( String, Result (List Parser.DeadEnd) RefereeData ) -> Result (List Parser.DeadEnd) RefereeData
        printOutput ( line, value ) =
            let
                _ =
                    case value of
                        Err deadEnds ->
                            List.map (debugLogErrors line) deadEnds
                                |> String.concat

                        Ok refData ->
                            Debug.toString <| Debug.log "ref data" refData
            in
            value
    in
    sampleRefereeText
        |> String.replace "\u{000D}\n" "\n"
        |> String.split "\n"
        |> -- drop header
           List.drop 1
        |> List.map (\line -> ( line, Parser.run parseRefereeData line ))
        |> List.map printOutput



-- |> List.map (Result.withDefault emptyTabEntry)


type alias T5TabEntry =
    { hexId : HexId
    , name : String
    , uwp : UWP
    , bases : List Base
    , remarks : List Remark
    , zone : Zone
    , pbg : PBG
    , allegiance : Maybe Allegiance
    , stars : List StellarData
    , ix : Maybe ImportanceExtension
    , ex : Maybe EconomicExtension
    , cx : Maybe CulturalExtension
    , nobility : Nobility
    , numWorlds : Int
    }


singleTab : Parser ()
singleTab =
    Parser.succeed ()
        |. Parser.symbol "\t"


t5TabEntry : Parser T5TabEntry
t5TabEntry =
    Parser.succeed
        (\hex_ name uwp_ bases remarks zone_ pbg_ allegiance_ stars ix ex cx nobility_ numWorlds ->
            { hexId = hex_
            , name = name
            , uwp = uwp_
            , bases = bases
            , remarks = remarks
            , zone = zone_
            , pbg = pbg_
            , allegiance = allegiance_
            , stars = stars
            , ix = ix
            , ex = ex
            , cx = cx
            , nobility = nobility_
            , numWorlds = numWorlds
            }
        )
        |= -- hex
           hexId
        |. singleTab
        |= -- name
           (Parser.getChompedString <| Parser.chompWhile (\c -> c /= '\t'))
        |. singleTab
        |= --uwp
           uwp
        |. singleTab
        |= -- bases
           Parser.many base
        |. singleTab
        |= --remarks
           Parser.many remark
        |. singleTab
        |= --zone
           zone
        |. singleTab
        |= -- PBG
           pbg
        |. singleTab
        |= -- allegiance
           allegiance
        |. singleTab
        |= -- stellar data
           Parser.many stellarData
        |. singleTab
        |= --ix
           importanceExtension
        |. singleTab
        |= -- ex
           economicExtension
        |. singleTab
        |= -- cx
           culturalExtension
        |. singleTab
        |= --nobility
           nobility
        |. singleTab
        |= -- worlds
           Parser.int


sampleRefereeSystemsText : String
sampleRefereeSystemsText =
    """
Hex\tName\tUWP\tBases\tRemarks\tZone\tPBG\tAllegiance\tStars\t{Ix}\t(Ex)\t[Cx]\tNobility\tW
0606\tXAA6000-0\tXAA6000-0\t\t\t\t004\t\tK3 V M8 V \t{}\t()\t[]\t\t5
0608\tX525000-0\tX525000-0\t\t\t\t004\t\tM1 V \t{}\t()\t[]\t\t5
1007\tX48A000-0\tX48A000-0\t\t\t\t003\t\tM1 V \t{}\t()\t[]\t\t4
1206\tX448000-0\tX448000-0\t\t\t\t003\t\tM1 V Y  \t{}\t()\t[]\t\t7
1301\tX9A7000-0\tX9A7000-0\t\t\t\t002\t\tK9 V M7 V \t{}\t()\t[]\t\t4
1309\tXDHA000-0\tXDHA000-0\t\t\t\t013\t\tM2 V \t{}\t()\t[]\t\t12
2003\tX436000-0\tX436000-0\t\t\t\t022\t\tM5 V \t{}\t()\t[]\t\t4
3103\tXAC9000-0\tXAC9000-0\t\t\t\t010\t\tG7 V \t{}\t()\t[]\t\t9
3202\tX6A7000-0\tX6A7000-0\t\t\t\t001\t\tA7 V F1 V \t{}\t()\t[]\t\t5
0119\tX7C8000-0\tX7C8000-0\t\t\t\t003\t\tA8 V F9 V M9 V \t{}\t()\t[]\t\t11
1111\tX987000-0\tX987000-0\t\t\t\t004\t\tM V \t{}\t()\t[]\t\t5
1117\tX896000-0\tX896000-0\t\t\t\t004\t\tA2 V K2 V A7 V \t{}\t()\t[]\t\t6
1220\tX100000-0\tX100000-0\t\t\t\t026\t\tM1 V \t{}\t()\t[]\t\t3
1414\tX9CA000-0\tX9CA000-0\t\t\t\t000\t\tF9 V \t{}\t()\t[]\t\t5
1911\tX452000-0\tX452000-0\t\t\t\t012\t\tM4 V L5  \t{}\t()\t[]\t\t4
2016\tX000000-0\tX000000-0\t\t\t\t014\t\tM5 V \t{}\t()\t[]\t\t3
2120\tX000000-0\tX000000-0\t\t\t\t026\t\tM8 V \t{}\t()\t[]\t\t3
2414\tX200000-0\tX200000-0\t\t\t\t003\t\tK9 V \t{}\t()\t[]\t\t6
3217\tX66A000-0\tX66A000-0\t\t\t\t024\t\tK3 V K3 V \t{}\t()\t[]\t\t5
3218\tX7A6000-0\tX7A6000-0\t\t\t\t003\t\tK2 V K2 V \t{}\t()\t[]\t\t4
0122\tXD98000-0\tXD98000-0\t\t\t\t022\t\tG3 V \t{}\t()\t[]\t\t4
0126\tX8C5000-0\tX8C5000-0\t\t\t\t032\t\tG6 V \t{}\t()\t[]\t\t8
0221\tX436000-0\tX436000-0\t\t\t\t004\t\tM5 V M7 V \t{}\t()\t[]\t\t6
0224\tXBA3000-0\tXBA3000-0\t\t\t\t002\t\tG4 V \t{}\t()\t[]\t\t5
0225\tXB88000-0\tXB88000-0\t\t\t\t001\t\tA6 IV \t{}\t()\t[]\t\t5
0426\tX8A6000-0\tX8A6000-0\t\t\t\t021\t\tG5 V \t{}\t()\t[]\t\t5
0529\tX655000-0\tX655000-0\t\t\t\t003\t\tG2 V \t{}\t()\t[]\t\t3
0927\tX522000-0\tX522000-0\t\t\t\t001\t\tK3 V \t{}\t()\t[]\t\t11
1023\tX100000-0\tX100000-0\t\t\t\t021\t\tK8 V D  \t{}\t()\t[]\t\t9
1026\tX8A7000-0\tX8A7000-0\t\t\t\t001\t\tF6 V F6 V \t{}\t()\t[]\t\t7
1121\tX658000-0\tX658000-0\t\t\t\t003\t\tM2 V \t{}\t()\t[]\t\t5
1122\tX000000-0\tX000000-0\t\t\t\t023\t\tK5 V K9 V D  \t{}\t()\t[]\t\t4
1123\tX9DA000-0\tX9DA000-0\t\t\t\t020\t\tM2 V \t{}\t()\t[]\t\t8
1223\tX000000-0\tX000000-0\t\t\t\t022\t\tK9 V \t{}\t()\t[]\t\t9
1224\tX656000-0\tX656000-0\t\t\t\t010\t\tK9 V \t{}\t()\t[]\t\t5
1327\tX965000-0\tX965000-0\t\t\t\t006\t\tK6 V \t{}\t()\t[]\t\t9
1329\tX67A000-0\tX67A000-0\t\t\t\t020\t\tM V M8 V \t{}\t()\t[]\t\t7
1421\tXBA9000-0\tXBA9000-0\t\t\t\t013\t\tM2 V \t{}\t()\t[]\t\t5
1524\tX100000-0\tX100000-0\t\t\t\t023\t\tG2 V D  M3 V \t{}\t()\t[]\t\t3
1530\tXBB9000-0\tXBB9000-0\t\t\t\t000\t\tK6 V D  \t{}\t()\t[]\t\t6
1623\tX000000-0\tX000000-0\t\t\t\t021\t\tG7 IV K2 IV \t{}\t()\t[]\t\t7
1725\tX67A000-0\tX67A000-0\t\t\t\t004\t\tG8 V \t{}\t()\t[]\t\t4
1727\tX886000-0\tX886000-0\t\t\t\t024\t\tK1 V \t{}\t()\t[]\t\t6
1730\tX000000-0\tX000000-0\t\t\t\t020\t\tM4 V \t{}\t()\t[]\t\t5
1826\tX777000-0\tX777000-0\t\t\t\t013\t\tK7 V \t{}\t()\t[]\t\t5
1828\tXS00000-0\tXS00000-0\t\t\t\t021\t\tG6 V \t{}\t()\t[]\t\t6
1921\tXCC9000-0\tXCC9000-0\t\t\t\t004\t\tK7 V M6 V \t{}\t()\t[]\t\t11
1929\tX536000-0\tX536000-0\t\t\t\t002\t\tM3 V \t{}\t()\t[]\t\t5
2023\tX100000-0\tX100000-0\t\t\t\t002\t\tF8 V G4 V \t{}\t()\t[]\t\t4
2024\tX000000-0\tX000000-0\t\t\t\t022\t\tF7 V G7 V \t{}\t()\t[]\t\t8
2029\tXBE9000-0\tXBE9000-0\t\t\t\t002\t\tM2 V \t{}\t()\t[]\t\t4
2121\tXACA000-0\tXACA000-0\t\t\t\t012\t\tM4 V \t{}\t()\t[]\t\t3
2126\tXBA9000-0\tXBA9000-0\t\t\t\t004\t\tG1 V M6 V \t{}\t()\t[]\t\t5
2127\tX100000-0\tX100000-0\t\t\t\t023\t\tK V \t{}\t()\t[]\t\t7
2130\tXACA000-0\tXACA000-0\t\t\t\t004\t\tK V \t{}\t()\t[]\t\t10
2228\tX5A6000-0\tX5A6000-0\t\t\t\t004\t\tM5 V M6 V \t{}\t()\t[]\t\t7
2321\tX775000-0\tX775000-0\t\t\t\t002\t\tM1 V \t{}\t()\t[]\t\t4
2325\tX9EA000-0\tX9EA000-0\t\t\t\t000\t\tM2 V T  \t{}\t()\t[]\t\t8
2330\tXAB8000-0\tXAB8000-0\t\t\t\t003\t\tG4 V K8 V \t{}\t()\t[]\t\t7
2421\tX6A7000-0\tX6A7000-0\t\t\t\t013\t\tA8 V K6 V \t{}\t()\t[]\t\t4
2426\tX000000-0\tX000000-0\t\t\t\t012\t\tK9 V \t{}\t()\t[]\t\t3
2526\tX100000-0\tX100000-0\t\t\t\t003\t\tM V \t{}\t()\t[]\t\t5
2626\tX5A6000-0\tX5A6000-0\t\t\t\t001\t\tM4 V M5 V Y  \t{}\t()\t[]\t\t7
2723\tX567000-0\tX567000-0\t\t\t\t004\t\tK7 V \t{}\t()\t[]\t\t4
2725\tX353000-0\tX353000-0\t\t\t\t001\t\tG V K2 V \t{}\t()\t[]\t\t5
2726\tX330000-0\tX330000-0\t\t\t\t003\t\tM7 V \t{}\t()\t[]\t\t5
2727\tX575000-0\tX575000-0\t\t\t\t000\t\tM3 V \t{}\t()\t[]\t\t6
2728\tXCC6000-0\tXCC6000-0\t\t\t\t000\t\tM1 V \t{}\t()\t[]\t\t3
2729\tX100000-0\tX100000-0\t\t\t\t013\t\tA5 V K1 V \t{}\t()\t[]\t\t6
2822\tX785000-0\tX785000-0\t\t\t\t022\t\tF9 IV \t{}\t()\t[]\t\t7
2825\tX432000-0\tX432000-0\t\t\t\t000\t\tK8 V \t{}\t()\t[]\t\t5
2826\tX3AA000-0\tX3AA000-0\t\t\t\t001\t\tG7 V K6 V K7 V \t{}\t()\t[]\t\t6
2827\tXEGA000-0\tXEGA000-0\t\t\t\t022\t\tK V M6 V \t{}\t()\t[]\t\t7
2829\tX757000-0\tX757000-0\t\t\t\t001\t\tK6 V \t{}\t()\t[]\t\t4
2922\tXCDA000-0\tXCDA000-0\t\t\t\t000\t\tA5 V A5 V \t{}\t()\t[]\t\t7
2926\tX976000-0\tX976000-0\t\t\t\t003\t\tK1 V \t{}\t()\t[]\t\t8
2927\tX57A000-0\tX57A000-0\t\t\t\t000\t\tM3 VI \t{}\t()\t[]\t\t8
3023\tX666000-0\tX666000-0\t\t\t\t001\t\tG4 V G7 V \t{}\t()\t[]\t\t5
3027\tX734000-0\tX734000-0\t\t\t\t003\t\tG2 V \t{}\t()\t[]\t\t8
3029\tX675000-0\tX675000-0\t\t\t\t002\t\tM6 V \t{}\t()\t[]\t\t7
3030\tXCB6000-0\tXCB6000-0\t\t\t\t023\t\tG3 V \t{}\t()\t[]\t\t7
3122\tX7B5000-0\tX7B5000-0\t\t\t\t003\t\tG8 V \t{}\t()\t[]\t\t8
3129\tX997000-0\tX997000-0\t\t\t\t005\t\tK2 V \t{}\t()\t[]\t\t11
3130\tX79A000-0\tX79A000-0\t\t\t\t010\t\tM V \t{}\t()\t[]\t\t7
3228\tX7A4000-0\tX7A4000-0\t\t\t\t001\t\tM4 V \t{}\t()\t[]\t\t3
0132\tXB99000-0\tXB99000-0\t\t\t\t003\t\tK5 V \t{}\t()\t[]\t\t9
0134\tX100000-0\tX100000-0\t\t\t\t004\t\tM4 V \t{}\t()\t[]\t\t7
0135\tX200000-0\tX200000-0\t\t\t\t000\t\tK5 V \t{}\t()\t[]\t\t7
0138\tX8B6000-0\tX8B6000-0\t\t\t\t015\t\tM2 V \t{}\t()\t[]\t\t9
0231\tX000000-0\tX000000-0\t\t\t\t022\t\tM V M1 V \t{}\t()\t[]\t\t4
0233\tX763000-0\tX763000-0\t\t\t\t000\t\tK4 V \t{}\t()\t[]\t\t4
0239\tX9C4000-0\tX9C4000-0\t\t\t\t004\t\tM5 V \t{}\t()\t[]\t\t4
0331\tX88A000-0\tX88A000-0\t\t\t\t023\t\tM8 V \t{}\t()\t[]\t\t3
0336\tXS00000-0\tXS00000-0\t\t\t\t021\t\tA9 V \t{}\t()\t[]\t\t3
0337\tX888000-0\tX888000-0\t\t\t\t020\t\tM4 V \t{}\t()\t[]\t\t10
0431\tXA8A000-0\tXA8A000-0\t\t\t\t002\t\tK8 V \t{}\t()\t[]\t\t8
0433\tX200000-0\tX200000-0\t\t\t\t010\t\tK9 V M1 V K9 V \t{}\t()\t[]\t\t5
0531\tX732000-0\tX732000-0\t\t\t\t004\t\tM7 V \t{}\t()\t[]\t\t9
0533\tX5A4000-0\tX5A4000-0\t\t\t\t000\t\tM3 V \t{}\t()\t[]\t\t8
0535\tX77A000-0\tX77A000-0\t\t\t\t014\t\tG6 V K5 V K8 V \t{}\t()\t[]\t\t4
0538\tX9D9000-0\tX9D9000-0\t\t\t\t002\t\tK8 V M5 V \t{}\t()\t[]\t\t8
0540\tX688000-0\tX688000-0\t\t\t\t012\t\tF8 III G9 V \t{}\t()\t[]\t\t9
0632\tX000000-0\tX000000-0\t\t\t\t012\t\tM2 V L5  \t{}\t()\t[]\t\t6
0634\tX635000-0\tX635000-0\t\t\t\t023\t\tG8 V \t{}\t()\t[]\t\t5
0640\tX3A5000-0\tX3A5000-0\t\t\t\t004\t\tG5 V \t{}\t()\t[]\t\t4
0732\tXAA4000-0\tXAA4000-0\t\t\t\t003\t\tK9 V \t{}\t()\t[]\t\t12
0736\tX552000-0\tX552000-0\t\t\t\t020\t\tM6 V \t{}\t()\t[]\t\t9
0831\tX651000-0\tX651000-0\t\t\t\t023\t\tG1 V \t{}\t()\t[]\t\t5
0832\tX68A000-0\tX68A000-0\t\t\t\t022\t\tK7 V \t{}\t()\t[]\t\t7
0838\tX742000-0\tX742000-0\t\t\t\t004\t\tM5 V \t{}\t()\t[]\t\t7
0840\tX200000-0\tX200000-0\t\t\t\t032\t\tA7 V F1 V \t{}\t()\t[]\t\t3
0933\tX525000-0\tX525000-0\t\t\t\t003\t\tM7 V \t{}\t()\t[]\t\t5
0934\tXCF7000-0\tXCF7000-0\t\t\t\t003\t\tK6 V K8 V \t{}\t()\t[]\t\t5
0935\tXECA000-0\tXECA000-0\t\t\t\t003\t\tK7 V \t{}\t()\t[]\t\t3
0937\tX9DA000-0\tX9DA000-0\t\t\t\t021\t\tK4 V K6 V \t{}\t()\t[]\t\t6
0939\tXS00000-0\tXS00000-0\t\t\t\t023\t\tK9 V \t{}\t()\t[]\t\t7
1031\tXEHA000-0\tXEHA000-0\t\t\t\t000\t\tM2 V T5  \t{}\t()\t[]\t\t5
1032\tXC98000-0\tXC98000-0\t\t\t\t023\t\tM1 V \t{}\t()\t[]\t\t8
1033\tX556000-0\tX556000-0\t\t\t\t012\t\tM V \t{}\t()\t[]\t\t8
1038\tX331000-0\tX331000-0\t\t\t\t000\t\tM7 V \t{}\t()\t[]\t\t5
1039\tXEB9000-0\tXEB9000-0\t\t\t\t014\t\tA1 V F2 V A7 V \t{}\t()\t[]\t\t5
1040\tXEA7000-0\tXEA7000-0\t\t\t\t000\t\tK8 V \t{}\t()\t[]\t\t6
1131\tX587000-0\tX587000-0\t\t\t\t000\t\tM V M3 V \t{}\t()\t[]\t\t7
1132\tX200000-0\tX200000-0\t\t\t\t021\t\tM2 V Y  \t{}\t()\t[]\t\t8
1136\tX000000-0\tX000000-0\t\t\t\t023\t\tK4 V \t{}\t()\t[]\t\t3
1139\tX668000-0\tX668000-0\t\t\t\t024\t\tM2 V \t{}\t()\t[]\t\t10
1231\tX866000-0\tX866000-0\t\t\t\t004\t\tG8 V K8 V \t{}\t()\t[]\t\t8
1233\tX7A9000-0\tX7A9000-0\t\t\t\t000\t\tK V M1 V \t{}\t()\t[]\t\t4
1234\tX8A6000-0\tX8A6000-0\t\t\t\t004\t\tG4 V K9 V \t{}\t()\t[]\t\t5
1238\tXCC6000-0\tXCC6000-0\t\t\t\t004\t\tM V \t{}\t()\t[]\t\t5
1333\tX9CA000-0\tX9CA000-0\t\t\t\t004\t\tM2 V L5  \t{}\t()\t[]\t\t9
1335\tXAA7000-0\tXAA7000-0\t\t\t\t003\t\tK4 V \t{}\t()\t[]\t\t12
1337\tX100000-0\tX100000-0\t\t\t\t012\t\tF6 V G1 V \t{}\t()\t[]\t\t9
1338\tX8A5000-0\tX8A5000-0\t\t\t\t014\t\tK6 V \t{}\t()\t[]\t\t7
1431\tX3A4000-0\tX3A4000-0\t\t\t\t001\t\tG9 V \t{}\t()\t[]\t\t5
1433\tX421000-0\tX421000-0\t\t\t\t021\t\tK3 V \t{}\t()\t[]\t\t4
1434\tXCDA000-0\tXCDA000-0\t\t\t\t004\t\tM1 V \t{}\t()\t[]\t\t4
1436\tXAC8000-0\tXAC8000-0\t\t\t\t022\t\tG8 V G8 V \t{}\t()\t[]\t\t9
1439\tX7A4000-0\tX7A4000-0\t\t\t\t023\t\tK5 V M3 V \t{}\t()\t[]\t\t7
1440\tXCE9000-0\tXCE9000-0\t\t\t\t023\t\tK6 V \t{}\t()\t[]\t\t8
1531\tX5A1000-0\tX5A1000-0\t\t\t\t004\t\tK4 V \t{}\t()\t[]\t\t4
1535\tX100000-0\tX100000-0\t\t\t\t020\t\tA5 V L5  \t{}\t()\t[]\t\t6
1536\tX5A7000-0\tX5A7000-0\t\t\t\t003\t\tM1 V \t{}\t()\t[]\t\t4
1537\tXDGA000-0\tXDGA000-0\t\t\t\t000\t\tG9 VI \t{}\t()\t[]\t\t6
1539\tXDDA000-0\tXDDA000-0\t\t\t\t004\t\tM1 V \t{}\t()\t[]\t\t4
1637\tX610000-0\tX610000-0\t\t\t\t020\t\tK3 V \t{}\t()\t[]\t\t10
1639\tX368000-0\tX368000-0\t\t\t\t000\t\tM4 V \t{}\t()\t[]\t\t7
1734\tXB88000-0\tXB88000-0\t\t\t\t002\t\tK3 V M6 V \t{}\t()\t[]\t\t5
1736\tX984000-0\tX984000-0\t\t\t\t002\t\tG V Y  \t{}\t()\t[]\t\t6
1737\tX100000-0\tX100000-0\t\t\t\t020\t\tM1 V T  D  \t{}\t()\t[]\t\t4
1738\tX200000-0\tX200000-0\t\t\t\t002\t\tG2 V G8 V \t{}\t()\t[]\t\t7
1832\tX310000-0\tX310000-0\t\t\t\t020\t\tM V \t{}\t()\t[]\t\t6
1836\tXCBA000-0\tXCBA000-0\t\t\t\t004\t\tG V \t{}\t()\t[]\t\t5
1840\tX200000-0\tX200000-0\t\t\t\t025\t\tM1 V \t{}\t()\t[]\t\t6
1931\tXS00000-0\tXS00000-0\t\t\t\t032\t\tF4 V G5 V M1 V \t{}\t()\t[]\t\t6
1932\tX7B6000-0\tX7B6000-0\t\t\t\t002\t\tM V T5  \t{}\t()\t[]\t\t7
1933\tX6AA000-0\tX6AA000-0\t\t\t\t022\t\tM2 V Y  \t{}\t()\t[]\t\t7
1934\tXCC9000-0\tXCC9000-0\t\t\t\t002\t\tK5 V D  \t{}\t()\t[]\t\t7
1935\tXCEA000-0\tXCEA000-0\t\t\t\t010\t\tG4 V K8 V \t{}\t()\t[]\t\t4
1936\tX588000-0\tX588000-0\t\t\t\t003\t\tM3 V \t{}\t()\t[]\t\t8
1938\tX536000-0\tX536000-0\t\t\t\t003\t\tK7 V M6 V \t{}\t()\t[]\t\t8
1940\tXBA8000-0\tXBA8000-0\t\t\t\t024\t\tM4 V \t{}\t()\t[]\t\t11
2031\tX633000-0\tX633000-0\t\t\t\t004\t\tM1 V \t{}\t()\t[]\t\t7
2033\tX999000-0\tX999000-0\t\t\t\t004\t\tF7 V \t{}\t()\t[]\t\t4
2034\tX412000-0\tX412000-0\t\t\t\t003\t\tK7 V M9 V \t{}\t()\t[]\t\t7
2037\tX000000-0\tX000000-0\t\t\t\t023\t\tK1 V M9 V \t{}\t()\t[]\t\t3
2040\tX79A000-0\tX79A000-0\t\t\t\t010\t\tK5 V M9 V M5 V \t{}\t()\t[]\t\t5
2132\tXB9A000-0\tXB9A000-0\t\t\t\t013\t\tK6 V \t{}\t()\t[]\t\t3
2133\tX000000-0\tX000000-0\t\t\t\t021\t\tM3 V \t{}\t()\t[]\t\t5
2134\tX986000-0\tX986000-0\t\t\t\t014\t\tG1 V \t{}\t()\t[]\t\t6
2137\tXCD5000-0\tXCD5000-0\t\t\t\t001\t\tK7 V \t{}\t()\t[]\t\t6
2139\tXAA9000-0\tXAA9000-0\t\t\t\t023\t\tF9 V G5 V \t{}\t()\t[]\t\t5
2231\tX100000-0\tX100000-0\t\t\t\t012\t\tM4 V \t{}\t()\t[]\t\t6
2238\tX100000-0\tX100000-0\t\t\t\t003\t\tM2 V \t{}\t()\t[]\t\t4
2239\tX500000-0\tX500000-0\t\t\t\t000\t\tK9 V \t{}\t()\t[]\t\t6
2331\tXA75000-0\tXA75000-0\t\t\t\t004\t\tM4 V \t{}\t()\t[]\t\t7
2337\tX322000-0\tX322000-0\t\t\t\t000\t\tG6 V \t{}\t()\t[]\t\t3
2340\tX899000-0\tX899000-0\t\t\t\t004\t\tM9 V \t{}\t()\t[]\t\t6
2434\tX000000-0\tX000000-0\t\t\t\t024\t\tM V Y5  \t{}\t()\t[]\t\t3
2436\tX9E7000-0\tX9E7000-0\t\t\t\t000\t\tK1 V M9 V \t{}\t()\t[]\t\t11
2437\tX99A000-0\tX99A000-0\t\t\t\t014\t\tM1 V \t{}\t()\t[]\t\t6
2439\tX200000-0\tX200000-0\t\t\t\t032\t\tB5 V B5 V A3 V \t{}\t()\t[]\t\t5
2440\tXAD6000-0\tXAD6000-0\t\t\t\t003\t\tM7 V \t{}\t()\t[]\t\t6
2531\tX000000-0\tX000000-0\t\t\t\t021\t\tK9 V M9 V \t{}\t()\t[]\t\t11
2533\tX642000-0\tX642000-0\t\t\t\t016\t\tM4 V \t{}\t()\t[]\t\t6
2536\tXBA7000-0\tXBA7000-0\t\t\t\t004\t\tG3 V Y  \t{}\t()\t[]\t\t8
2537\tX412000-0\tX412000-0\t\t\t\t000\t\tK9 V \t{}\t()\t[]\t\t5
2538\tX852000-0\tX852000-0\t\t\t\t001\t\tM4 V \t{}\t()\t[]\t\t7
2539\tX655000-0\tX655000-0\t\t\t\t002\t\tG V M2 V \t{}\t()\t[]\t\t5
2540\tX442000-0\tX442000-0\t\t\t\t000\t\tM1 V \t{}\t()\t[]\t\t6
2631\tX553000-0\tX553000-0\t\t\t\t020\t\tG2 V \t{}\t()\t[]\t\t6
2632\tX666000-0\tX666000-0\t\t\t\t000\t\tM2 V M2 V \t{}\t()\t[]\t\t6
2633\tX46A000-0\tX46A000-0\t\t\t\t002\t\tG5 V \t{}\t()\t[]\t\t11
2637\tX200000-0\tX200000-0\t\t\t\t003\t\tM2 V \t{}\t()\t[]\t\t9
2731\tX864000-0\tX864000-0\t\t\t\t010\t\tK7 V \t{}\t()\t[]\t\t5
2732\tX8A1000-0\tX8A1000-0\t\t\t\t001\t\tM V T  \t{}\t()\t[]\t\t3
2734\tX75A000-0\tX75A000-0\t\t\t\t024\t\tG6 V M5 V K5 V \t{}\t()\t[]\t\t4
2736\tX727000-0\tX727000-0\t\t\t\t010\t\tM4 V \t{}\t()\t[]\t\t12
2737\tXAB9000-0\tXAB9000-0\t\t\t\t010\t\tF8 V G5 V \t{}\t()\t[]\t\t10
2740\tX986000-0\tX986000-0\t\t\t\t004\t\tK V K2 V \t{}\t()\t[]\t\t6
2832\tX6A7000-0\tX6A7000-0\t\t\t\t024\t\tK9 V M8 V \t{}\t()\t[]\t\t10
2835\tX100000-0\tX100000-0\t\t\t\t021\t\tG9 V M6 V \t{}\t()\t[]\t\t3
2836\tXAB5000-0\tXAB5000-0\t\t\t\t005\t\tK3 V M7 V \t{}\t()\t[]\t\t5
2839\tXDDA000-0\tXDDA000-0\t\t\t\t012\t\tM V \t{}\t()\t[]\t\t11
2934\tX200000-0\tX200000-0\t\t\t\t003\t\tM2 V \t{}\t()\t[]\t\t7
2935\tX310000-0\tX310000-0\t\t\t\t004\t\tM2 V \t{}\t()\t[]\t\t8
2937\tX510000-0\tX510000-0\t\t\t\t003\t\tM6 V \t{}\t()\t[]\t\t8
2940\tX530000-0\tX530000-0\t\t\t\t023\t\tM4 V Y  \t{}\t()\t[]\t\t11
3031\tX000000-0\tX000000-0\t\t\t\t013\t\tG2 V K3 V \t{}\t()\t[]\t\t7
3033\tX333000-0\tX333000-0\t\t\t\t024\t\tM2 V \t{}\t()\t[]\t\t5
3037\tX645000-0\tX645000-0\t\t\t\t004\t\tK6 V \t{}\t()\t[]\t\t5
3133\tXS00000-0\tXS00000-0\t\t\t\t013\t\tA4 V F3 V F7 V F4 V \t{}\t()\t[]\t\t4
3134\tX336000-0\tX336000-0\t\t\t\t000\t\tK8 V M2 V \t{}\t()\t[]\t\t10
3136\tX000000-0\tX000000-0\t\t\t\t011\t\tG4 V \t{}\t()\t[]\t\t5
3231\tX796000-0\tX796000-0\t\t\t\t033\t\tM5 V \t{}\t()\t[]\t\t9
3233\tX7A4000-0\tX7A4000-0\t\t\t\t023\t\tG2 V G8 V G2 V \t{}\t()\t[]\t\t10
3234\tXBFA000-0\tXBFA000-0\t\t\t\t001\t\tK6 V K6 V \t{}\t()\t[]\t\t5
3236\tXBB7000-0\tXBB7000-0\t\t\t\t004\t\tK8 V \t{}\t()\t[]\t\t9
3238\tX000000-0\tX000000-0\t\t\t\t032\t\tG4 V \t{}\t()\t[]\t\t5
"""


type alias RefereeData =
    { hex : HexId
    , name : String
    , scanPoints : Int
    , bioComplexity : String
    , nativeSophont : Bool
    , extinctSophont : Bool
    , compatibility : Int
    , resources : Int
    , habitability : Int
    }


parseBool : Parser Bool
parseBool =
    Parser.oneOf
        [ Parser.succeed True |. Parser.symbol "true"
        , Parser.succeed False |. Parser.symbol "false"
        ]


parseRefereeData : Parser RefereeData
parseRefereeData =
    Parser.succeed RefereeData
        |= --hex id
           hexId
        |. Parser.symbol ","
        |= -- name
           Parser.getChompedString (Parser.chompUntil ",")
        |. Parser.symbol ","
        |= -- scan points
           Parser.int
        |. Parser.symbol ","
        |= -- bio complexity
           Parser.getChompedString (Parser.chompUntil ",")
        |. Parser.symbol ","
        |= -- native sophont
           parseBool
        |. Parser.symbol ","
        |= --exinct sophont
           parseBool
        |. Parser.symbol ","
        |= -- compatibility
           Parser.int
        |. Parser.symbol ","
        |= -- resources
           Parser.int
        |. Parser.symbol ","
        |= -- habitability
           Parser.int


sampleRefereeText : String
sampleRefereeText =
    """
Hex,Name,Scan Points,bio complexity,Native Sophont,Extinct Sophont,compatibility,resources,habitability
0606,,7,Differentiated multicellular organisms,false,false,6,14,0
0608,,4,Primitive single-cell organisms,false,false,9,13,5
1007,,3,Primitive single-cell organisms,false,false,3,12,2
1206,,3,Primitive multicellular organisms,false,false,5,14,8
1301,,3,Primitive single-cell organisms,false,false,0,10,0
1309,,7,Advanced cellular organisms,false,false,1,14,9
2003,,5,Primitive single-cell organisms,false,false,9,6,4
3103,,6,Primitive multicellular organisms,false,false,5,16,7
3202,,8,Primitive single-cell organisms,false,false,0,15,0
0119,,4,Primitive single-cell organisms,false,false,10,11,6
1111,,3,Primitive single-cell organisms,false,false,11,10,8
1117,,9,Primitive single-cell organisms,false,false,0,8,5
1220,,4,Advanced cellular organisms,false,false,6,12,2
1414,,7,Advanced cellular organisms,false,false,0,12,0
1911,,9,Complex multicellular organisms,false,false,5,4,5
2016,,5,Primitive single-cell organisms,false,false,5,16,7
2120,,4,Advanced cellular organisms,false,false,7,15,0
2414,,2,Differentiated multicellular organisms,false,false,0,14,0
3217,,7,Primitive single-cell organisms,false,false,7,11,9
3218,,8,Differentiated multicellular organisms,false,false,0,15,0
0122,,3,Primitive single-cell organisms,false,false,1,16,6
0126,,4,Primitive single-cell organisms,false,false,1,14,0
0221,,5,Primitive single-cell organisms,false,false,11,14,5
0224,,6,Primitive single-cell organisms,false,false,1,12,0
0225,,5,Primitive single-cell organisms,false,false,6,14,7
0426,,3,Primitive single-cell organisms,false,false,7,17,5
0529,,8,Primitive single-cell organisms,false,false,2,12,8
0927,,7,Socially advanced organisms,false,false,2,11,6
1023,,9,Advanced cellular organisms,false,false,0,15,0
1026,,7,Primitive single-cell organisms,false,false,0,14,1
1121,,5,Advanced cellular organisms,false,false,6,8,7
1122,,4,Primitive single-cell organisms,false,false,0,13,0
1123,,5,Differentiated multicellular organisms,false,false,10,13,9
1223,,4,Primitive single-cell organisms,false,false,5,14,9
1224,,6,Advanced cellular organisms,false,false,9,18,8
1327,,5,Advanced cellular organisms,false,false,7,13,8
1329,,4,Primitive single-cell organisms,false,false,8,13,5
1421,,7,Primitive single-cell organisms,false,false,3,18,5
1524,,8,Primitive single-cell organisms,false,false,0,8,0
1530,,6,Primitive single-cell organisms,false,false,2,15,5
1623,,7,Complex multicellular organisms,false,false,1,11,0
1725,,2,Primitive single-cell organisms,false,false,6,11,2
1727,,4,Primitive single-cell organisms,false,false,8,14,6
1730,,6,Complex multicellular organisms,false,false,5,12,6
1826,,5,Primitive single-cell organisms,false,false,2,14,8
1828,,5,Primitive single-cell organisms,false,false,0,14,0
1921,,5,Differentiated multicellular organisms,false,false,1,17,1
1929,,5,Complex multicellular organisms,false,false,9,15,6
2023,,9,Primitive single-cell organisms,false,false,1,7,0
2024,,4,Primitive single-cell organisms,false,false,2,16,3
2029,,5,Complex multicellular organisms,false,false,2,16,7
2121,,6,Primitive single-cell organisms,false,false,3,13,0
2126,,5,Primitive single-cell organisms,false,false,9,11,2
2127,,4,Primitive single-cell organisms,false,false,0,9,2
2130,,4,Primitive single-cell organisms,false,false,6,13,9
2228,,6,Primitive single-cell organisms,false,false,0,6,0
2321,,8,Differentiated multicellular organisms,false,false,8,8,10
2325,,8,Primitive multicellular organisms,false,false,9,13,8
2330,,4,Complex multicellular organisms,false,false,0,17,0
2421,,5,Primitive single-cell organisms,false,false,0,12,0
2426,,7,Complex multicellular organisms,false,false,9,9,4
2526,,4,Primitive single-cell organisms,false,false,4,15,7
2626,,9,Primitive single-cell organisms,false,false,1,12,0
2723,,6,Primitive single-cell organisms,false,false,4,10,6
2725,,4,Advanced cellular organisms,false,false,6,8,0
2726,,4,Primitive single-cell organisms,false,false,10,9,3
2727,,5,Primitive single-cell organisms,false,false,5,15,8
2728,,8,Primitive single-cell organisms,false,false,1,13,0
2729,,6,Primitive single-cell organisms,false,false,0,18,0
2822,,5,Advanced multicellular organisms,false,false,10,14,8
2825,,3,Advanced cellular organisms,false,false,6,15,5
2826,,6,Primitive single-cell organisms,false,false,0,13,0
2827,,8,Advanced cellular organisms,false,false,1,13,0
2829,,3,Complex multicellular organisms,false,false,7,15,8
2922,,7,Primitive single-cell organisms,false,false,6,15,1
2926,,3,Socially advanced organisms,false,false,6,14,10
2927,,6,Primitive multicellular organisms,false,false,3,17,5
3023,,7,Primitive single-cell organisms,false,false,3,13,9
3027,,5,Advanced multicellular organisms,false,false,7,11,6
3029,,6,Complex multicellular organisms,false,false,7,13,8
3030,,5,Advanced cellular organisms,false,false,6,14,3
3122,,6,Primitive single-cell organisms,false,false,3,21,4
3129,,7,Socially advanced organisms,false,false,4,13,9
3130,,4,Advanced cellular organisms,false,false,9,11,6
3228,,5,Primitive single-cell organisms,false,false,0,6,0
0132,,5,Socially advanced organisms,false,false,5,13,7
0134,,5,Primitive single-cell organisms,false,false,7,15,5
0135,,5,Primitive single-cell organisms,false,false,5,14,2
0138,,5,Primitive single-cell organisms,false,false,6,14,8
0231,,4,Complex multicellular organisms,false,false,0,14,0
0233,,6,Complex multicellular organisms,false,false,4,7,7
0239,,2,Advanced multicellular organisms,false,false,8,12,7
0331,,3,Primitive single-cell organisms,false,false,4,8,4
0336,,7,Primitive single-cell organisms,false,false,0,5,1
0337,,4,Primitive single-cell organisms,false,false,9,16,8
0431,,7,Primitive single-cell organisms,false,false,8,20,10
0433,,5,Primitive single-cell organisms,false,false,8,12,1
0531,,3,Primitive single-cell organisms,false,false,8,16,6
0533,,5,Primitive single-cell organisms,false,false,2,10,0
0535,,5,Primitive single-cell organisms,false,false,5,17,2
0538,,6,Advanced cellular organisms,false,false,9,18,8
0540,,5,Differentiated multicellular organisms,false,false,6,15,8
0632,,7,Primitive single-cell organisms,false,false,0,15,0
0634,,7,Advanced multicellular organisms,false,false,5,15,6
0640,,8,Advanced cellular organisms,false,false,0,9,0
0732,,3,Socially advanced organisms,false,false,4,16,8
0736,,5,Primitive single-cell organisms,false,false,4,11,3
0831,,7,Differentiated multicellular organisms,false,false,9,10,4
0832,,7,Advanced multicellular organisms,false,false,5,12,7
0838,,5,Complex multicellular organisms,false,false,8,21,3
0840,,3,Primitive single-cell organisms,false,false,0,18,0
0933,,4,Advanced multicellular organisms,false,false,7,15,8
0934,,8,Socially advanced organisms,false,false,3,15,7
0935,,5,Primitive single-cell organisms,false,false,3,17,0
0937,,4,Primitive single-cell organisms,false,false,7,13,4
0939,,4,Differentiated multicellular organisms,false,false,0,13,0
1031,,5,Primitive multicellular organisms,false,false,2,13,6
1032,,5,Primitive multicellular organisms,false,false,9,18,6
1033,,4,Primitive single-cell organisms,false,false,9,16,7
1038,,6,Primitive single-cell organisms,false,false,7,12,6
1039,,4,Primitive single-cell organisms,false,false,0,16,0
1040,,4,Primitive multicellular organisms,false,false,0,18,0
1131,,7,Advanced multicellular organisms,false,false,6,8,7
1132,,5,Advanced cellular organisms,false,false,0,10,0
1136,,8,Primitive single-cell organisms,false,false,1,12,0
1139,,4,Primitive single-cell organisms,false,false,2,12,9
1231,,7,Complex multicellular organisms,false,false,5,15,9
1233,,6,Primitive single-cell organisms,false,false,0,16,0
1234,,6,Primitive single-cell organisms,false,false,0,10,0
1238,,6,Advanced multicellular organisms,false,false,2,15,11
1333,,9,Advanced multicellular organisms,false,false,6,15,8
1335,,5,Primitive single-cell organisms,false,false,7,12,8
1337,,5,Primitive single-cell organisms,false,false,0,15,0
1338,,4,Advanced multicellular organisms,false,false,0,12,1
1431,,5,Advanced cellular organisms,false,false,0,7,0
1433,,5,Primitive single-cell organisms,false,false,0,10,1
1434,,7,Differentiated multicellular organisms,false,false,6,20,4
1436,,5,Primitive single-cell organisms,false,false,0,10,0
1439,,6,Primitive single-cell organisms,false,false,1,16,0
1440,,3,Primitive multicellular organisms,false,false,2,16,3
1531,,4,Primitive single-cell organisms,false,false,3,17,0
1535,,6,Primitive single-cell organisms,false,false,6,19,3
1536,,4,Advanced cellular organisms,false,false,0,15,0
1537,,3,Primitive single-cell organisms,false,false,3,16,4
1539,,4,Advanced cellular organisms,false,false,4,16,5
1637,,5,Primitive single-cell organisms,false,false,4,9,3
1639,,3,Primitive single-cell organisms,false,false,4,10,5
1734,,5,Primitive single-cell organisms,false,false,11,18,9
1736,,6,Primitive single-cell organisms,false,false,9,17,10
1737,,5,Primitive single-cell organisms,false,false,3,13,3
1738,,7,Primitive single-cell organisms,false,false,0,15,0
1832,,3,Primitive single-cell organisms,false,false,0,10,0
1836,,3,Primitive single-cell organisms,false,false,6,20,0
1840,,5,Primitive single-cell organisms,false,false,9,13,2
1931,,6,Primitive single-cell organisms,false,false,3,9,0
1932,,3,Advanced cellular organisms,false,false,1,12,0
1933,,7,Advanced cellular organisms,false,false,1,16,0
1934,,6,Differentiated multicellular organisms,false,false,5,16,3
1935,,8,Primitive single-cell organisms,false,false,3,12,8
1936,,5,Primitive single-cell organisms,false,false,7,13,6
1938,,9,Primitive single-cell organisms,false,false,6,12,6
1940,,6,Primitive single-cell organisms,false,false,9,15,2
2031,,3,Primitive single-cell organisms,false,false,6,15,4
2033,,3,Primitive single-cell organisms,false,false,2,15,4
2034,,7,Primitive single-cell organisms,false,false,7,15,2
2037,,7,Primitive single-cell organisms,false,false,6,11,6
2040,,5,Primitive single-cell organisms,false,false,1,14,3
2132,,7,Primitive single-cell organisms,false,false,4,15,2
2133,,7,Advanced cellular organisms,false,false,0,19,6
2134,,7,Primitive multicellular organisms,false,false,6,12,6
2137,,5,Primitive single-cell organisms,false,false,8,18,5
2139,,5,Primitive single-cell organisms,false,false,7,14,4
2231,,4,Primitive single-cell organisms,false,false,6,16,8
2238,,6,Advanced cellular organisms,false,false,0,11,0
2239,,8,Primitive single-cell organisms,false,false,3,11,0
2331,,5,Primitive single-cell organisms,false,false,4,17,6
2337,,4,Primitive single-cell organisms,false,false,6,10,0
2340,,5,Advanced multicellular organisms,false,false,7,13,7
2434,,3,Primitive single-cell organisms,false,false,7,15,9
2436,,5,Primitive single-cell organisms,false,false,5,11,7
2437,,5,Primitive single-cell organisms,false,false,7,11,8
2439,,6,Primitive single-cell organisms,false,false,0,15,0
2440,,2,Primitive single-cell organisms,false,false,3,14,5
2531,,7,Primitive single-cell organisms,false,false,6,19,6
2533,,7,Primitive single-cell organisms,false,false,5,12,5
2536,,3,Primitive single-cell organisms,false,false,3,16,4
2537,,4,Primitive single-cell organisms,false,false,0,10,0
2538,,5,Primitive multicellular organisms,false,false,10,13,6
2539,,5,Advanced multicellular organisms,false,false,6,13,8
2540,,3,Socially advanced organisms,false,false,9,12,4
2631,,3,Primitive single-cell organisms,false,false,8,17,3
2632,,8,Differentiated multicellular organisms,false,false,9,16,11
2633,,6,Advanced cellular organisms,false,false,6,18,5
2637,,6,Primitive single-cell organisms,false,false,0,13,0
2731,,6,Primitive single-cell organisms,false,false,5,14,9
2732,,6,Primitive single-cell organisms,false,false,4,11,4
2734,,6,Advanced cellular organisms,false,false,5,14,6
2736,,5,Socially advanced organisms,false,false,9,15,7
2737,,5,Differentiated multicellular organisms,false,false,3,16,6
2740,,8,Advanced multicellular organisms,false,false,7,13,5
2832,,4,Advanced cellular organisms,false,false,1,13,4
2835,,8,Primitive single-cell organisms,false,false,0,14,0
2836,,5,Primitive single-cell organisms,false,false,6,14,1
2839,,3,Differentiated multicellular organisms,false,false,2,15,5
2934,,7,Primitive single-cell organisms,false,false,7,10,6
2935,,3,Advanced cellular organisms,false,false,5,15,4
2937,,3,Complex multicellular organisms,false,false,0,14,0
2940,,5,Primitive single-cell organisms,false,false,11,14,3
3031,,6,Advanced cellular organisms,false,false,0,15,0
3033,,5,Primitive single-cell organisms,false,false,7,12,3
3037,,6,Primitive single-cell organisms,false,false,0,12,6
3133,,6,Primitive single-cell organisms,false,false,2,14,0
3134,,7,Primitive single-cell organisms,false,false,10,13,4
3136,,4,Primitive single-cell organisms,false,false,2,14,4
3231,,5,Primitive single-cell organisms,false,false,7,15,3
3233,,7,Primitive single-cell organisms,false,false,1,9,1
3234,,5,Differentiated multicellular organisms,false,false,3,14,7
3236,,5,Differentiated multicellular organisms,false,false,7,15,6
3238,,6,Primitive single-cell organisms,false,false,4,14,6
"""
