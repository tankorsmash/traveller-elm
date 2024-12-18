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
    , Remark(..)
    , SpectralClassification(..)
    , SpectralType(..)
    , Starport(..)
    , StellarData(..)
    , SystemName
    , TechLevel(..)
    , UWP
    , Zone(..)
    , allegiance
    , base
    , culturalExtension
    , economicExtension
    , government
    , hydrosphere
    , importanceExtension
    , lawLevel
    , nobility
    , parseSingleDigit
    , pbg
    , planetarySize
    , remark
    , starport
    , stellarData
    , techLevel
    , uwp
    , zone
    )

import Parser exposing ((|.), (|=), Parser)
import Parser.Extras as Parser
import Traveller.Atmosphere exposing (Atmosphere, atmosphere)
import Traveller.EHex exposing (EHex, eHex)
import Traveller.Population exposing (Population, population)



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
