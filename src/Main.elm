port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Chart.Item as CI
import Codec
import Dialog
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import Element.Lazy
import Element.Region exposing (description)
import Html exposing (Html, a, b, button, div, h1, hr, li, span, text, ul)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onClick)
import Http exposing (Error(..))
import Json.Decode
import Json.Encode
import List
import List.Extra as List
import Random
import Random.List
import RemoteData exposing (RemoteData(..))
import SolarSystemPage
import Traveller
import Url
import Url.Parser exposing ((</>), Parser, int, map, oneOf, s, string, top)


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , route : Maybe Route
    , dialogBody : Html Msg
    , isDarkMode : Bool
    , travellerModel : Traveller.Model
    , solarSystemModel : SolarSystemPage.Model
    }


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ToggleErrorDialog
    | ToggleDarkMode
    | GotTravellerMsg Traveller.Msg
    | GotSolarSystemPageMsg SolarSystemPage.Msg


type Route
    = TravellerPage
    | StarPage


port writeToLocalStorage : ( String, String ) -> Cmd msg


port listenToLocalStorage : (Json.Encode.Value -> msg) -> Sub msg


port toggleDialog : String -> Cmd msg


port toggleDarkMode : Bool -> Cmd msg


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map TravellerPage top
        , map StarPage (s "view_star")
        ]


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( travellerModel, travellerCmds ) =
            Traveller.init

        ( solarSystemModel, starPageCmds ) =
            SolarSystemPage.init

        model : Model
        model =
            { key = key
            , url = url
            , route = Url.Parser.parse routeParser url
            , isDarkMode = False
            , dialogBody = text "Error dialog"
            , travellerModel = travellerModel
            , solarSystemModel = solarSystemModel
            }
    in
    ( model
    , Cmd.batch
        [ Cmd.map GotTravellerMsg travellerCmds, Cmd.map GotSolarSystemPageMsg starPageCmds]
    )


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


subscriptions : Model -> Sub Msg
subscriptions { travellerModel } =
    Traveller.subscriptions travellerModel
        |> Sub.map GotTravellerMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model
                | url = url
                , route = Url.Parser.parse routeParser url
              }
            , Cmd.none
            )

        ToggleErrorDialog ->
            ( model, toggleDialog "error-dialog" )

        ToggleDarkMode ->
            ( { model | isDarkMode = not model.isDarkMode }
            , toggleDarkMode (not model.isDarkMode)
            )

        GotTravellerMsg travellerMsg ->
            let
                ( newTravellerModel, newTravellerCmds ) =
                    Traveller.update travellerMsg model.travellerModel
            in
            ( { model | travellerModel = newTravellerModel }
            , Cmd.map GotTravellerMsg newTravellerCmds
            )

        GotSolarSystemPageMsg solarSystemPageMsg ->
            let
                ( newStarPageModel, newStarPageCmds ) =
                    SolarSystemPage.update solarSystemPageMsg model.solarSystemModel
            in
            ( { model | solarSystemModel = newStarPageModel }
            , Cmd.map GotSolarSystemPageMsg newStarPageCmds
            )


view : Model -> Browser.Document Msg
view model =
    { title = "Traveller"
    , body =
        [ Dialog.view "error-dialog" ToggleErrorDialog model.dialogBody
        , div
            []
            [ Html.button [ Html.Events.onClick ToggleDarkMode ] [ text "Light/Dark" ]
            , case model.route of
                Just TravellerPage ->
                    Html.map GotTravellerMsg <|
                        Element.layout [ Element.centerX ] <|
                            Traveller.view model.travellerModel

                Just StarPage ->
                    Html.map GotSolarSystemPageMsg <|
                        Element.layout [ Element.centerX ] <|
                            SolarSystemPage.view model.solarSystemModel

                Nothing ->
                    Html.text "404 i guess"
            ]
        ]
    }
