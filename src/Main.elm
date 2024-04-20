port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Chart.Item as CI
import Codec
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Lazy
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
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
    }


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ToggleErrorDialog
    | ToggleDarkMode
    | GotTravellerMsg Traveller.Msg


type Route
    = TravellerPage


port writeToLocalStorage : ( String, String ) -> Cmd msg


port listenToLocalStorage : (Json.Encode.Value -> msg) -> Sub msg


port toggleDialog : String -> Cmd msg


port toggleDarkMode : Bool -> Cmd msg


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map TravellerPage top
        ]


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( travellerModel, travellerCmds ) =
            Traveller.init

        model : Model
        model =
            { key = key
            , url = url
            , route = Url.Parser.parse routeParser url
            , isDarkMode = False
            , dialogBody = text "Error dialog"
            , travellerModel = travellerModel
            }
    in
    ( model
    , Cmd.batch
        [ Cmd.map GotTravellerMsg travellerCmds ]
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
subscriptions _ =
    Sub.none


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


dialog : String -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
dialog elementId attr content =
    Html.node "dialog" (Html.Attributes.id elementId :: attr) content


view : Model -> Browser.Document Msg
view model =
    { title = "Traveller"
    , body =
        [ dialog "error-dialog"
            [-- any bootstrap classes in here seem to force visibliy and breaks HTML Dialog
            ]
            [ model.dialogBody
            , Html.button
                [ Html.Attributes.class "btn btn-primary m-2"
                , Html.Events.onClick ToggleErrorDialog
                ]
                [ text "Close" ]
            ]
        , div
            []
            [ Html.button [ Html.Events.onClick ToggleDarkMode ] [ text "Light/Dark" ]
            , case model.route of
                Just TravellerPage ->
                    Html.map GotTravellerMsg <|
                        Element.layout [] <|
                            Traveller.view model.travellerModel

                Nothing ->
                    Html.text "404 i guess"
            ]
        ]
    }
