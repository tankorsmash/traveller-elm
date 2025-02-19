port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Dialog
import Element
import HostConfig
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, href)
import Html.Events
import Http exposing (Error(..))
import Json.Encode
import Maybe.Extra as Maybe
import RemoteData exposing (RemoteData(..))
import Traveller
import Url
import Url.Parser exposing ((</>), Parser, map, oneOf, s, top)
import Url.Parser.Query


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


init : Maybe ( Int, Int ) -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        hostConfig : HostConfig.HostConfig
        hostConfig =
            Url.Parser.parse HostConfig.urlParser url
                |> (\parseResult ->
                        case parseResult of
                            Just (Just parsedHostConfig) ->
                                parsedHostConfig

                            Just Nothing ->
                                HostConfig.default

                            Nothing ->
                                HostConfig.default
                   )

        ( travellerModel, travellerCmds ) =
            Traveller.init flags key hostConfig

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
        [ -- we take a bunch of `Traveller.Cmds` from elsewhere and wrap them in the right
          -- `Main.Msg`, since only the toplevel `update` function can pass them back up to Elm
          --
          -- So when we get the Cmd's response back, we can pass them back to the `Traveller.update`
          -- when we handle the `GotTravellerMsg` msg
          Cmd.map GotTravellerMsg travellerCmds
        ]
    )


main : Program (Maybe ( Int, Int )) Model Msg
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

        GotTravellerMsg travellerMsg ->
            let
                ( newTravellerModel, newTravellerCmds ) =
                    Traveller.update travellerMsg model.travellerModel
            in
            ( { model | travellerModel = newTravellerModel }
            , Cmd.map GotTravellerMsg newTravellerCmds
            )


{-| since elm-ui only expects one layout with a static stylesheet, we put this
one first always so it'll have the stylesheet set-up and all the rest don't
need to specify much.
-}
elmUiHackLayout : Html.Html msg
elmUiHackLayout =
    Html.div [ Html.Attributes.style "height" "0" ]
        [ Element.layoutWith
            { options =
                [ Element.focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }
            [ Element.htmlAttribute <| Html.Attributes.id "hack" ]
          <|
            Element.none
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Revelation"
    , body =
        [ Dialog.view "error-dialog" ToggleErrorDialog model.dialogBody
        , div
            []
            [ elmUiHackLayout
            , Html.nav [ class "navbar navbar-expand m-3" ]
                [ a [ href "?", class "navbar-brand" ] [ text "Navigation" ]
                , Html.ul [ class "navbar-nav" ]
                    [ Html.li [ class "nav-item" ] [ a [ class "nav-link", href "/" ] [ text "Uncharted Space" ] ]
                    ]
                ]
            , Html.map GotTravellerMsg <|
                Element.layoutWith { options = [ Element.noStaticStyleSheet ] } [ Element.centerX ] <|
                    Traveller.view model.travellerModel
            ]
        ]
    }
