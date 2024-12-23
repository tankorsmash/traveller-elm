module HostConfig exposing (HostConfig, default, protocolToString, serverHost, urlParseHostConfig)

import Url
import Url.Parser
import Url.Parser.Query


type alias HostConfig =
    ( String, List String )


default : HostConfig
default =
    ( "https://radiofreewaba.net", [ "deepnight", "data", "solarsystems" ] )


{-| parses the entire Url, looks at the query string and returns the value of hostConfig

Eg. ?hostConfig=<https://radiofreewaba.net:80>
a query string with hostConfig and a string

> > > Just ("<https://radiofreewaba.net:80">)

Eg. ?hostConfig=123
a query string with hostConfig and but with a number, so it fails the query parsing

> > > Just (Nothing)

Eg. ?foo=bar
a query string without the key we want at all

> > > Nothing

-}
urlParseHostConfig : Url.Url -> Maybe (Maybe String)
urlParseHostConfig url =
    let
        -- parsed a query string and returns a String if the field at hostConfig is a string
        -- Eg looks at ?hostConfig= and returns the value, if its a string
        queryParser : Url.Parser.Query.Parser (Maybe String)
        queryParser =
            Url.Parser.Query.string "hostConfig"
    in
    Url.Parser.parse (Url.Parser.query queryParser) url


protocolToString : Url.Protocol -> String
protocolToString protocol =
    case protocol of
        Url.Http ->
            "http://"

        Url.Https ->
            "https://"


serverHost : Url.Url -> Maybe HostConfig
serverHost url =
    let
        {- >> rebuildRoot Http "radiofreewaba.net" (Just 8080) == "http://radiofreewaba.net:8080" -}
        rebuildRoot : Url.Protocol -> String -> Maybe Int -> String
        rebuildRoot protocol host port_ =
            protocolToString protocol
                ++ host
                ++ ":"
                ++ (Maybe.map String.fromInt port_ |> Maybe.withDefault "80")
    in
    case urlParseHostConfig url of
        Just (Just rawServerHost) ->
            Url.fromString rawServerHost
                |> Maybe.map
                    (\{ protocol, host, path, port_ } ->
                        ( rebuildRoot protocol host port_
                        , [ --drop the leading slash from the url
                            String.dropLeft 1 path
                          ]
                        )
                    )

        _ ->
            -- for all other cases, return the default host config
            Nothing
