module HostConfig exposing (HostConfig, default, protocolToString, queryStringParser, urlParser)

import Url
import Url.Parser
import Url.Parser.Query


type alias HostConfig =
    ( String, List String )


default : HostConfig
default =
    ( "https://radiofreewaba.net", [ "deepnight", "data" ] )


{-| A full Url.Parser that looks at the query string and returns it if found
-}
queryStringParser : Url.Parser.Parser (Maybe String -> a) a
queryStringParser =
    let
        -- parses a query string and returns a String if the field at hostConfig is a string
        -- Eg looks at ?hostConfig= and returns the value, if its a string
        queryParser : Url.Parser.Query.Parser (Maybe String)
        queryParser =
            Url.Parser.Query.string "hostConfig"
    in
    -- takes the query parser and makes a full parser out of it
    Url.Parser.query queryParser


protocolToString : Url.Protocol -> String
protocolToString protocol =
    case protocol of
        Url.Http ->
            "http://"

        Url.Https ->
            "https://"


{-| constructs a url from its parts

> rebuildRoot Http "radiofreewaba.net" (Just 8080) == "<http://radiofreewaba.net:8080">

-}
rebuildRoot : Url.Protocol -> String -> Maybe Int -> String
rebuildRoot protocol host port_ =
    protocolToString protocol
        ++ host
        ++ ":"
        ++ (Maybe.map String.fromInt port_ |> Maybe.withDefault "80")


{-| parses the entire Url, looks at the query string and returns the value the
hostConfig field if it is a string

Think of this func like Codec (Maybe HostConfig), and any Url.Parser turns into
a Maybe, instead of a DecodeError. The `a` generic type here seems weird, but
Elm's Url.Parser requires it.

---

Eg. ?hostConfig=<https://radiofreewaba.net:80>
a query string with hostConfig and a string

> Just ("<https://radiofreewaba.net:80">)

Eg. ?hostConfig=123
a query string with hostConfig and but with a number, so it fails the query parsing

> Just (Nothing)

Eg. ?foo=bar
a query string without the key we want at all

> Nothing

-}
urlParser : Url.Parser.Parser (Maybe HostConfig -> a) a
urlParser =
    queryStringParser
        |> Url.Parser.map
            -- Url.Parser.map always returns Maybe no matter what.
            -- Maybe.andThen takes a func that takes a value and returns another Maybe
            (Maybe.andThen
                -- so if we've got a string from the query, we try to turn it into
                -- a proper Url
                (Url.fromString
                    >> -- if it is, we build a HostConfig out of it
                       Maybe.map
                        (\{ protocol, host, path, port_ } ->
                            ( rebuildRoot protocol host port_
                            , --drop the leading slash from the url
                              case String.dropLeft 1 path of
                                "" ->
                                    []

                                withoutSlash ->
                                    [ withoutSlash ]
                            )
                        )
                )
            )
