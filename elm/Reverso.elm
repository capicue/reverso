module Reverso exposing (..)

import Http
import Json.Decode as JD
import Task exposing (Task)
import String


languages : List String
languages =
    [ "arabic", "german", "english", "spanish", "french", "hebrew", "italian", "dutch", "polish", "portuguese", "russian" ]


url : String -> String -> String -> String
url from to word =
    "https://tonicdev.io/capicue/reverso/branches/master?from=" ++ from ++ "&to=" ++ to ++ "&word=" ++ word


decoder : JD.Decoder (List ( String, String ))
decoder =
    JD.list (JD.tuple2 (,) JD.string JD.string)


examples : String -> String -> String -> Task Http.Error (List ( String, String ))
examples from to word =
    Http.send
        Http.defaultSettings
        { verb = "GET"
        , url = (url from to word)
        , headers = []
        , body = Http.string ""
        }
        |> Http.fromJson decoder
