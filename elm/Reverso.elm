module Reverso exposing (..)

import Array exposing (..)
import Http
import Json.Decode as JD
import Task exposing (Task)
import String
import RemoteData
import Translation


languages : List String
languages =
    [ "arabic", "german", "english", "spanish", "french", "hebrew", "italian", "dutch", "polish", "portuguese", "russian" ]


url : String -> String -> String -> String
url from to word =
    "https://tonicdev.io/capicue/reverso/branches/master?from=" ++ from ++ "&to=" ++ to ++ "&word=" ++ word


decoder : JD.Decoder (List ( String, String ))
decoder =
    JD.list (JD.tuple2 (,) JD.string JD.string)


toTranslationArray : List ( String, String ) -> Array Translation.Model
toTranslationArray list =
    let
        toTranslation i pair =
            Translation.Model i (fst pair) (snd pair) Translation.initialState
    in
        fromList (List.indexedMap toTranslation list)


examples : String -> String -> String -> Task Http.Error (Array Translation.Model)
examples from to word =
    Http.send
        Http.defaultSettings
        { verb = "GET"
        , url = (url from to word)
        , headers = []
        , body = Http.string ""
        }
        |> Http.fromJson decoder
        |> Task.map toTranslationArray
