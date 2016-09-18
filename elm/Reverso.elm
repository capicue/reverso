module Reverso exposing (..)

import Http
import Json.Decode as JD
import Task exposing (Task)
import String
import RemoteData
import TranslationList


languages : List String
languages =
    [ "arabic"
    , "german"
    , "english"
    , "spanish"
    , "french"
    , "hebrew"
    , "italian"
    , "dutch"
    , "polish"
    , "portuguese"
    , "russian"
    ]


url : String -> String -> String -> String
url from to word =
    "https://runkit.io/capicue/reverso/branches/master?from=" ++ from ++ "&to=" ++ to ++ "&word=" ++ word


decoder : JD.Decoder (List ( String, String ))
decoder =
    JD.list (JD.tuple2 (,) JD.string JD.string)


toTranslationList : List ( String, String ) -> TranslationList.Model
toTranslationList list =
    let
        toTranslation pair =
            TranslationList.Translation (fst pair) (snd pair) TranslationList.initialTranslationState
    in
        TranslationList.Model
            []
            (List.map toTranslation list)


examples : String -> String -> String -> Task Http.Error TranslationList.Model
examples from to word =
    Http.send
        Http.defaultSettings
        { verb = "GET"
        , url = (url from to word)
        , headers = []
        , body = Http.string ""
        }
        |> Http.fromJson decoder
        |> Task.map toTranslationList
