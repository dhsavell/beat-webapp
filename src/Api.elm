module Api exposing (ProcessingSettings, SongSource(..), sendSong)

import Bytes exposing (Bytes)
import Dict
import Effects
import File exposing (File)
import Http exposing (filePart, jsonBody, multipartBody, post, stringPart)
import Json.Encode as Encode
import Validate


type SongSource
    = FromFile File
    | FromYoutubeUrl String


type alias ProcessingSettings =
    { estimatedBpm : Int
    , tolerance : Int
    }

effectsToJsonArray : List Effects.EffectInstance -> Encode.Value
effectsToJsonArray effects =
    Encode.list Encode.object <|
        List.map
            (\e ->
                ( "type", Encode.string e.type_.id )
                    :: (List.map (\( s, v ) -> ( s, Encode.int v )) <| Dict.toList (e.type_.postValidation e.values))
            )
            effects


sendSong : String -> SongSource -> Maybe ProcessingSettings -> List (Validate.Valid Effects.EffectInstance) -> (Result String Bytes -> msg) -> Cmd msg
sendSong baseUrl song settings validEffects toMsg =
    let
        effects =
            List.map Validate.fromValid validEffects

        endpoint =
            case song of
                FromFile _ ->
                    baseUrl

                FromYoutubeUrl _ ->
                    baseUrl ++ "/yt"

        body =
            case song of
                FromFile f ->
                    multipartBody
                        [ filePart "song" f
                        , stringPart "effects"
                            (Encode.encode 0 <|
                                Encode.object
                                    (( "effects", effectsToJsonArray effects )
                                        :: (case settings of
                                                Just s ->
                                                    [ ( "settings", Encode.object [ ( "suggested_bpm", Encode.int s.estimatedBpm ), ( "drift", Encode.int s.tolerance ) ] ) ]

                                                Nothing ->
                                                    []
                                           )
                                    )
                            )
                        ]

                FromYoutubeUrl u ->
                    jsonBody (Encode.object [ ( "youtube_url", Encode.string u ), ( "effects", effectsToJsonArray effects ), ( "settings", Encode.object [] ) ])
    in
    post
        { url = endpoint
        , body = body
        , expect = expectSongBytes toMsg
        }


expectSongBytes : (Result String Bytes -> msg) -> Http.Expect msg
expectSongBytes toMsg =
    Http.expectBytesResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ _ ->
                    Err "Failed to process server URL. This is a bug. If you have the time, please report it!"

                Http.Timeout_ ->
                    Err "A timeout occurred while contacting the server. It may be down or under heavy load. Try again in a moment."

                Http.NetworkError_ ->
                    Err "A network error occurred. Make sure that your MP3 or YouTube link is valid."

                Http.BadStatus_ metadata _ ->
                    Err ("A network error occurred (" ++ String.fromInt metadata.statusCode ++ "). Make sure that your MP3 or YouTube link is valid.")

                Http.GoodStatus_ _ body ->
                    Ok body