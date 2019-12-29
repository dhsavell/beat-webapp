port module Main exposing (Model, Msg, init, subscriptions, update, updatePlayerSong, view)

import Api
import Base64
import Browser
import Bytes exposing (Bytes)
import EffectView
import Effects
import File exposing (File)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Patrons
import Taglines
import Random


taglineGenerator : Random.Generator Int
taglineGenerator = Random.int 0 <| List.length Taglines.all

selectNewTagline : Cmd Msg
selectNewTagline = Random.generate (\i -> ChangeTagline <| Maybe.withDefault "???" <| (Taglines.all |> List.drop (i - 1) |> List.head)) taglineGenerator


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type InputMode
    = File
    | Url


type CustomSettings
    = Disabled
    | EstimatedBpm Int Int


type ProcessingState
    = Succeeded
    | Failed String
    | InProgress
    | NotStarted


type alias Model =
    { song : Maybe Api.SongSource
    , settings : CustomSettings
    , inputMode : InputMode
    , effects : EffectView.EffectCollection
    , processing : ProcessingState
    , tagline : String
    }



-- UPDATE


type Msg
    = ChangeInputMode InputMode
    | SetSongUrl String
    | SetSongFile File
    | SendSong
    | GotSong (Result Http.Error Bytes)
    | EffectMsg EffectView.Msg
    | ToggleCustomSettings
    | NoOp
    | ChangeTagline String
    | RandomizeTagline


canSubmit : List Effects.EffectInstance -> Bool
canSubmit effects =
    case Effects.validateAll effects of
        Ok _ ->
            True

        _ ->
            False


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RandomizeTagline ->
            ( model, selectNewTagline)
        ChangeTagline s ->
            ( { model | tagline = s}, Cmd.none)

        ChangeInputMode m ->
            ( { model | inputMode = m }, Cmd.none )

        SetSongUrl u ->
            ( { model | song = Just <| Api.FromYoutubeUrl u }, Cmd.none )

        SetSongFile f ->
            ( { model | song = Just <| Api.FromFile f }, Cmd.none )

        SendSong ->
            case Effects.validateAll model.effects of
                Ok validEffects ->
                    case model.song of
                        Nothing ->
                            ( model, Cmd.none )

                        Just s ->
                            ( { model | processing = InProgress }
                            , Cmd.batch [ clearPlayerSong (), Api.sendSong s validEffects GotSong ]
                            )

                Err _ ->
                    -- TODO: List errors
                    ( model, Cmd.none )

        ToggleCustomSettings ->
            ( { model
                | settings =
                    if model.settings == Disabled then
                        EstimatedBpm 120 10

                    else
                        Disabled
              }
            , Cmd.none
            )

        EffectMsg m ->
            ( { model | effects = EffectView.update m model.effects }, Cmd.none )

        GotSong result ->
            case result of
                Err a ->
                    ( { model | processing = Failed (Debug.toString a) }, Cmd.none )

                Ok s ->
                    case Base64.fromBytes s of
                        Just d ->
                            ( { model | processing = Succeeded }, updatePlayerSong d )

                        Nothing ->
                            ( { model | processing = Failed "Couldn't decode song" }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "container"
        , class "app"
        ]
        [ section []
            [ h1 [ class "one-word-per-line" ] [ text "The Beat Machine" ]
            , h3 [ class "tagline", onClick RandomizeTagline ] [ text model.tagline ]
            , p [] [ text "Ever wondered what your favorite song sounds like with every other beat missing? No? Well, either way, now you can find out! The Beat Machine is a webapp for making beat edits to songs." ]
            ]
        , section [ class "frame" ]
            [ h3 [] [ text "Song" ]
            , p [] [ text "Choose and configure a song. Shorter songs process faster!" ]
            , div [ class "row" ]
                [ div [ class "four", class "columns" ]
                    [ label [] [ text "Source" ]
                    , label []
                        [ input
                            [ onClick (ChangeInputMode File)
                            , type_ "radio"
                            , name "input-mode"
                            , checked (model.inputMode == File)
                            ]
                            []
                        , span [ class "label-body" ] [ text "MP3 File" ]
                        ]
                    , label []
                        [ input
                            [ onClick (ChangeInputMode Url)
                            , type_ "radio"
                            , name "input-mode"
                            , checked (model.inputMode == Url)
                            ]
                            []
                        , span [ class "label-body" ] [ text "YouTube Video" ]
                        ]
                    ]
                , div [ class "eight", class "columns" ]
                    [ case model.inputMode of
                        File ->
                            div []
                                [ label [] [ text "Select file" ]
                                , input
                                    [ type_ "file"
                                    , multiple False
                                    , accept "audio/mpeg, .mp3"
                                    , on "change" (D.map SetSongFile filesDecoder)
                                    ]
                                    []
                                ]

                        Url ->
                            div []
                                [ label [] [ text "Paste YouTube video URL" ]
                                , input
                                    [ type_ "url"
                                    , class "u-full-width"
                                    , onInput SetSongUrl
                                    ]
                                    []
                                , p [] [ text "Not all videos can be downloaded. If you run into weird issues (i.e. empty audio after rendering), try using an MP3 instead."]
                                ]
                    ]
                ]
            , br [] []
            , p [] [ text "The following settings are optional, but let you fine-tune the result if it's not what you expected. When using live performances or songs with tempo changes, be sure to set a high enough tolerance." ]
            , div [ class "row" ]
                [ div [ class "four", class "columns" ]
                    [ label []
                        [ input
                            [ type_ "checkbox"
                            , name "use-bpm"
                            , onClick ToggleCustomSettings
                            , checked (model.settings /= Disabled)
                            ]
                            []
                        , span [ class "label-body" ] [ text "Set tempo manually" ]
                        ]
                    ]
                , div [ class "four", class "columns" ]
                    [ label [] [ text "Estimated BPM" ]
                    , input [ type_ "number", value "120", Html.Attributes.min "10", Html.Attributes.max "500", class "u-full-width", disabled (model.settings == Disabled) ] []
                    ]
                , div [ class "four", class "columns" ]
                    [ label [] [ text "Tolerance" ]
                    , input [ type_ "number", value "10", Html.Attributes.min "3", Html.Attributes.max "500", class "u-full-width", disabled (model.settings == Disabled) ] []
                    ]
                ]
            ]
        , section [ class "frame" ]
            [ h3 [] [ text "Effects" ]
            , p [] [ text "Add up to 5 sequential effects to rearrange your song." ]
            , Html.map EffectMsg (EffectView.viewAllEffects model.effects)
            ]
        , section []
            [ h3 [] [ text "Support" ]
            , p [] [ text "Continued development of The Beat Machine is made possible by supporters on Patreon!" ]
            , div [ class "patrons" ] (List.map (\p -> Html.map (\_ -> NoOp) (Patrons.viewPatron p)) Patrons.all)
            , p [] [ text "If you'd like to have your name and links on this page, consider making a pledge." ]
            ]
        , section [ class "frame" ]
            [ h3 [] [ text "Result" ]
            , p [] [ text "Press the button to render the result! This will take a moment." ]
            , div [ class "render-button-container" ] [ button
                [ disabled (model.song == Nothing || model.processing == InProgress || List.length model.effects <= 0 || not (canSubmit model.effects))
                , onClick SendSong
                , class "button-primary"
                , class "render-button"
                ]
                [ text "Render!" ]
            ]
            , case model.processing of
                InProgress ->
                    div []
                        [ p [ class "status" ] [ text "Hold on..." ]
                        , div [ class "loader" ]
                            [ div [ id "r1" ] []
                            , div [ id "r2" ] []
                            , div [ id "r3" ] []
                            , div [ id "r4" ] []
                            ]
                        ]

                Failed errorMsg ->
                    p [ class "status", class "error" ] [ text ("An error occurred (" ++ errorMsg ++ "). ") ]

                _ ->
                    text ""
            , audio
                [ id "player"
                , controls True
                , classList [ ( "hidden", model.processing /= Succeeded ) ]
                ]
                []
            , p [ classList [ ( "hidden", model.processing /= Succeeded ) ] ]
                [ text "Right-click on the player above or "
                , a
                    [ id "download"
                    , download ""
                    , href "test"
                    ]
                    [ text "use this link" ]
                , text " to download the result."
                ]
            ]
        ]


filesDecoder : D.Decoder File
filesDecoder =
    D.at [ "target", "files" ] (D.index 0 File.decoder)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( { song = Nothing
      , inputMode = File
      , settings = Disabled
      , effects =
            [ { type_ = Effects.swap, values = Effects.defaultValues Effects.swap }
            ]
      , processing = NotStarted
      , tagline = ""
      }
    , selectNewTagline
    )



-- PORTS


port clearPlayerSong : () -> Cmd msg


port updatePlayerSong : String -> Cmd msg
