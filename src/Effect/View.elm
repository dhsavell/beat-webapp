module Effect.View exposing (Msg, update, viewEffects)

import Dict
import Effect exposing (Effect)
import Effect.Types
import FeatherIcons
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D


type Msg
    = SetType Int (Maybe Effect)
    | SetParam Int String Int
    | AddEffect
    | RemoveEffect


replace : Int -> (a -> a) -> List a -> List a
replace idx f list =
    List.indexedMap
        (\i v ->
            if i == idx then
                f v

            else
                v
        )
        list


update : Msg -> List Effect.Instance -> List Effect.Instance
update msg effects =
    case msg of
        SetType idx maybeNewType ->
            case maybeNewType of
                Just newType ->
                    replace idx (\_ -> { type_ = newType, values = Effect.defaults newType }) effects

                Nothing ->
                    effects

        SetParam idx paramId newValue ->
            replace idx (\d -> { d | values = Dict.update paramId (\m -> Maybe.map (\_ -> newValue) m) d.values }) effects

        AddEffect ->
            effects ++ [ { type_ = Effect.Types.swap, values = Effect.defaults Effect.Types.swap } ]

        RemoveEffect ->
            List.take (List.length effects - 1) effects


effectFromId : String -> Maybe Effect
effectFromId id =
    List.head <| List.filter (\e -> e.id == id) Effect.Types.all


viewField : Int -> Dict.Dict String Int -> Effect.Field -> Html Msg
viewField effectIdx values field =
    div [ class "param" ]
        [ label [] [ text field.name ]
        , input
            [ class "u-full-width"
            , type_ "number"
            , Html.Attributes.min (String.fromInt field.min)
            , Html.Attributes.max (String.fromInt field.max)
            , value <| String.fromInt <| Maybe.withDefault 0 <| Dict.get field.id values
            , onInput (\s -> SetParam effectIdx field.id <| Maybe.withDefault 0 (String.toInt s))
            ]
            []
        ]


viewEffectSelector : Int -> List Effect -> Html Msg
viewEffectSelector effectIdx types =
    select [ class "effect-type", class "u-full-width", onInput (SetType effectIdx << effectFromId) ] (List.map (\t -> option [ value t.id ] [ text t.name ]) types)


viewEffect : Int -> Effect.Instance -> Html Msg
viewEffect effectIdx effect =
    let
        validation =
            Effect.validateInstance effect
    in
    section
        [ class "frame"
        , classList
            [ ( "error"
              , case validation of
                    Err _ ->
                        True

                    _ ->
                        False
              )
            ]
        ]
        [ h5 [] [ text <| String.fromInt (effectIdx + 1) ++ ". " ++ effect.type_.name ]
        , case validation of
            Ok _ ->
                text ""

            Err [] ->
                p [ class "effect-error" ] [ text "An error occurred." ]

            Err (errorMsg :: _) ->
                p [ class "effect-error" ] [ text errorMsg ]
        , div [ class "row" ]
            [ div [ class "five", class "columns" ]
                [ label [] [ text "Type" ]
                , viewEffectSelector effectIdx Effect.Types.all
                , p [ class "effect-desc" ] [ text effect.type_.description ]
                ]
            , if List.length effect.type_.params > 0 then
                div [ class "seven", class "columns" ] (List.map (viewField effectIdx effect.values) effect.type_.params)

              else
                text ""
            ]
        ]


viewEffects : List Effect.Instance -> Html Msg
viewEffects effects =
    div [ class "container" ]
        (List.indexedMap viewEffect effects
            ++ [ div [ class "effect-controls" ]
                    [ a
                        [ class "button button-primary"
                        , on "click" <| D.succeed AddEffect
                        , disabled (List.length effects >= 5)
                        ]
                        [ FeatherIcons.toHtml [ height 16 ] FeatherIcons.plus ]
                    , a
                        [ class "button"
                        , on "click" <| D.succeed RemoveEffect
                        , disabled (List.length effects <= 1)
                        ]
                        [ FeatherIcons.toHtml [ height 16 ] FeatherIcons.minus ]
                    ]
               ]
        )
