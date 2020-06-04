module Effect exposing (Field, Instance, Effect, defaults, instanceValidator, validateAll, validateInstance, effect, field, validator, finally)

import Dict exposing (Dict)
import Validate exposing (Validator)



-- MODELS


{-| A Field is a ranged integer parameter for an effect.
-}
type alias Field =
    { id : String
    , name : String
    , hint : Maybe String
    , min : Int
    , max : Int
    , default : Int
    }


{-| A Type is a class of effect, defined by a set of fields and optionally
custom validation and serialization logic.
-}
type alias Effect =
    { name : String
    , description : String
    , id : String
    , params : List Field
    , validation : List (Validator String (Dict String Int))
    , postValidation : Dict String Int -> Dict String Int
    }


effect : { id: String, name: String, description: String } -> Effect
effect r =
    { name = r.name, description = r.description, id = r.id, params = [], validation = [], postValidation = identity }


field : Field -> Effect -> Effect
field f t =
    { t | params = t.params ++ [ f ] }


validator : Validator String (Dict String Int) -> Effect -> Effect
validator v t =
    { t | validation = t.validation ++ [ v ] }


finally : (Dict String Int -> Dict String Int) -> Effect -> Effect
finally f t =
    { t | postValidation = f }


{-| An Instance is a Type coupled with a Dict.Dict representing the values of
each Field. Instances should be used with caution, because they may or may not
be constructed correctly. To ensure a proper instance is passed, require a
`Validate.Valid Instance`.
-}
type alias Instance =
    { type_ : Effect
    , values : Dict.Dict String Int
    }


{-| Retrieves the default values for the given effect type.
-}
defaults : Effect -> Dict String Int
defaults e =
    e.params
        |> List.map (\p -> ( p.id, p.default ))
        |> Dict.fromList



-- VALIDATION


{-| A validator for checking if a field is within its defined range in an instance.
-}
rangeValidator : Field -> Validator String Instance
rangeValidator f =
    Validate.ifTrue
        (\d ->
            case Dict.get f.id d.values of
                Just i ->
                    i < f.min || i > f.max

                Nothing ->
                    True
        )
        ("The value for setting \""
            ++ f.name
            ++ "\" is out of range. Put it in the range ["
            ++ String.fromInt f.min
            ++ ", "
            ++ String.fromInt f.max
            ++ "]."
        )


{-| A validator for checking if a field is present in an instance.
-}
presenceValidator : Field -> Validator String Instance
presenceValidator f =
    Validate.ifNothing (\i -> Dict.get f.id i.values) ("Missing field " ++ f.id)


{-| A validator that applies any type-specific checks for an instance.
-}
effectValidator : Effect -> Validator String Instance
effectValidator t =
    Validate.fromErrors
        (\i ->
            case Validate.validate (Validate.all t.validation) i.values of
                Ok _ ->
                    []

                Err errs ->
                    errs
        )


{-| A validator that runs all necessary checks on an instance and its fields.
-}
instanceValidator : Effect -> Validator String Instance
instanceValidator t =
    [ effectValidator t ]
        |> (++) (List.map presenceValidator t.params)
        |> (++) (List.map rangeValidator t.params)
        |> Validate.all


{-| Validates an instance, returning a list of human-friendly errors or a
`Validate.Valid Instance`.
-}
validateInstance : Instance -> Result (List String) (Validate.Valid Instance)
validateInstance i =
    Validate.validate (instanceValidator i.type_) i


validateAll : List Instance -> Result (List (List String)) (List (Validate.Valid Instance))
validateAll effects =
    let
        result =
            List.map (\e -> Validate.validate (instanceValidator e.type_) e) effects

        oks =
            List.filterMap
                (\r ->
                    case r of
                        Ok v ->
                            Just v

                        _ ->
                            Nothing
                )
                result

        errs =
            List.filterMap
                (\r ->
                    case r of
                        Err e ->
                            Just e

                        _ ->
                            Nothing
                )
                result
    in
    if List.length oks == List.length effects then
        Ok oks

    else
        Err errs
