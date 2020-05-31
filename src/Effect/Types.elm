module Effect.Types exposing (all, cut, randomize, remove, repeat, reverse, silence, swap)

import Dict
import Effect exposing (Effect)
import Validate


all : List Effect
all =
    [ swap, randomize, remove, cut, repeat, silence, reverse ]


offsetField : Effect.Field
offsetField =
    { id = "offset"
    , name = "Offset"
    , hint = Just "Number of beats to wait before applying this effect"
    , min = 0
    , default = 0
    , max = 1000
    }


validateSwapPeriod : Validate.Validator String (Dict.Dict String Int)
validateSwapPeriod =
    Validate.ifTrue
        (\v -> Dict.get "x_period" v == Dict.get "y_period" v)
        "Can't swap a beat with itself! Change one of the values below."


swap =
    Effect.effect { id = "swap", name = "Swap", description = "Swaps two beats throughout the entire song." }
        |> Effect.field { id = "x_period", name = "Every", hint = Nothing, min = 1, default = 2, max = 1000 }
        |> Effect.field { id = "y_period", name = "With", hint = Nothing, min = 1, default = 4, max = 1000 }
        |> Effect.field offsetField
        |> Effect.validator validateSwapPeriod


randomize : Effect
randomize =
    Effect.effect { id = "randomize", name = "Randomize", description = "Totally randomizes the order of all beats." }


remove : Effect
remove =
    Effect.effect { id = "remove", name = "Remove", description = "Removes beats entirely." }
        |> Effect.field { id = "period", name = "Every", hint = Nothing, min = 2, default = 2, max = 1000 }


validateCutIndex : Validate.Validator String (Dict.Dict String Int)
validateCutIndex =
    Validate.ifTrue
        (\m -> (Maybe.withDefault 0 <| Dict.get "take_index" m) > (Maybe.withDefault 0 <| Dict.get "denominator" m))
        "Each beat isn't being cut into enough pieces to take that one. Try increasing the number of pieces."


cut : Effect
cut =
    Effect.effect { id = "cut", name = "Cut", description = "Cuts beats into pieces and keeps one of them (i.e. pieces = 2, piece to keep = 2 takes the second half of each beat)." }
        |> Effect.field { id = "period", name = "Every", hint = Nothing, min = 1, default = 2, max = 1000 }
        |> Effect.field offsetField
        |> Effect.field { id = "denominator", name = "Pieces", hint = Nothing, min = 2, default = 2, max = 1000 }
        |> Effect.field { id = "take_index", name = "Piece to Keep", hint = Nothing, min = 1, default = 1, max = 1000 }
        |> Effect.validator validateCutIndex
        |> Effect.finally (Dict.update "take_index" (Maybe.map (\i -> i - 1)))


repeat : Effect
repeat =
    Effect.effect { id = "repeat", name = "Repeat", description = "Repeat beats a certain number of times." }
        |> Effect.field { id = "period", name = "Every", hint = Nothing, min = 1, default = 2, max = 1000 }
        |> Effect.field offsetField
        |> Effect.field { id = "times", name = "Times", hint = Nothing, min = 1, default = 2, max = 1000 }


silence : Effect
silence =
    Effect.effect { id = "silence", name = "Silence", description = "Silences beats, but retains their length." }
        |> Effect.field { id = "period", name = "Every", hint = Nothing, min = 2, default = 2, max = 1000 }
        |> Effect.field offsetField


reverse : Effect
reverse =
    Effect.effect { id = "reverse", name = "Reverse", description = "Reverses individual beats." }
        |> Effect.field { id = "period", name = "Every", hint = Nothing, min = 1, default = 2, max = 1000 }
        |> Effect.field offsetField
