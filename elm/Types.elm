module Types exposing (..)

import Dict exposing (Dict)


type MalVal
    = MalList (List MalVal)
    | MalVector (List MalVal)
    | MalHashMap (Dict String MalVal)
    | MalInt Int
    | MalSymbol String
    | MalString String
    | MalNil
    | MalBool Bool
