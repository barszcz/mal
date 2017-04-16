module Types exposing (..)


type MalVal
    = MalList (List MalVal)
    | MalInt Int
    | MalSymbol String
    | MalNil
