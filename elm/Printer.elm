module Printer exposing (prStr)

import Types exposing (..)


prStr : MalVal -> String
prStr v =
    case v of
        MalInt i ->
            toString i

        MalSymbol s ->
            s

        MalList xs ->
            let
                joined =
                    List.map prStr xs
                        |> String.join " "
            in
                "(" ++ joined ++ ")"

        MalNil ->
            "nil"
