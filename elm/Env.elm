module Env exposing (..)

import Dict exposing (Dict)
import Types exposing (MalVal)


type alias Env =
    { outer : OuterEnv
    , data : EnvData
    }


type alias EnvData =
    Dict String MalVal


type OuterEnv
    = Outer Env
    | Outermost


set : Env -> String -> MalVal -> Env
set env k v =
    let
        newData =
            Dict.insert env.data k v
    in
        { env | data = newData }


find : Env -> String -> Maybe EnvData
find env k =
    let
        ret =
            Dict.member k env.data
    in
        case ret of
            True ->
                Just env.data

            False ->
                case env.outer of
                    Outer o ->
                        find o

                    Outermost ->
                        Nothing


get env k =
    find env k
        |> Maybe.andThen Dict.get k
        |> Maybe.withDefault MalNil
