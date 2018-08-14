port module Mal exposing (..)

import Platform exposing (program)
import Json.Decode
import Dict exposing (Dict)
import Reader exposing (readStr)
import Printer exposing (prStr)
import Types exposing (..)


type alias Model =
    ()


type Msg
    = Input String
    | Close


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input str ->
            ( model, rep str )

        Close ->
            ( model, end 0 )


read : String -> MalVal
read =
    readStr


eval : Dict String MalVal -> MalVal -> MalVal
eval env ast =
    case ast of
        MalList [] ->
            ast

        MalList _ ->
            case evalAst env ast of
                MalList (x :: xs) ->
                    case x of
                        MalFunc f ->
                            f xs

                        _ ->
                            x

                _ ->
                    MalNil

        _ ->
            evalAst env ast


evalAst : Dict String MalVal -> MalVal -> MalVal
evalAst env ast =
    case ast of
        MalSymbol sym ->
            case Dict.get sym env of
                Just v ->
                    v

                Nothing ->
                    MalString "nothing in al capone's vault"

        MalList xs ->
            MalList <| List.map (eval env) xs

        MalVector xs ->
            MalVector <| List.map (eval env) xs

        _ ->
            ast


print : MalVal -> Cmd Msg
print v =
    prStr v |> output


rep : String -> Cmd Msg
rep =
    read >> eval replEnv >> print


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ input Input, close (always Close) ]


replEnv : Dict String MalVal
replEnv =
    Dict.fromList
        [ ( "+", MalFunc add )
        , ( "-", MalFunc sub )
        , ( "*", MalFunc mult )
        , ( "/", MalFunc div )
        ]


add : Func
add args =
    case args of
        [ MalInt x, MalInt y ] ->
            MalInt (x + y)

        _ ->
            MalNil


sub : Func
sub args =
    case args of
        [ MalInt x, MalInt y ] ->
            MalInt (x - y)

        _ ->
            MalNil


mult : Func
mult args =
    case args of
        [ MalInt x, MalInt y ] ->
            MalInt (x * y)

        _ ->
            MalNil


div : Func
div args =
    case args of
        [ MalInt x, MalInt y ] ->
            MalInt (x // y)

        _ ->
            MalNil


main =
    program
        { init = ( (), Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }


port input : (String -> msg) -> Sub msg


port output : String -> Cmd msg


port close : ({} -> msg) -> Sub msg


port end : Int -> Cmd msg
