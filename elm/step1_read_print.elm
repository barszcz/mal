port module Mal exposing (..)

import Platform exposing (program)
import Json.Decode
import Reader exposing (readStr)
import Printer exposing (prStr)
import Types exposing (MalVal)


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
read = readStr


eval : MalVal -> MalVal
eval v =
    v


print : MalVal -> Cmd Msg
print v =
    prStr v |> output


rep : String -> Cmd Msg
rep =
    read >> eval >> print


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ input Input, close (always Close) ]


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
