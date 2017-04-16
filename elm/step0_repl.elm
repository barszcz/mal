port module Mal exposing (..)

import Platform exposing (program)
import Json.Decode


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


read : String -> String
read str =
    str


eval : String -> String
eval str =
    str


print : String -> Cmd Msg
print str =
    output str


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
