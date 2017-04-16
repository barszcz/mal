module Reader exposing (readStr)

import Parser exposing (Parser, (|.), (|=), zeroOrMore, repeat, lazy, oneOf, succeed, symbol)
import Parser.LanguageKit exposing (variable)
import Set exposing (Set)
import Char
import Types exposing (..)


readStr : String -> MalVal
readStr str =
    let
        result =
            Parser.run readForm str
    in
        case result of
            Err _ ->
                MalNil

            Ok v ->
                v


readForm : Parser MalVal
readForm =
    Parser.succeed identity
        |. whitespace
        |= lazy
            (\_ ->
                oneOf
                    [ readList
                    , readAtom
                    ]
            )
        |. whitespace


readList : Parser MalVal
readList =
    Parser.succeed MalList
        |. symbol "("
        |. whitespace
        |= repeat zeroOrMore (lazy (\() -> readForm))
        |. whitespace
        |. symbol ")"


readAtom : Parser MalVal
readAtom =
    oneOf
        [ int
        , malSymbol
        ]


malSymbol : Parser MalVal
malSymbol =
    succeed MalSymbol
        |= Parser.keep Parser.oneOrMore isSymbolChar


int : Parser MalVal
int =
    succeed MalInt
        |= Parser.int


isSymbolChar : Char -> Bool
isSymbolChar c =
    Char.isUpper c
        || Char.isLower c
        || Char.isDigit c
        || isNonSpecialChar c


isNonSpecialChar : Char -> Bool
isNonSpecialChar c =
    Set.member c nonSpecialChars


nonSpecialChars : Set Char
nonSpecialChars =
    "!#$%&|*+-/:<=>?@^_~"
        |> String.toList
        |> Set.fromList


whitespace : Parser ()
whitespace =
    Parser.ignore zeroOrMore (\c -> Set.member c whitespaceChars)


whitespaceChars : Set Char
whitespaceChars =
    ", \n"
        |> String.toList
        |> Set.fromList
