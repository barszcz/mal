module Reader exposing (readStr)

import Parser exposing (Parser, (|.), (|=), zeroOrMore, repeat, lazy, oneOf, succeed, symbol)
import Parser.LanguageKit exposing (variable)
import Set exposing (Set)
import Char
import Dict
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
    succeed identity
        |. whitespace
        |= lazy
            (\_ ->
                oneOf
                    [ readReaderMacro
                    , readList
                    , readVector
                    , readAtom
                    ]
            )
        |. whitespace


readList : Parser MalVal
readList =
    succeed MalList
        |= (lazy (\_ -> readSeq "(" ")"))


readVector : Parser MalVal
readVector =
    succeed MalVector
        |= (lazy (\_ -> readSeq "[" "]"))



-- |> Parser.map (\xs -> MalVector (Array.fromList xs))
-- readHashMap : Parser MalVal
-- readHashMap =
--     succeed MalHashMap
--         |. symbol "{"
--         |. whitespace
--         |= repeat zeroOrMore (lazy (\() -> readForm))
--         |. whitespace
--         |. symbol "}"


readSeq : String -> String -> Parser (List MalVal)
readSeq start end =
    succeed identity
        |. symbol start
        |. whitespace
        |= repeat zeroOrMore (lazy (\() -> readForm))
        |. whitespace
        |. symbol end


readAtom : Parser MalVal
readAtom =
    oneOf
        [ negativeInt
        , int
        , nil
        , bool
        , string
        , malSymbol
        ]


negativeInt : Parser MalVal
negativeInt =
    Parser.delayedCommit (symbol "-") <|
        succeed identity
            |= ((Parser.int) |> Parser.map ((*) -1) |> Parser.map MalInt)


int : Parser MalVal
int =
    succeed MalInt
        |= Parser.int


nil : Parser MalVal
nil =
    succeed MalNil
        |. symbol "nil"


bool : Parser MalVal
bool =
    succeed MalBool
        |= oneOf
            [ true
            , false
            ]


true : Parser Bool
true =
    succeed (always True)
        |= symbol "true"


false : Parser Bool
false =
    succeed (always False)
        |= symbol "false"


malSymbol : Parser MalVal
malSymbol =
    succeed MalSymbol
        |= Parser.keep Parser.oneOrMore isSymbolChar


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


string : Parser MalVal
string =
    succeed MalString
        |. symbol "\""
        |= (repeat Parser.zeroOrMore
                (oneOf
                    [ symbol "\\\"" |> Parser.map (always "\"")
                    , symbol "\\\\" |> Parser.map (always "\\")
                    , Parser.keep (Parser.Exactly 1) (\c -> c /= '"')
                    ]
                )
                |> Parser.map (String.join "")
           )
        |. symbol "\""


whitespace : Parser ()
whitespace =
    Parser.ignore zeroOrMore (\c -> Set.member c whitespaceChars)


whitespaceChars : Set Char
whitespaceChars =
    ", \n"
        |> String.toList
        |> Set.fromList


readReaderMacro : Parser MalVal
readReaderMacro =
    succeed MalList
        |= lazy (\() -> oneOf readerMacros)


readerMacros : List (Parser (List MalVal))
readerMacros =
    let
        macrosList =
            [ ( "'", "quote" )
            , ( "`", "quasiquote" )
            , ( "~@", "splice-unquote" )
            , ( "~", "unquote" )
            , ( "@", "deref" )
            ]

        ctor x y =
            MalSymbol x :: [ y ]

        helper ( m, v ) =
            succeed (ctor v)
                |. symbol m
                |= lazy (\() -> readForm)
    in
        List.map helper macrosList
