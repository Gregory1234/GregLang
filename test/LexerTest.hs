{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module LexerTest where

import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Tasty.HUnit

import           GL.Token
import           GL.Lexer
import           GL.Utils
import           TokenTest

lexerTests :: [TestTree]
lexerTests =
  [ testProperty "lexS . getKeyword == id" $ \k -> lexS (getKeyword k) === k
  , testProperty "lexS . spellToken == id" $ \k -> lexS (spellToken k) === k
  , testProperty "lexemes of an empty file have the correct filename"
    $ \fn -> lexGregLang fn "" === Right (mkLocTokens fn [(TBegin, "", "")])
  , testCase "lexer gives TBegin the whitespace"
    $   lexGregLang "file" "\t   \n\n"
    @?= Right (mkLocTokens "file" [(TBegin, "", "\t   \n\n")])
  , testProperty "lexer puts an identifier into TIdent" $ \i ->
    lexGregLang "file" (getIdent i) === Right
      (mkLocTokens "file" [(TBegin, "", ""), (TIdent i, getIdent i, "")])
  , testCase "lexer puts an integer into TIntLit"
    $   lexGregLang "file" "123"
    @?= Right (mkLocTokens "file" [(TBegin, "", ""), (TIntLit 123, "123", "")])
  , testCase "lexer seperates whitespace from a single identifier"
    $   lexGregLang "file" " \t hello\t\t"
    @?= Right
          (mkLocTokens "file"
                       [(TBegin, "", " \t "), (TIdent "hello", "hello", "\t\t")]
          )
  , testCase "lexer seperates whitespace from multiple identifiers"
    $   lexGregLang "file" " \t hello\t\tworld  "
    @?= Right
          (mkLocTokens
            "file"
            [ (TBegin        , ""     , " \t ")
            , (TIdent "hello", "hello", "\t\t")
            , (TIdent "world", "world", "  ")
            ]
          )
  , testCase "lexer separates tokens without whitespace between them"
    $   lexGregLang "file" "{(world)}"
    @?= Right
          (mkLocTokens
            "file"
            [ (TBegin                             , ""     , "")
            , (TKeyword (BKeyword (Braces, False)), "{"    , "")
            , (TKeyword (BKeyword (Parens, False)), "("    , "")
            , (TIdent "world"                     , "world", "")
            , (TKeyword (BKeyword (Parens, True)) , ")"    , "")
            , (TKeyword (BKeyword (Braces, True)) , "}"    , "")
            ]
          )
  , testCase "lexer supports strings with whitespace"
    $   lexGregLang "file" "\"hello world\""
    @?= Right
          (mkLocTokens
            "file"
            [(TBegin, "", ""), (TStringLit "hello world", "\"hello world\"", "")]
          )
  , testCase "lexer supports strings with escaped quotes"
    $   lexGregLang "file" "\"hello \\\"world\\\"\""
    @?= Right
          (mkLocTokens
            "file"
            [ (TBegin                      , ""                       , "")
            , (TStringLit "hello \"world\"", "\"hello \\\"world\\\"\"", "")
            ]
          )
  , testCase "lexer supports strings with escaped slashes"
    $   lexGregLang "file" "\"hello \\\\\""
    @?= Right
          (mkLocTokens
            "file"
            [(TBegin, "", ""), (TStringLit "hello \\", "\"hello \\\\\"", "")]
          )
  , testCase "lexer supports line comments"
    $   lexGregLang "file" "hello//world \n hey"
    @?= Right
          (mkLocTokens
            "file"
            [ (TBegin        , ""     , "")
            , (TIdent "hello", "hello", "//world \n ")
            , (TIdent "hey"  , "hey"  , "")
            ]
          )
  , testCase "lexer supports block comments"
    $   lexGregLang "file" "hello/*world \n hey*/ hey"
    @?= Right
          (mkLocTokens
            "file"
            [ (TBegin        , ""     , "")
            , (TIdent "hello", "hello", "/*world \n hey*/ ")
            , (TIdent "hey"  , "hey"  , "")
            ]
          )
  , testCase "lexer fails at unfinished strings"
    $   lexGregLang "file" "\"unfinished string"
    @?= Left "couldn't lex \"\\\"unfinished string\""
  , testCase "lexer fails at weird symbols" $ lexGregLang "file" "$" @?= Left
    "couldn't lex \"$\""
  ]