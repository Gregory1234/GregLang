{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module LexerTest where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           GL.Lexer
import           GL.Token
import           GL.Utils
import           TokenTest

default (Token)

lexerTests :: [TestTree]
lexerTests =
  [ testProperty "lexemes of an empty file have the correct filename"
    $ \fn -> lexGregLang fn "" === Right (mkLocTokens fn [(TBegin, "", "")])
  , testCase "lexer gives TBegin the whitespace"
    $   lexGregLang "file" "\t   \n\n"
    @?= Right (mkLocTokens "file" [(TBegin, "", "\t   \n\n")])
  , testProperty "lexer puts an identifier into TIdent" $ \i ->
    lexGregLang "file" (getIdent i) === Right
      (mkLocTokens "file" [(TBegin, "", ""), (TIdent i, getIdent i, "")])
  , testProperty "lexer puts an uppercase identifier into TTypeIdent" $ \i ->
    lexGregLang "file" (getClassName i) === Right
      (mkLocTokens "file" [(TBegin, "", ""), (TTypeIdent i, getClassName i, "")]
      )
  , testCase "lexer puts an integer into TIntLit"
    $   lexGregLang "file" "123"
    @?= Right (mkLocTokens "file" [(TBegin, "", ""), (TIntLit 123, "123", "")])
  , testCase "lexer puts an float into TFloatLit"
    $   lexGregLang "file" "123.1"
    @?= Right
          (mkLocTokens "file" [(TBegin, "", ""), (TFloatLit 123.1, "123.1", "")])
  , testCase "lexer puts an float beginning with a dot into TFloatLit"
    $   lexGregLang "file" ".123"
    @?= Right
          (mkLocTokens "file" [(TBegin, "", ""), (TFloatLit 0.123, ".123", "")])
  , testCase "lexer puts an float ending with a dot into TFloatLit"
    $   lexGregLang "file" ".123"
    @?= Right
          (mkLocTokens "file" [(TBegin, "", ""), (TFloatLit 0.123, ".123", "")])
  , testCase "lexer puts a char into TCharLit"
    $   lexGregLang "file" "'a'"
    @?= Right (mkLocTokens "file" [(TBegin, "", ""), (TCharLit 'a', "'a'", "")])
  , testCase "lexer puts a keyword into TKeyword"
    $   lexGregLang "file" "if"
    @?= Right (mkLocTokens "file" [(TBegin, "", ""), (TKeyword "if", "if", "")])
  , testCase "lexer puts multiple separated keywords into TKeywords"
    $   lexGregLang "file" "if else"
    @?= Right
          (mkLocTokens
            "file"
            [ (TBegin         , ""    , "")
            , (TKeyword "if"  , "if"  , " ")
            , (TKeyword "else", "else", "")
            ]
          )
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
            [ (TBegin                           , ""     , "")
            , (TSymbol (BrSym (Braces, OpenB))  , "{"    , "")
            , (TSymbol (BrSym (Parens, OpenB))  , "("    , "")
            , (TIdent "world"                   , "world", "")
            , (TSymbol (BrSym (Parens, ClosedB)), ")"    , "")
            , (TSymbol (BrSym (Braces, ClosedB)), "}"    , "")
            ]
          )
  , testCase "lexer supports strings with whitespace"
    $   lexGregLang "file" "\"hello world\""
    @?= Right
          (mkLocTokens
            "file"
            [(TBegin, "", ""), (TStringLit "hello world", "\"hello world\"", "")]
          )
  , testCase "lexer supports strings with escaped characters"
    $   lexGregLang "file" "\"hello\\r\\n\\t \\\"world\\\"\""
    @?= Right
          (mkLocTokens
            "file"
            [ (TBegin, "", "")
            , ( TStringLit "hello\r\n\t \"world\""
              , "\"hello\\r\\n\\t \\\"world\\\"\""
              , ""
              )
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
    @?= Left "Lexer error"
  , testCase "lexer fails given bad escaped character"
    $   lexGregLang "file" "\"\\e\""
    @?= Left "Lexer error"
  , testCase "lexer fails at weird symbols" $ lexGregLang "file" "$" @?= Left
    "Lexer error"
  ]
