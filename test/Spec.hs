{-# LANGUAGE OverloadedStrings #-}

import qualified Lib as E (EExpr(..))
import Lib
  ( parseAlias
  , parseAtom
  , parseCharlist
  , parseExpr
  , parseFloat
  , parseInteger
  , parseList
  , parseMap
  , parseNonQualifiedCall
  , parseQualifiedCall
  , parseString
  , parseStruct
  , parseTuple
  , parseVariable
  )
import Test.Hspec
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (parse)

main :: IO ()
main =
  hspec $ do
    describe "parsers" $ do
      it "parses an alias" $ do
        parse parseAlias "" "Some.Alias" `shouldParse` E.Alias ["Some", "Alias"]

      it "parses a simple alias" $ do
        parse parseAlias "" "Alias" `shouldParse` E.Alias ["Alias"]

      it "parses a list" $ do
        parse parseList "" "[1, 2, 3]" `shouldParse`
          E.List [E.Integer 1, E.Integer 2, E.Integer 3]

      it "parses an empty list" $ do
        parse parseList "" "[]" `shouldParse` E.List []

      it "parses a tuple" $ do
        parse parseTuple "" "{1, 2, 3}" `shouldParse`
          E.Tuple [E.Integer 1, E.Integer 2, E.Integer 3]

      it "parses an emtpy tuple" $ do
        parse parseTuple "" "{}" `shouldParse` E.Tuple []

      it "parses a map" $ do
        parse parseMap "" "%{1 => 2, 3 => 4}" `shouldParse`
          E.Map [(E.Integer 1, E.Integer 2), (E.Integer 3, E.Integer 4)]

      it "parses an empty map" $ do
        parse parseMap "" "%{}" `shouldParse` E.Map []

      it "parses a struct" $ do
        parse parseStruct "" "%Some.Alias{:a => 1, :b => 2}" `shouldParse`
          E.Struct
          { E.alias' = E.Alias ["Some", "Alias"]
          , E.map = [(E.Atom "a", E.Integer 1), (E.Atom "b", E.Integer 2)]
          }

      it "parses an empty struct" $ do
        parse parseStruct "" "%Some.Alias{}" `shouldParse`
          E.Struct {E.alias' = E.Alias ["Some", "Alias"], E.map = []}

      it "parses an atom" $ do
        parse parseAtom "" ":atom!" `shouldParse` E.Atom "atom!"

      it "parses a quoted atom" $ do
        parse parseAtom "" ":\"quoted atom!\"" `shouldParse` E.Atom "quoted atom!"

      it "parses a string" $ do
        parse parseString "" "\"a string\"" `shouldParse` E.String "a string"

      it "parses an empty string" $ do
        parse parseString "" "\"\"" `shouldParse` E.String ""

      it "parses a charlist" $ do
        parse parseCharlist "" "'a charlist'" `shouldParse`
          E.Charlist "a charlist"

      it "parses an empty charlist" $ do
        parse parseCharlist "" "''" `shouldParse` E.Charlist ""

      it "parses a variable" $ do
        parse parseVariable "" "a_V4riable!" `shouldParse`
          E.Variable "a_V4riable!"

      it "parses an integer" $ do
        parse parseInteger "" "1234" `shouldParse` E.Integer 1234

      it "parses a float" $ do
        parse parseFloat "" "1234.5678" `shouldParse` E.Float 1234.5678

      it "parses a non-qualified call" $ do
        parse parseNonQualifiedCall "" "func(1, 2)" `shouldParse`
          E.NonQualifiedCall
          {E.name = "func", E.args = [E.Integer 1, E.Integer 2]}

      it "parses a qualified call" $ do
        parse parseQualifiedCall "" "Some.Alias.func(1, 2)" `shouldParse`
          E.QualifiedCall
          { E.alias' = E.Alias ["Some", "Alias"]
          , E.name = "func"
          , E.args = [E.Integer 1, E.Integer 2]
          }
