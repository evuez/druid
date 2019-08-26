{-# LANGUAGE OverloadedStrings #-}

import qualified Lib as E (EExpr(..))
import Lib
  ( listKeywords
  , mapKeywords
  , parseAlias
  , parseAtom
  , parseBinary
  , parseCharlist
  , parseExpr
  , parseFloat
  , parseInteger
  , parseList
  , parseMap
  , parseNonQualifiedCall
  , parseQualifiedCall
  , parseSigil
  , parseString
  , parseStruct
  , parseTuple
  , parseVariable
  )
import Test.Hspec
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse)
import Text.Megaparsec (parse)

main :: IO ()
main =
  hspec $ do
    describe "parser helpers" $ do
      it "parses a keywords-style key / value" $ do
        parse mapKeywords "" "key: 123" `shouldParse`
          (E.Atom "key", E.Integer 123)
        parse listKeywords "" "key: 123" `shouldParse`
          E.Tuple [E.Atom "key", E.Integer 123]

      it "parses a quoted keywords-style key / value" $ do
        parse mapKeywords "" "\"a key\": 123" `shouldParse`
          (E.Atom "a key", E.Integer 123)
        parse listKeywords "" "\"a key\": 123" `shouldParse`
          E.Tuple [E.Atom "a key", E.Integer 123]

      it "parses a ' quoted keywords-style key / value" $ do
        parse mapKeywords "" "'a key': 123" `shouldParse`
          (E.Atom "a key", E.Integer 123)
        parse listKeywords "" "'a key': 123" `shouldParse`
          E.Tuple [E.Atom "a key", E.Integer 123]

    describe "unit parsers" $ do
      it "parses an alias" $
        parse parseAlias "" "Some.Alias" `shouldParse` E.Alias ["Some", "Alias"]

      it "parses a simple alias" $
        parse parseAlias "" "Alias" `shouldParse` E.Alias ["Alias"]

      it "parses a list" $
        parse parseList "" "[1, 2, 3]" `shouldParse`
        E.List [E.Integer 1, E.Integer 2, E.Integer 3]

      it "parses an empty list" $
        parse parseList "" "[]" `shouldParse` E.List []

      it "parses a keywords list" $
        parse parseList "" "[a: 1, b: 2]" `shouldParse`
        E.List
          [E.Tuple [E.Atom "a", E.Integer 1], E.Tuple [E.Atom "b", E.Integer 2]]

      it "does not parse a list with mixed notations" $
        parse parseList "" `shouldFailOn` "[a: 1, 2]"

      it "parses a tuple" $
        parse parseTuple "" "{1, 2, 3}" `shouldParse`
        E.Tuple [E.Integer 1, E.Integer 2, E.Integer 3]

      it "parses an empty tuple" $
        parse parseTuple "" "{}" `shouldParse` E.Tuple []

      it "parses a binary" $
        parse parseBinary "" "<<1, 2, 3>>" `shouldParse`
        E.Binary [E.Integer 1, E.Integer 2, E.Integer 3]

      it "parses an empty binary" $
        parse parseBinary "" "<<>>" `shouldParse` E.Binary []

      it "parses a sigil" $
        parse parseSigil "" "~A{b}c" `shouldParse`
        E.Sigil {E.id = 'A', E.contents = "b", E.modifiers = "c"}

      it "parses a map" $
        parse parseMap "" "%{1 => 2, 3 => 4}" `shouldParse`
        E.Map [(E.Integer 1, E.Integer 2), (E.Integer 3, E.Integer 4)]

      it "parses an empty map" $ parse parseMap "" "%{}" `shouldParse` E.Map []

      it "parses a map with keyword notation" $
        parse parseMap "" "%{a: 1, 'b': 2}" `shouldParse`
        E.Map [(E.Atom "a", E.Integer 1), (E.Atom "b", E.Integer 2)]

      it "does not parse a map with mixed notations" $
        parse parseMap "" `shouldFailOn` "%{a: 1, :'b' => 2}"

      it "parses a struct" $
        parse parseStruct "" "%Some.Alias{:a => 1, :b => 2}" `shouldParse`
        E.Struct
        { E.alias' = E.Alias ["Some", "Alias"]
        , E.map = [(E.Atom "a", E.Integer 1), (E.Atom "b", E.Integer 2)]
        }

      it "parses an empty struct" $
        parse parseStruct "" "%Some.Alias{}" `shouldParse`
        E.Struct {E.alias' = E.Alias ["Some", "Alias"], E.map = []}

      it "parses a struct with keyword notation" $
        parse parseStruct "" "%Some.Alias{a: 1, b: 2}" `shouldParse`
        E.Struct
        { E.alias' = E.Alias ["Some", "Alias"]
        , E.map = [(E.Atom "a", E.Integer 1), (E.Atom "b", E.Integer 2)]
        }

      it "does not parse a struct with mixed notations" $
        parse parseStruct "" `shouldFailOn` "%SomeAlias{a: 1, :b => 2}"

      it "does not parse a struct with string keys" $
        parse parseStruct "" `shouldFailOn` "%SomeAlias{\"a\" => 1, \"b\" => 2}"

      it "parses an atom" $
        parse parseAtom "" ":atom!" `shouldParse` E.Atom "atom!"

      it "parses a quoted atom" $
        parse parseAtom "" ":\"quoted atom!\"" `shouldParse`
        E.Atom "quoted atom!"

      it "parses a ' quoted atom" $
        parse parseAtom "" ":'quoted atom!'" `shouldParse` E.Atom "quoted atom!"

      it "parses true" $ parse parseAtom "" "true" `shouldParse` E.Atom "true"

      it "parses false" $
        parse parseAtom "" "false" `shouldParse` E.Atom "false"

      it "parses nil" $ parse parseAtom "" "nil" `shouldParse` E.Atom "nil"

      it "parses a string" $
        parse parseString "" "\"a string\"" `shouldParse` E.String "a string"

      it "parses an empty string" $
        parse parseString "" "\"\"" `shouldParse` E.String ""

      it "parses a charlist" $
        parse parseCharlist "" "'a charlist'" `shouldParse`
        E.Charlist "a charlist"

      it "parses an empty charlist" $
        parse parseCharlist "" "''" `shouldParse` E.Charlist ""

      it "parses a variable" $
        parse parseVariable "" "a_V4riable!" `shouldParse`
        E.Variable "a_V4riable!"

      it "parses an integer" $
        parse parseInteger "" "1234" `shouldParse` E.Integer 1234

      it "parses a float" $
        parse parseFloat "" "1234.5678" `shouldParse` E.Float 1234.5678

      it "parses a /2 non-qualified call" $
        parse parseNonQualifiedCall "" "func!(1, 2)" `shouldParse`
        E.NonQualifiedCall
        {E.name = "func!", E.args = [E.Integer 1, E.Integer 2]}

      it "parses a /1 non-qualified call" $
        parse parseNonQualifiedCall "" "func!(1)" `shouldParse`
        E.NonQualifiedCall {E.name = "func!", E.args = [E.Integer 1]}

      it "parses a /0 non-qualified call" $
        parse parseNonQualifiedCall "" "func!()" `shouldParse`
        E.NonQualifiedCall {E.name = "func!", E.args = []}

      it "parses a spaced non-qualified call" $
        parse parseNonQualifiedCall "" "func! 1, 2" `shouldParse`
        E.NonQualifiedCall
        {E.name = "func!", E.args = [E.Integer 1, E.Integer 2]}

      it "parses a /2 qualified call" $
        parse parseQualifiedCall "" "Some.Alias.func!(1, 2)" `shouldParse`
        E.QualifiedCall
        { E.alias' = E.Alias ["Some", "Alias"]
        , E.name = "func!"
        , E.args = [E.Integer 1, E.Integer 2]
        }

      it "parses a /1 qualified call" $
        parse parseQualifiedCall "" "Some.Alias.func!(1)" `shouldParse`
        E.QualifiedCall
        { E.alias' = E.Alias ["Some", "Alias"]
        , E.name = "func!"
        , E.args = [E.Integer 1]
        }

      it "parses a /0 qualified call" $
        parse parseQualifiedCall "" "Some.Alias.func!()" `shouldParse`
        E.QualifiedCall
        {E.alias' = E.Alias ["Some", "Alias"], E.name = "func!", E.args = []}

      it "parses a spaced qualified call" $
        parse parseQualifiedCall "" "Some.Alias.func! 1, 2" `shouldParse`
        E.QualifiedCall
        { E.alias' = E.Alias ["Some", "Alias"]
        , E.name = "func!"
        , E.args = [E.Integer 1, E.Integer 2]
        }

      it "parses a quoted qualified call" $
        parse parseQualifiedCall "" "Some.Alias.\"a func!\"(1, 2)" `shouldParse`
        E.QualifiedCall
        { E.alias' = E.Alias ["Some", "Alias"]
        , E.name = "a func!"
        , E.args = [E.Integer 1, E.Integer 2]
        }

      it "parses a ' quoted qualified call" $
        parse parseQualifiedCall "" "Some.Alias.'a func!'(1, 2)" `shouldParse`
        E.QualifiedCall
        { E.alias' = E.Alias ["Some", "Alias"]
        , E.name = "a func!"
        , E.args = [E.Integer 1, E.Integer 2]
        }

    describe "expression parser" $ do
      it "parses an alias" $
        parse parseExpr "" "Some.Alias" `shouldParse` E.Alias ["Some", "Alias"]

      it "parses a simple alias" $
        parse parseExpr "" "Alias" `shouldParse` E.Alias ["Alias"]

      it "parses a list" $
        parse parseExpr "" "[1, 2, 3]" `shouldParse`
        E.List [E.Integer 1, E.Integer 2, E.Integer 3]

      it "parses an empty list" $
        parse parseExpr "" "[]" `shouldParse` E.List []

      it "parses a keywords list" $
        parse parseExpr "" "[a: 1, b: 2]" `shouldParse`
        E.List
          [E.Tuple [E.Atom "a", E.Integer 1], E.Tuple [E.Atom "b", E.Integer 2]]

      it "does not parse a list with mixed notations" $
        parse parseExpr "" `shouldFailOn` "[a: 1, 2]"

      it "parses a tuple" $
        parse parseExpr "" "{1, 2, 3}" `shouldParse`
        E.Tuple [E.Integer 1, E.Integer 2, E.Integer 3]

      it "parses an empty tuple" $
        parse parseExpr "" "{}" `shouldParse` E.Tuple []

      it "parses a binary" $
        parse parseExpr "" "<<1, 2, 3>>" `shouldParse`
        E.Binary [E.Integer 1, E.Integer 2, E.Integer 3]

      it "parses an empty binary" $
        parse parseExpr "" "<<>>" `shouldParse` E.Binary []

      it "parses a sigil" $
        parse parseExpr "" "~A{b}c" `shouldParse`
        E.Sigil {E.id = 'A', E.contents = "b", E.modifiers = "c"}

      it "parses a map" $
        parse parseExpr "" "%{1 => 2, 3 => 4}" `shouldParse`
        E.Map [(E.Integer 1, E.Integer 2), (E.Integer 3, E.Integer 4)]

      it "parses an empty map" $ parse parseExpr "" "%{}" `shouldParse` E.Map []

      it "parses a map with keyword notation" $
        parse parseExpr "" "%{a: 1, 'b': 2}" `shouldParse`
        E.Map [(E.Atom "a", E.Integer 1), (E.Atom "b", E.Integer 2)]

      it "does not parse a map with mixed notations" $
        parse parseExpr "" `shouldFailOn` "%{a: 1, :'b' => 2}"

      it "parses a struct" $
        parse parseExpr "" "%Some.Alias{:a => 1, :b => 2}" `shouldParse`
        E.Struct
        { E.alias' = E.Alias ["Some", "Alias"]
        , E.map = [(E.Atom "a", E.Integer 1), (E.Atom "b", E.Integer 2)]
        }

      it "parses an empty struct" $
        parse parseExpr "" "%Some.Alias{}" `shouldParse`
        E.Struct {E.alias' = E.Alias ["Some", "Alias"], E.map = []}

      it "parses a struct with keyword notation" $
        parse parseExpr "" "%Some.Alias{a: 1, b: 2}" `shouldParse`
        E.Struct
        { E.alias' = E.Alias ["Some", "Alias"]
        , E.map = [(E.Atom "a", E.Integer 1), (E.Atom "b", E.Integer 2)]
        }

      it "does not parse a struct with mixed notations" $
        parse parseExpr "" `shouldFailOn` "%SomeAlias{a: 1, :b => 2}"

      it "does not parse a struct with string keys" $
        parse parseExpr "" `shouldFailOn` "%SomeAlias{\"a\" => 1, \"b\" => 2}"

      it "parses an atom" $
        parse parseExpr "" ":atom!" `shouldParse` E.Atom "atom!"

      it "parses a quoted atom" $
        parse parseExpr "" ":\"quoted atom!\"" `shouldParse`
        E.Atom "quoted atom!"

      it "parses a ' quoted atom" $
        parse parseExpr "" ":'quoted atom!'" `shouldParse` E.Atom "quoted atom!"

      it "parses true" $ parse parseExpr "" "true" `shouldParse` E.Atom "true"

      it "parses false" $
        parse parseExpr "" "false" `shouldParse` E.Atom "false"

      it "parses nil" $ parse parseExpr "" "nil" `shouldParse` E.Atom "nil"

      it "parses a string" $
        parse parseExpr "" "\"a string\"" `shouldParse` E.String "a string"

      it "parses an empty string" $
        parse parseExpr "" "\"\"" `shouldParse` E.String ""

      it "parses a charlist" $
        parse parseExpr "" "'a charlist'" `shouldParse` E.Charlist "a charlist"

      it "parses an empty charlist" $
        parse parseExpr "" "''" `shouldParse` E.Charlist ""

      it "parses a variable" $
        parse parseExpr "" "a_V4riable!" `shouldParse` E.Variable "a_V4riable!"

      it "parses an integer" $
        parse parseExpr "" "1234" `shouldParse` E.Integer 1234

      it "parses a float" $
        parse parseExpr "" "1234.5678" `shouldParse` E.Float 1234.5678

      it "parses a /2 non-qualified call" $
        parse parseExpr "" "func!(1, 2)" `shouldParse`
        E.NonQualifiedCall
        {E.name = "func!", E.args = [E.Integer 1, E.Integer 2]}

      it "parses a /1 non-qualified call" $
        parse parseExpr "" "func!(1)" `shouldParse`
        E.NonQualifiedCall {E.name = "func!", E.args = [E.Integer 1]}

      it "parses a /0 non-qualified call" $
        parse parseExpr "" "func!()" `shouldParse`
        E.NonQualifiedCall {E.name = "func!", E.args = []}

      it "parses a spaced non-qualified call" $
        parse parseExpr "" "func! 1, 2" `shouldParse`
        E.NonQualifiedCall
        {E.name = "func!", E.args = [E.Integer 1, E.Integer 2]}

      it "parses a /2 qualified call" $
        parse parseExpr "" "Some.Alias.func!(1, 2)" `shouldParse`
        E.QualifiedCall
        { E.alias' = E.Alias ["Some", "Alias"]
        , E.name = "func!"
        , E.args = [E.Integer 1, E.Integer 2]
        }

      it "parses a /1 qualified call" $
        parse parseExpr "" "Some.Alias.func!(1)" `shouldParse`
        E.QualifiedCall
        { E.alias' = E.Alias ["Some", "Alias"]
        , E.name = "func!"
        , E.args = [E.Integer 1]
        }

      it "parses a /0 qualified call" $
        parse parseExpr "" "Some.Alias.func!()" `shouldParse`
        E.QualifiedCall
        {E.alias' = E.Alias ["Some", "Alias"], E.name = "func!", E.args = []}

      it "parses a spaced qualified call" $
        parse parseExpr "" "Some.Alias.func! 1, 2" `shouldParse`
        E.QualifiedCall
        { E.alias' = E.Alias ["Some", "Alias"]
        , E.name = "func!"
        , E.args = [E.Integer 1, E.Integer 2]
        }

      it "parses a quoted qualified call" $
        parse parseExpr "" "Some.Alias.\"a func!\"(1, 2)" `shouldParse`
        E.QualifiedCall
        { E.alias' = E.Alias ["Some", "Alias"]
        , E.name = "a func!"
        , E.args = [E.Integer 1, E.Integer 2]
        }

      it "parses a ' quoted qualified call" $
        parse parseExpr "" "Some.Alias.'a func!'(1, 2)" `shouldParse`
        E.QualifiedCall
        { E.alias' = E.Alias ["Some", "Alias"]
        , E.name = "a func!"
        , E.args = [E.Integer 1, E.Integer 2]
        }
