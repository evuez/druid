{-# LANGUAGE OverloadedStrings #-}

import qualified Lib as E (EExpr(..), Operator(..))
import Lib
  ( exprParser
  , listKeywords
  , mapKeywords
  , parseAlias
  , parseAtom
  , parseBinary
  , parseBlock
  , parseCharlist
  , parseExpr
  , parseFloat
  , parseFn
  , parseInteger
  , parseList
  , parseMap
  , parseNonQualifiedCall
  , parseQualifiedCall
  , parseRightArrow
  , parseSigil
  , parseString
  , parseStruct
  , parseTuple
  , parseVariable
  )
import Test.Hspec
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse, shouldSucceedOn)
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
        E.Sigil {E.ident = 'A', E.contents = "b", E.modifiers = "c"}
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
      it "parses a multi-line string" $
        parse parseString "" "\"\"\"a\n\"string\"\"\"" `shouldParse`
        E.String "a\n\"string"
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
      it "parses a spaced non-qualified call with a do block" $ do
        parse parseNonQualifiedCall "" "func! 1, 2 do 1;2; end" `shouldParse`
          E.NonQualifiedCall
          { E.name = "func!"
          , E.args =
              [ E.Integer 1
              , E.Integer 2
              , E.List
                  [E.Tuple [E.Atom "do", E.Block [E.Integer 1, E.Integer 2]]]
              ]
          }
        parse parseNonQualifiedCall "" "func! 1, 2 do 1;2 end" `shouldParse`
          E.NonQualifiedCall
          { E.name = "func!"
          , E.args =
              [ E.Integer 1
              , E.Integer 2
              , E.List
                  [E.Tuple [E.Atom "do", E.Block [E.Integer 1, E.Integer 2]]]
              ]
          }
        parse parseNonQualifiedCall "" "func! 1, 2 do \n1\n2\n end" `shouldParse`
          E.NonQualifiedCall
          { E.name = "func!"
          , E.args =
              [ E.Integer 1
              , E.Integer 2
              , E.List
                  [E.Tuple [E.Atom "do", E.Block [E.Integer 1, E.Integer 2]]]
              ]
          }
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
      it "parses a spaced qualified call with a do block" $ do
        parse parseQualifiedCall "" "Some.Alias.func! 1, 2 do 1;2; end" `shouldParse`
          E.QualifiedCall
          { E.alias' = E.Alias ["Some", "Alias"]
          , E.name = "func!"
          , E.args =
              [ E.Integer 1
              , E.Integer 2
              , E.List
                  [E.Tuple [E.Atom "do", E.Block [E.Integer 1, E.Integer 2]]]
              ]
          }
        parse parseQualifiedCall "" "Some.Alias.func! 1, 2 do 1;2 end" `shouldParse`
          E.QualifiedCall
          { E.alias' = E.Alias ["Some", "Alias"]
          , E.name = "func!"
          , E.args =
              [ E.Integer 1
              , E.Integer 2
              , E.List
                  [E.Tuple [E.Atom "do", E.Block [E.Integer 1, E.Integer 2]]]
              ]
          }
        parse parseQualifiedCall "" "Some.Alias.func! 1, 2 do \n1\n2\n end" `shouldParse`
          E.QualifiedCall
          { E.alias' = E.Alias ["Some", "Alias"]
          , E.name = "func!"
          , E.args =
              [ E.Integer 1
              , E.Integer 2
              , E.List
                  [E.Tuple [E.Atom "do", E.Block [E.Integer 1, E.Integer 2]]]
              ]
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
      it "parses an anonymous function declaration" $
        parse parseFn "" "fn :a -> :b end" `shouldParse`
        E.Fn [E.BinaryOp E.RightArrow (E.List [E.Atom "a"]) (E.Atom "b")]
      it "parses an anonymous function declaration with multiple clauses" $
        parse parseFn "" "fn :a -> :b\n:c -> :d end" `shouldParse`
        E.Fn
          [ E.BinaryOp E.RightArrow (E.List [E.Atom "a"]) (E.Atom "b")
          , E.BinaryOp E.RightArrow (E.List [E.Atom "c"]) (E.Atom "d")
          ]
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
        E.Sigil {E.ident = 'A', E.contents = "b", E.modifiers = "c"}
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
    describe "operator parser" $ do
      it "parses @" $
        parse parseExpr "" "@a" `shouldParse`
        E.UnaryOp E.Attribute (E.Variable "a")
      -- it "parses ." $
        -- parse parseExpr "" "a.b" `shouldParse` E.BinaryOp E.Application (E.Variable "a") (E.Atom "b")
      it "parses the + prefix" $
        parse parseExpr "" "+1" `shouldParse` E.UnaryOp E.Id (E.Integer 1)
      it "parses the - prefix" $
        parse parseExpr "" "-1" `shouldParse` E.UnaryOp E.Negation (E.Integer 1)
      it "parses !" $
        parse parseExpr "" "!true" `shouldParse`
        E.UnaryOp E.Bang (E.Atom "true")
      it "parses ^" $
        parse parseExpr "" "^abc" `shouldParse`
        E.UnaryOp E.Pin (E.Variable "abc")
      it "parses not" $
        parse parseExpr "" "not true" `shouldParse`
        E.UnaryOp E.Not (E.Atom "true")
      it "parses ~~~" $
        parse parseExpr "" "~~~1" `shouldParse`
        E.UnaryOp E.BitwiseNot (E.Integer 1)
      it "parses *" $
        parse parseExpr "" "1 * 2" `shouldParse`
        E.BinaryOp E.Product (E.Integer 1) (E.Integer 2)
      it "parses /" $
        parse parseExpr "" "1 / 2" `shouldParse`
        E.BinaryOp E.Division (E.Integer 1) (E.Integer 2)
      it "parses +" $
        parse parseExpr "" "1 + 2" `shouldParse`
        E.BinaryOp E.Sum (E.Integer 1) (E.Integer 2)
      it "parses -" $
        parse parseExpr "" "1 - 2" `shouldParse`
        E.BinaryOp E.Subtraction (E.Integer 1) (E.Integer 2)
      it "parses ++" $
        parse parseExpr "" "[1] ++ [2]" `shouldParse`
        E.BinaryOp E.Concat (E.List [E.Integer 1]) (E.List [E.Integer 2])
      it "parses --" $
        parse parseExpr "" "[1] -- [2]" `shouldParse`
        E.BinaryOp E.Difference (E.List [E.Integer 1]) (E.List [E.Integer 2])
      it "parses .." $
        parse parseExpr "" "1..2" `shouldParse`
        E.BinaryOp E.Range (E.Integer 1) (E.Integer 2)
      it "parses <>" $
        parse parseExpr "" "\"a\" <> \"b\"" `shouldParse`
        E.BinaryOp E.StringConcat (E.String "a") (E.String "b")
      it "parses ^^^" $
        parse parseExpr "" "1 ^^^ 2" `shouldParse`
        E.BinaryOp E.BitwiseXor (E.Integer 1) (E.Integer 2)
      it "parses in" $
        parse parseExpr "" "1 in [2, 3]" `shouldParse`
        E.BinaryOp E.In (E.Integer 1) (E.List [E.Integer 2, E.Integer 3])
      it "parses not in" $
        parse parseExpr "" "1 not in [2, 3]" `shouldParse`
        E.BinaryOp E.NotIn (E.Integer 1) (E.List [E.Integer 2, E.Integer 3])
      it "parses |>" $
        parse parseExpr "" "1 |> a()" `shouldParse`
        E.BinaryOp
          E.PipeRight
          (E.Integer 1)
          (E.NonQualifiedCall {E.name = "a", E.args = []})
      it "parses <<<" $
        parse parseExpr "" "1 <<< 2" `shouldParse`
        E.BinaryOp E.ShiftLeft (E.Integer 1) (E.Integer 2)
      it "parses >>>" $
        parse parseExpr "" "1 >>> 2" `shouldParse`
        E.BinaryOp E.ShiftRight (E.Integer 1) (E.Integer 2)
      it "parses <<~" $
        parse parseExpr "" "1 <<~ 2" `shouldParse`
        E.BinaryOp E.DoubleChevronTilde (E.Integer 1) (E.Integer 2)
      it "parses ~>>" $
        parse parseExpr "" "1 ~>> 2" `shouldParse`
        E.BinaryOp E.TildeDoubleChevron (E.Integer 1) (E.Integer 2)
      it "parses <~" $
        parse parseExpr "" "1 <~ 2" `shouldParse`
        E.BinaryOp E.ChevronTilde (E.Integer 1) (E.Integer 2)
      it "parses ~>" $
        parse parseExpr "" "1 ~> 2" `shouldParse`
        E.BinaryOp E.TildeChevron (E.Integer 1) (E.Integer 2)
      it "parses <~>" $
        parse parseExpr "" "1 <~> 2" `shouldParse`
        E.BinaryOp E.ChevronTildeChevron (E.Integer 1) (E.Integer 2)
      it "parses <|>" $
        parse parseExpr "" "1 <|> 2" `shouldParse`
        E.BinaryOp E.ChevronPipeChevron (E.Integer 1) (E.Integer 2)
      it "parses <" $
        parse parseExpr "" "1 < 2" `shouldParse`
        E.BinaryOp E.LessThan (E.Integer 1) (E.Integer 2)
      it "parses >" $
        parse parseExpr "" "1 > 2" `shouldParse`
        E.BinaryOp E.GreaterThan (E.Integer 1) (E.Integer 2)
      it "parses <=" $
        parse parseExpr "" "1 <= 2" `shouldParse`
        E.BinaryOp E.LessThanOrEqual (E.Integer 1) (E.Integer 2)
      it "parses >=" $
        parse parseExpr "" "1 >= 2" `shouldParse`
        E.BinaryOp E.GreaterThanOrEqual (E.Integer 1) (E.Integer 2)
      it "parses ===" $
        parse parseExpr "" "1 === 2" `shouldParse`
        E.BinaryOp E.StrictEqual (E.Integer 1) (E.Integer 2)
      it "parses !==" $
        parse parseExpr "" "1 !== 2" `shouldParse`
        E.BinaryOp E.StrictNotEqual (E.Integer 1) (E.Integer 2)
      it "parses ==" $
        parse parseExpr "" "1 == 2" `shouldParse`
        E.BinaryOp E.Equal (E.Integer 1) (E.Integer 2)
      it "parses !=" $
        parse parseExpr "" "1 != 2" `shouldParse`
        E.BinaryOp E.NotEqual (E.Integer 1) (E.Integer 2)
      it "parses =~" $
        parse parseExpr "" "\"a\" =~ ~r/b/" `shouldParse`
        E.BinaryOp
          E.RegexEqual
          (E.String "a")
          (E.Sigil {E.ident = 'r', E.contents = "b", E.modifiers = ""})
      it "parses &&" $
        parse parseExpr "" "a && b" `shouldParse`
        E.BinaryOp E.And (E.Variable "a") (E.Variable "b")
      it "parses &&&" $
        parse parseExpr "" "1 &&& 2" `shouldParse`
        E.BinaryOp E.BitwiseAnd (E.Integer 1) (E.Integer 2)
      it "parses and" $
        parse parseExpr "" "true and false" `shouldParse`
        E.BinaryOp E.BooleanAnd (E.Atom "true") (E.Atom "false")
      it "parses ||" $
        parse parseExpr "" "a || b" `shouldParse`
        E.BinaryOp E.Or (E.Variable "a") (E.Variable "b")
      it "parses |||" $
        parse parseExpr "" "1 ||| 2" `shouldParse`
        E.BinaryOp E.BitwiseOr (E.Integer 1) (E.Integer 2)
      it "parses or" $
        parse parseExpr "" "true or false" `shouldParse`
        E.BinaryOp E.BooleanOr (E.Atom "true") (E.Atom "false")
      it "parses =" $
        parse parseExpr "" "a = 1" `shouldParse`
        E.BinaryOp E.Assignment (E.Variable "a") (E.Integer 1)
      it "parses &" $
        parse parseExpr "" "&a" `shouldParse`
        E.UnaryOp E.Capture (E.Variable "a")
      it "parses |" $
        parse parseExpr "" "1 | 2" `shouldParse`
        E.BinaryOp E.Pipe (E.Integer 1) (E.Integer 2)
      it "parses ::" $
        parse parseExpr "" "a :: 1" `shouldParse`
        E.BinaryOp E.SpecType (E.Variable "a") (E.Integer 1)
      it "parses when" $
        parse parseExpr "" "a when 1" `shouldParse`
        E.BinaryOp E.When (E.Variable "a") (E.Integer 1)
      it "parses <-" $
        parse parseExpr "" "a <- 1" `shouldParse`
        E.BinaryOp E.LeftArrow (E.Variable "a") (E.Integer 1)
      it "parses \\\\" $
        parse parseExpr "" "a \\ 1" `shouldParse`
        E.BinaryOp E.DefaultArg (E.Variable "a") (E.Integer 1)
      it "parses ->" $ do
        parse parseRightArrow "" ":a -> :b" `shouldParse`
          E.BinaryOp E.RightArrow (E.List [E.Atom "a"]) (E.Atom "b")
        parse parseRightArrow "" ":a, :b -> :c" `shouldParse`
          E.BinaryOp E.RightArrow (E.List [E.Atom "a", E.Atom "b"]) (E.Atom "c")
        parse parseRightArrow "" "(:a, :b) -> :c" `shouldParse`
          E.BinaryOp E.RightArrow (E.List [E.Atom "a", E.Atom "b"]) (E.Atom "c")
        parse parseRightArrow "" "(:a, :b) -> :c; :d" `shouldParse`
          E.BinaryOp
            E.RightArrow
            (E.List [E.Atom "a", E.Atom "b"])
            (E.Block [E.Atom "c", E.Atom "d"])
        parse parseExpr "" "(:a -> :b)" `shouldParse`
          E.BinaryOp E.RightArrow (E.List [E.Atom "a"]) (E.Atom "b")
    describe "block parser" $ do
      it "parses an empty expression" $ do
        parse parseBlock "" "" `shouldParse` E.Block []
        parse parseBlock "" " \t" `shouldParse` E.Block []
      it "parses expressions separated by semicolons" $ do
        parse parseBlock "" "1;" `shouldParse` E.Block [(E.Integer 1)]
        parse parseBlock "" "1;2;3" `shouldParse`
          E.Block [(E.Integer 1), (E.Integer 2), (E.Integer 3)]
        parse parseBlock "" "1 ;2" `shouldParse`
          E.Block [(E.Integer 1), (E.Integer 2)]
        parse parseBlock "" "1; 2" `shouldParse`
          E.Block [(E.Integer 1), (E.Integer 2)]
        parse parseBlock "" "1 ; 2" `shouldParse`
          E.Block [(E.Integer 1), (E.Integer 2)]
      it "parses expressions separated by new lines" $ do
        parse parseBlock "" "1\n" `shouldParse` E.Block [(E.Integer 1)]
        parse parseBlock "" "1\n2\n3" `shouldParse`
          E.Block [(E.Integer 1), (E.Integer 2), (E.Integer 3)]
        parse parseBlock "" "1 \n2" `shouldParse`
          E.Block [(E.Integer 1), (E.Integer 2)]
        parse parseBlock "" "1\n 2" `shouldParse`
          E.Block [(E.Integer 1), (E.Integer 2)]
        parse parseBlock "" "1 \n 2" `shouldParse`
          E.Block [(E.Integer 1), (E.Integer 2)]
      it "parses -> clauses" $
        parse parseBlock "" ":a -> 1\n:b -> 2" `shouldParse`
        E.Block
          [ E.BinaryOp E.RightArrow (E.List [E.Atom "a"]) (E.Integer 1)
          , E.BinaryOp E.RightArrow (E.List [E.Atom "b"]) (E.Integer 2)
          ]
      it "parses -> clauses with sub-blocks" $ do
        parse parseBlock "" ":a ->\n1\n\n2" `shouldParse`
          E.Block
            [ E.BinaryOp
                E.RightArrow
                (E.List [E.Atom "a"])
                (E.Block [E.Integer 1, E.Integer 2])
            ]
        parse parseBlock "" ":a ->\na()\n\n2" `shouldParse`
          E.Block
            [ E.BinaryOp
                E.RightArrow
                (E.List [E.Atom "a"])
                (E.Block [E.NonQualifiedCall "a" [], E.Integer 2])
            ]
      it "does not parse 1\\n|> 2 as a block" $ do
        parse parseExpr "" "1\n|> 2" `shouldParse`
          E.BinaryOp E.PipeRight (E.Integer 1) (E.Integer 2)
        parse parseExpr "" "1\n   |> 2" `shouldParse`
          E.BinaryOp E.PipeRight (E.Integer 1) (E.Integer 2)
    describe "function call parser" $ do
      it "parses arguments separated by new lines" $
        parse exprParser "" "func(1,\n2)" `shouldParse`
        E.NonQualifiedCall
        {E.name = "func", E.args = [E.Integer 1, E.Integer 2]}
      it "parses spaced arguments separated by new lines" $
        parse exprParser "" "func 1,\n2" `shouldParse`
        E.NonQualifiedCall
        {E.name = "func", E.args = [E.Integer 1, E.Integer 2]}
      it "parses keywords with no brackets as last arguments" $ do
        parse exprParser "" "func(1, a: 2, b: 3)" `shouldParse`
          E.NonQualifiedCall
          { E.name = "func"
          , E.args =
              [ E.Integer 1
              , E.List
                  [ E.Tuple [E.Atom "a", E.Integer 2]
                  , E.Tuple [E.Atom "b", E.Integer 3]
                  ]
              ]
          }
        parse exprParser "" "func 1, a: 2, b: 3" `shouldParse`
          E.NonQualifiedCall
          { E.name = "func"
          , E.args =
              [ E.Integer 1
              , E.List
                  [ E.Tuple [E.Atom "a", E.Integer 2]
                  , E.Tuple [E.Atom "b", E.Integer 3]
                  ]
              ]
          }
      it "does not parse keywords with no brackets as intermediate arguments" $
        parse exprParser "" `shouldFailOn` "func(1, a: 2, 3)"
    describe "parens and EOL parsing" $ do
      it "parses expressions wrapped in parens" $ do
        parse parseExpr "" "((1) + (2))" `shouldParse`
          E.BinaryOp E.Sum (E.Integer 1) (E.Integer 2)
        parse parseExpr "" "([1 + (2)])" `shouldParse`
          E.List [E.BinaryOp E.Sum (E.Integer 1) (E.Integer 2)]
      it "parses EOL inside expressions" $ do
        parse parseExpr "" "(\n(1)\n+\n(2)\n)" `shouldParse`
          E.BinaryOp E.Sum (E.Integer 1) (E.Integer 2)
        parse parseExpr "" "a =\n 1" `shouldParse`
          E.BinaryOp E.Assignment (E.Variable "a") (E.Integer 1)
    describe "| operator parser" $ do
      it "parses a map update" $
        parse parseExpr "" "%{a | b: 1}" `shouldParse`
        E.MapUpdate (E.Variable "a") [(E.Atom "b", E.Integer 1)]
      it "parses a struct update" $
        parse parseExpr "" "%Struct{a | b: 1}" `shouldParse`
        E.StructUpdate
          (E.Alias ["Struct"])
          (E.Variable "a")
          [(E.Atom "b", E.Integer 1)]
      it "parses a list cons" $
        parse parseExpr "" "[1 | [2, 3]]" `shouldParse`
        E.List
          [E.BinaryOp E.Pipe (E.Integer 1) (E.List [E.Integer 2, E.Integer 3])]
    describe "map parser" $ do
      it "parses key / values separated by new lines" $
        parse parseExpr "" "%{\n1 => 2,\n3 => 4\n }" `shouldParse`
        E.Map [(E.Integer 1, E.Integer 2), (E.Integer 3, E.Integer 4)]
      it "parses keywords-style key / values separated by new lines" $
        parse parseExpr "" "%{\na: 1,\nb: 2\n }" `shouldParse`
        E.Map [(E.Atom "a", E.Integer 1), (E.Atom "b", E.Integer 2)]
    describe "do block parser" $ do
      it "parses a do block with an else clause" $
        parse parseExpr "" "case :a do\n:b\nelse\n:c\nend" `shouldParse`
        E.NonQualifiedCall
          "case"
          [ E.Atom "a"
          , E.List
              [ E.Tuple [E.Atom "do", E.Block [E.Atom "b"]]
              , E.Tuple [E.Atom "else", E.Block [E.Atom "c"]]
              ]
          ]
      it "parses a do block with a catch clause" $
        parse parseExpr "" "case :a do\n:b\ncatch\n:c\nend" `shouldParse`
        E.NonQualifiedCall
          "case"
          [ E.Atom "a"
          , E.List
              [ E.Tuple [E.Atom "do", E.Block [E.Atom "b"]]
              , E.Tuple [E.Atom "catch", E.Block [E.Atom "c"]]
              ]
          ]
      it "parses a do block with a rescue clause" $
        parse parseExpr "" "case :a do\n:b\nrescue\n:c\nend" `shouldParse`
        E.NonQualifiedCall
          "case"
          [ E.Atom "a"
          , E.List
              [ E.Tuple [E.Atom "do", E.Block [E.Atom "b"]]
              , E.Tuple [E.Atom "rescue", E.Block [E.Atom "c"]]
              ]
          ]
      it "parses a do block with an after clause" $
        parse parseExpr "" "case :a do\n:b\nafter\n:c\nend" `shouldParse`
        E.NonQualifiedCall
          "case"
          [ E.Atom "a"
          , E.List
              [ E.Tuple [E.Atom "do", E.Block [E.Atom "b"]]
              , E.Tuple [E.Atom "after", E.Block [E.Atom "c"]]
              ]
          ]
      it "parses a do block with multiple clauses" $
        parse
          parseExpr
          ""
          "case :a do\n:b\nelse\n:c\ncatch\n:d\nrescue\n:e\nafter\n:f\nend" `shouldParse`
        E.NonQualifiedCall
          "case"
          [ E.Atom "a"
          , E.List
              [ E.Tuple [E.Atom "do", E.Block [E.Atom "b"]]
              , E.Tuple [E.Atom "else", E.Block [E.Atom "c"]]
              , E.Tuple [E.Atom "catch", E.Block [E.Atom "d"]]
              , E.Tuple [E.Atom "rescue", E.Block [E.Atom "e"]]
              , E.Tuple [E.Atom "after", E.Block [E.Atom "f"]]
              ]
          ]
    describe "access parser" $ do
      it "parses a[1] as an access call" $
        parse parseExpr "" "a[1]" `shouldParse`
        E.QualifiedCall (E.Alias ["Access"]) "get" [E.Variable "a", E.Integer 1]
      it "parses %{}[1] as an access call" $
        parse parseExpr "" "%{}[1]" `shouldParse`
        E.QualifiedCall (E.Alias ["Access"]) "get" [E.Map [], E.Integer 1]
      it "parses %Struct{}[1] as an access call" $
        parse parseExpr "" "%Struct{}[1]" `shouldParse`
        E.QualifiedCall
          (E.Alias ["Access"])
          "get"
          [E.Struct (E.Alias ["Struct"]) [], E.Integer 1]
    describe "misc" $ do
      it "parses \"def a when a in ~w[b] do\\n1\\nend\"" $
        parse parseExpr "" `shouldSucceedOn` "def a when b in ~W[c] do\n1\nend"
      it "parses \"fn -> 1 end\"" $
        parse parseExpr "" `shouldSucceedOn` "fn -> 1 end"
