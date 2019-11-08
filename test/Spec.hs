import qualified Expr as E (EExpr(..))
import Lib (parseAST, reify)
import qualified Parser as P (ParseError, parser)
import Test.Hspec
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse, shouldSucceedOn)
import Text.Megaparsec (parse)

main :: IO ()
main =
  hspec $ do
    describe "ast parser" $ do
      it "parses a non qualified function call with no args" $ do
        parseAndReify "{:sum, [], []}" `shouldBe`
          Right (E.NonQualifiedCall "sum" [])
      it "parses a non qualified function call for an operator" $ do
        parseAndReify "{:+, [], [1, 2]}" `shouldBe`
          Right (E.NonQualifiedCall "+" [E.Integer 1, E.Integer 2])
      it "parses a variable" $ do
        parseAndReify "{:sum, [], Elixir}" `shouldBe` Right (E.Variable "sum")
      it "parses a remote call" $ do
        parseAndReify "{{:., [], [{:foo, [], Elixir}, :bar]}, [], [1, 2, 3]}" `shouldBe`
          Right
            (E.QualifiedCall
               (E.Variable "foo")
               "bar"
               [E.Integer 1, E.Integer 2, E.Integer 3])
      it "parses an anonymous function call" $ do
        parseAndReify "{{:., [], [{:foo, [], Elixir}]}, [], [1, 2, 3]}" `shouldBe`
          Right
            (E.AnonymousCall
               (E.Variable "foo")
               [E.Integer 1, E.Integer 2, E.Integer 3])
      it "parses an alias" $ do
        parseAndReify "{:__aliases__, [], [:Foo, :Bar, :Baz]}" `shouldBe`
          Right (E.Alias (E.Atom "Foo", ["Bar", "Baz"]))
      it "parses an alias starting with __MODULE__" $ do
        parseAndReify
          "{:__aliases__, [], [{:__MODULE__, [], Elixir}, :Bar, :Baz]}" `shouldBe`
          Right (E.Alias (E.Variable "__MODULE__", ["Bar", "Baz"]))
      it "parses a list" $ do
        parseAndReify "[1, 2, 3]" `shouldBe`
          Right (E.List [E.Integer 1, E.Integer 2, E.Integer 3])
      it "parses a keywords list" $ do
        parseAndReify "[a: 1, b: 2]" `shouldBe`
          Right
            (E.List
               [ E.Tuple [E.Atom "a", E.Integer 1]
               , E.Tuple [E.Atom "b", E.Integer 2]
               ])
      it "parses a pair" $ do
        parseAndReify "{1, 2}" `shouldBe`
          Right (E.Tuple [E.Integer 1, E.Integer 2])
      it "parses a tuple" $ do
        parseAndReify "{:{}, [], [1, 2, 3]}" `shouldBe`
          Right (E.Tuple [E.Integer 1, E.Integer 2, E.Integer 3])
      it "parses a binary" $ do
        parseAndReify "{:<<>>, [], [1, 2, 3]}" `shouldBe`
          Right (E.Binary [E.Integer 1, E.Integer 2, E.Integer 3])
      it "parses a map" $ do
        parseAndReify "{:%{}, [], [{1, 2}, {3, 4}]}" `shouldBe`
          Right (E.Map [(E.Integer 1, E.Integer 2), (E.Integer 3, E.Integer 4)])
      it "parses a block" $ do
        parseAndReify "{:__block__, [], [1, 2, 3]}" `shouldBe`
          Right (E.Block [E.Integer 1, E.Integer 2, E.Integer 3])
      it "parses a case" $ do
        parseAndReify
          "{:case, [], [1, [do: [{:->, [], [[2], 3]}, {:->, [], [[4], 5]}]]]}" `shouldBe`
          Right
            (E.NonQualifiedCall
               "case"
               [ E.Integer 1
               , E.List
                   [ E.Tuple
                       [ E.Atom "do"
                       , E.List
                           [ E.NonQualifiedCall
                               "->"
                               [E.List [E.Integer 2], E.Integer 3]
                           , E.NonQualifiedCall
                               "->"
                               [E.List [E.Integer 4], E.Integer 5]
                           ]
                       ]
                   ]
               ])
      it "parses a cond" $ do
        parseAndReify "{:cond, [], [[do: [{:->, [], [[true], false]}]]]}" `shouldBe`
          Right
            (E.NonQualifiedCall
               "cond"
               [ E.List
                   [ E.Tuple
                       [ E.Atom "do"
                       , E.List
                           [ E.NonQualifiedCall
                               "->"
                               [E.List [E.Atom "true"], E.Atom "false"]
                           ]
                       ]
                   ]
               ])
      it "parses a multi-head anonymous function" $ do
        parseAndReify
          "{:fn, [], [{:->, [], [[1, 2], 3]}, {:->, [], [[4, 5], 6]}]}" `shouldBe`
          Right
            (E.Fn
               [ (E.NonQualifiedCall
                    "->"
                    [E.List [E.Integer 1, E.Integer 2], E.Integer 3])
               , E.NonQualifiedCall
                   "->"
                   [E.List [E.Integer 4, E.Integer 5], E.Integer 6]
               ])

parseAndReify :: String -> Either P.ParseError E.EExpr
parseAndReify a = fmap reify $ parseAST a
