import qualified Expr as E (EExpr(..))
import Lib (compact, parseAST)
import qualified Parser as P (ParseError, parser)
import Test.Hspec
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse, shouldSucceedOn)
import Text.Megaparsec (parse)

main :: IO ()
main =
  hspec $ do
    describe "ast parser" $ do
      it "parses a non qualified function call with no args" $ do
        parseAndCompact "{:sum, [], []}" `shouldBe`
          Right (E.NonQualifiedCall "sum" [])
      it "parses a non qualified function call for an operator" $ do
        parseAndCompact "{:+, [], [1, 2]}" `shouldBe`
          Right (E.NonQualifiedCall "+" [E.Integer 1, E.Integer 2])
      it "parses a variable" $ do
        parseAndCompact "{:sum, [], Elixir}" `shouldBe` Right (E.Variable "sum")
      it "parses a remote call" $ do
        parseAndCompact "{{:., [], [{:foo, [], Elixir}, :bar]}, [], [1, 2, 3]}" `shouldBe`
          Right (E.Atom "key")
      it "parses an anonymous function call" $ do
        parseAndCompact "{{:., [], [{:foo, [], Elixir}]}, [], [1, 2, 3]}" `shouldBe`
          Right (E.Atom "key")
      it "parses an alias" $ do
        parseAndCompact "{:__aliases__, [], [:Foo, :Bar, :Baz]}" `shouldBe`
          Right (E.Alias ["Foo", "Bar", "Baz"])
      it "parses an alias starting with __MODULE__" $ do
        parseAndCompact
          "{:__aliases__, [], [{:__MODULE__, [], Elixir}, :Bar, :Baz]}" `shouldBe`
          Right (E.Alias ["Foo", "Bar", "Baz"])
      it "parses a list" $ do
        parseAndCompact "[1, 2, 3]" `shouldBe`
          Right (E.List [E.Integer 1, E.Integer 2, E.Integer 3])
      it "parses a pair" $ do
        parseAndCompact "{1, 2}" `shouldBe`
          Right (E.Tuple [E.Integer 1, E.Integer 2])
      it "parses a tuple" $ do
        parseAndCompact "{:{}, [], [1, 2, 3]}" `shouldBe`
          Right (E.Tuple [E.Integer 1, E.Integer 2, E.Integer 3])
      it "parses a binary" $ do
        parseAndCompact "{:<<>>, [], [1, 2, 3]}" `shouldBe`
          Right (E.Binary [E.Integer 1, E.Integer 2, E.Integer 3])
      it "parses a map" $ do
        parseAndCompact "{:%{}, [], [{1, 2}, {3, 4}]}" `shouldBe`
          Right (E.Map [(E.Integer 1, E.Integer 2), (E.Integer 3, E.Integer 4)])
      it "parses a block" $ do
        parseAndCompact "{:__block__, [], [1, 2, 3]}" `shouldBe`
          Right (E.Block [E.Integer 1, E.Integer 2, E.Integer 3])
      it "parses a case" $ do
        parseAndCompact "{:case, [], [1, [do: [{:->, [], [[2], 3]}, {:->, [], [[4], 5]}]]]}" `shouldBe` Right (E.NonQualifiedCall "case" [ E.Integer 1 , E.List [E.Tuple [E.Atom "do", E.List [E.NonQualifiedCall "->" [E.List [E.Integer 2], E.Integer 3], E.NonQualifiedCall "->" [E.List [E.Integer 4], E.Integer 5]]]]])
      it "parses a cond" $ do
        parseAndCompact "{:cond, [], [[do: [{:->, [], [[true], false]}]]]}" `shouldBe`
          Right (E.Atom "key")
      it "parses a multi-head anonymous function" $ do
        parseAndCompact
          "{:fn, [], [{:->, [], [[1, 2], 3]}, {:->, [], [[4, 5], 6]}]}" `shouldBe`
          Right (E.Atom "key")

parseAndCompact :: String -> Either P.ParseError E.EExpr
parseAndCompact a = fmap compact $ parseAST a
