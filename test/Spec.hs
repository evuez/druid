import Control.Monad.Writer (runWriter, writer)
import qualified Expr as E (EExpr(..), Meta(..), WExpr(..))
import Lib (parseAST, reify)
import qualified Parser as P (ParseError, parser)
import Test.Hspec
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse, shouldSucceedOn)
import Text.Megaparsec (parse)

main :: IO ()
main =
  hspec $ do
    describe "ast parser" $ do
      it "parses a non qualified function call with no args" $
        parseAndReify "{:sum, [], []}" `shouldBe`
        Right (E.NonQualifiedCall "sum" [])
      it "parses a non qualified function call for an operator" $
        parseAndReify "{:+, [], [1, 2]}" `shouldBe`
        Right (E.NonQualifiedCall "+" [w (E.Integer 1), w (E.Integer 2)])
      it "parses a variable" $
        parseAndReify "{:sum, [], Elixir}" `shouldBe` Right (E.Variable "sum")
      it "parses a remote call" $
        parseAndReify "{{:., [], [{:foo, [], Elixir}, :bar]}, [], [1, 2, 3]}" `shouldBe`
        Right
          (E.QualifiedCall
             (w $ E.Variable "foo")
             "bar"
             [w (E.Integer 1), w (E.Integer 2), w (E.Integer 3)])
      it "parses an anonymous function call" $
        parseAndReify "{{:., [], [{:foo, [], Elixir}]}, [], [1, 2, 3]}" `shouldBe`
        Right
          (E.AnonymousCall
             (w $ E.Variable "foo")
             [w (E.Integer 1), w (E.Integer 2), w (E.Integer 3)])
      it "parses an alias" $
        parseAndReify "{:__aliases__, [], [:Foo, :Bar, :Baz]}" `shouldBe`
        Right (E.Alias (w (E.Atom "Foo"), ["Bar", "Baz"]))
      it "parses an alias starting with __MODULE__" $
        parseAndReify
          "{:__aliases__, [], [{:__MODULE__, [], Elixir}, :Bar, :Baz]}" `shouldBe`
        Right (E.Alias (w (E.Variable "__MODULE__"), ["Bar", "Baz"]))
      it "parses a list" $
        parseAndReify "[1, 2, 3]" `shouldBe`
        Right (E.List [w (E.Integer 1), w (E.Integer 2), w (E.Integer 3)])
      it "parses a keywords list" $
        parseAndReify "[a: 1, b: 2]" `shouldBe`
        Right
          (E.List
             [ w $ E.Tuple [w (E.Atom "a"), w (E.Integer 1)]
             , w $ E.Tuple [w (E.Atom "b"), w (E.Integer 2)]
             ])
      it "parses a pair" $
        parseAndReify "{1, 2}" `shouldBe`
        Right (E.Tuple [w (E.Integer 1), w (E.Integer 2)])
      it "parses a tuple" $
        parseAndReify "{:{}, [], [1, 2, 3]}" `shouldBe`
        Right (E.Tuple [w (E.Integer 1), w (E.Integer 2), w (E.Integer 3)])
      it "parses a binary" $
        parseAndReify "{:<<>>, [], [1, 2, 3]}" `shouldBe`
        Right (E.Binary [w (E.Integer 1), w (E.Integer 2), w (E.Integer 3)])
      it "parses a map" $
        parseAndReify "{:%{}, [], [{1, 2}, {3, 4}]}" `shouldBe`
        Right
          (E.Map
             [ (w (E.Integer 1), w (E.Integer 2))
             , (w (E.Integer 3), w (E.Integer 4))
             ])
      it "parses a block" $
        parseAndReify "{:__block__, [], [1, 2, 3]}" `shouldBe`
        Right (E.Block [w (E.Integer 1), w (E.Integer 2), w (E.Integer 3)])
      it "parses a case" $
        parseAndReify
          "{:case, [], [1, [do: [{:->, [], [[2], 3]}, {:->, [], [[4], 5]}]]]}" `shouldBe`
        Right
          (E.NonQualifiedCall
             "case"
             [ w (E.Integer 1)
             , w $
               E.List
                 [ w $
                   E.Tuple
                     [ w $ E.Atom "do"
                     , w $
                       E.List
                         [ w $
                           E.NonQualifiedCall
                             "->"
                             [w $ E.List [w (E.Integer 2)], w (E.Integer 3)]
                         , w $
                           E.NonQualifiedCall
                             "->"
                             [w $ E.List [w (E.Integer 4)], w (E.Integer 5)]
                         ]
                     ]
                 ]
             ])
      it "parses a cond" $
        parseAndReify "{:cond, [], [[do: [{:->, [], [[true], false]}]]]}" `shouldBe`
        Right
          (E.NonQualifiedCall
             "cond"
             [ w $
               E.List
                 [ w $
                   E.Tuple
                     [ w $ E.Atom "do"
                     , w $
                       E.List
                         [ w $
                           E.NonQualifiedCall
                             "->"
                             [ w $ E.List [w (E.Atom "true")]
                             , w (E.Atom "false")
                             ]
                         ]
                     ]
                 ]
             ])
      it "parses a multi-head anonymous function" $
        parseAndReify
          "{:fn, [], [{:->, [], [[1, 2], 3]}, {:->, [], [[4, 5], 6]}]}" `shouldBe`
        Right
          (E.Fn
             [ (w $
                E.NonQualifiedCall
                  "->"
                  [ w $ E.List [w (E.Integer 1), w (E.Integer 2)]
                  , w (E.Integer 3)
                  ])
             , w $
               E.NonQualifiedCall
                 "->"
                 [ w $ E.List [w (E.Integer 4), w (E.Integer 5)]
                 , w (E.Integer 6)
                 ]
             ])
    describe "ast parser with meta" $ do
      it
        "parses a non qualified function call with no args and saves the metadata" $
        wParseAndReify "{:sum, [line: 15], []}" `shouldBe`
        (Right $ w' (E.NonQualifiedCall "sum" []) (E.Meta 15))

parseAndReify :: String -> Either P.ParseError E.EExpr
parseAndReify a = fst . runWriter <$> (fmap reify $ parseAST a)

wParseAndReify :: String -> Either P.ParseError E.WExpr
wParseAndReify a = fmap reify $ parseAST a

w :: E.EExpr -> E.WExpr
w e = writer (e, E.Empty)

w' :: E.EExpr -> E.Meta -> E.WExpr
w' e m = writer (e, m)
