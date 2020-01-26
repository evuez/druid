import Control.Monad.Writer (runWriter, writer)
import qualified Expr.AST as A (reify)
import qualified Expr.Base as B (BlockVal(..), Expr(..), ExprW, reify)
import qualified Expr.Concrete as C (BlockVal(..), Expr(..))
import Lib (parseAST)
import qualified Meta as M (Meta(..), MetaW)
import qualified Parser as P (ParseError)
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "ast parser" $ do
      it "parses a non qualified function call with no args" $ do
        parseAndReify "{:sum, [], []}" `shouldBe`
          Right (B.NonQualifiedCall "sum" [])
        parseAndReify2 "{:sum, [], []}" `shouldBe`
          Right (C.NonQualifiedCall "sum" [])
      it "parses a non qualified function call for an operator" $ do
        parseAndReify "{:+, [], [1, 2]}" `shouldBe`
          Right (B.NonQualifiedCall "+" [w (B.Integer 1), w (B.Integer 2)])
        parseAndReify2 "{:+, [], [1, 2]}" `shouldBe`
          Right (C.NonQualifiedCall "+" [w (C.Integer 1), w (C.Integer 2)])
      it "parses a variable" $ do
        parseAndReify "{:sum, [], Elixir}" `shouldBe` Right (B.Variable "sum")
        parseAndReify2 "{:sum, [], Elixir}" `shouldBe` Right (C.Variable "sum")
      it "parses a remote call" $ do
        parseAndReify "{{:., [], [{:foo, [], Elixir}, :bar]}, [], [1, 2, 3]}" `shouldBe`
          Right
            (B.QualifiedCall
               (w $ B.Variable "foo")
               "bar"
               [w (B.Integer 1), w (B.Integer 2), w (B.Integer 3)])
        parseAndReify2 "{{:., [], [{:foo, [], Elixir}, :bar]}, [], [1, 2, 3]}" `shouldBe`
          Right
            (C.QualifiedCall
               (w $ C.Variable "foo")
               "bar"
               [w (C.Integer 1), w (C.Integer 2), w (C.Integer 3)])
      it "parses an anonymous function call" $ do
        parseAndReify "{{:., [], [{:foo, [], Elixir}]}, [], [1, 2, 3]}" `shouldBe`
          Right
            (B.AnonymousCall
               (w $ B.Variable "foo")
               [w (B.Integer 1), w (B.Integer 2), w (B.Integer 3)])
        parseAndReify2 "{{:., [], [{:foo, [], Elixir}]}, [], [1, 2, 3]}" `shouldBe`
          Right
            (C.AnonymousCall
               (w $ C.Variable "foo")
               [w (C.Integer 1), w (C.Integer 2), w (C.Integer 3)])
      it "parses an alias" $ do
        parseAndReify "{:__aliases__, [], [:Foo, :Bar, :Baz]}" `shouldBe`
          Right (B.Alias (w (B.Atom "Foo"), ["Bar", "Baz"]))
        parseAndReify2 "{:__aliases__, [], [:Foo, :Bar, :Baz]}" `shouldBe`
          Right (C.Alias (w (C.Atom "Foo"), ["Bar", "Baz"]))
      it "parses an alias starting with __MODULE__" $ do
        parseAndReify
          "{:__aliases__, [], [{:__MODULE__, [], Elixir}, :Bar, :Baz]}" `shouldBe`
          Right (B.Alias (w (B.Variable "__MODULE__"), ["Bar", "Baz"]))
        parseAndReify2
          "{:__aliases__, [], [{:__MODULE__, [], Elixir}, :Bar, :Baz]}" `shouldBe`
          Right (C.Alias (w (C.Variable "__MODULE__"), ["Bar", "Baz"]))
      it "parses a list" $ do
        parseAndReify "[1, 2, 3]" `shouldBe`
          Right (B.List [w (B.Integer 1), w (B.Integer 2), w (B.Integer 3)])
        parseAndReify2 "[1, 2, 3]" `shouldBe`
          Right (C.List [w (C.Integer 1), w (C.Integer 2), w (C.Integer 3)])
      it "parses a keywords list" $ do
        parseAndReify "[a: 1, b: 2]" `shouldBe`
          Right
            (B.List
               [ w $ B.Tuple [w (B.Atom "a"), w (B.Integer 1)]
               , w $ B.Tuple [w (B.Atom "b"), w (B.Integer 2)]
               ])
        parseAndReify2 "[a: 1, b: 2]" `shouldBe`
          Right
            (C.List
               [ w $ C.Tuple [w (C.Atom "a"), w (C.Integer 1)]
               , w $ C.Tuple [w (C.Atom "b"), w (C.Integer 2)]
               ])
      it "parses a pair" $ do
        parseAndReify "{1, 2}" `shouldBe`
          Right (B.Tuple [w (B.Integer 1), w (B.Integer 2)])
        parseAndReify2 "{1, 2}" `shouldBe`
          Right (C.Tuple [w (C.Integer 1), w (C.Integer 2)])
      it "parses a tuple" $ do
        parseAndReify "{:{}, [], [1, 2, 3]}" `shouldBe`
          Right (B.Tuple [w (B.Integer 1), w (B.Integer 2), w (B.Integer 3)])
        parseAndReify2 "{:{}, [], [1, 2, 3]}" `shouldBe`
          Right (C.Tuple [w (C.Integer 1), w (C.Integer 2), w (C.Integer 3)])
      it "parses a binary" $ do
        parseAndReify "{:<<>>, [], [1, 2, 3]}" `shouldBe`
          Right (B.Binary [w (B.Integer 1), w (B.Integer 2), w (B.Integer 3)])
        parseAndReify2 "{:<<>>, [], [1, 2, 3]}" `shouldBe`
          Right (C.Binary [w (C.Integer 1), w (C.Integer 2), w (C.Integer 3)])
      it "parses a map" $ do
        parseAndReify "{:%{}, [], [{1, 2}, {3, 4}]}" `shouldBe`
          Right
            (B.Map
               [ (w (B.Integer 1), w (B.Integer 2))
               , (w (B.Integer 3), w (B.Integer 4))
               ])
        parseAndReify2 "{:%{}, [], [{1, 2}, {3, 4}]}" `shouldBe`
          Right
            (C.Map
               [ (w (C.Integer 1), w (C.Integer 2))
               , (w (C.Integer 3), w (C.Integer 4))
               ])
      it "parses a block" $ do
        parseAndReify "{:__block__, [], [1, 2, 3]}" `shouldBe`
          Right
            (B.Block $
             B.BlockVal [w (B.Integer 1), w (B.Integer 2), w (B.Integer 3)])
        parseAndReify2 "{:__block__, [], [1, 2, 3]}" `shouldBe`
          Right
            (C.Block $
             C.BlockVal [w (C.Integer 1), w (C.Integer 2), w (C.Integer 3)])
      it "parses a case" $ do
        parseAndReify
          "{:case, [], [1, [do: [{:->, [], [[2], 3]}, {:->, [], [[4], 5]}]]]}" `shouldBe`
          Right
            (B.NonQualifiedCall
               "case"
               [ w (B.Integer 1)
               , w $
                 B.List
                   [ w $
                     B.Tuple
                       [ w $ B.Atom "do"
                       , w $
                         B.List
                           [ w $
                             B.NonQualifiedCall
                               "->"
                               [w $ B.List [w (B.Integer 2)], w (B.Integer 3)]
                           , w $
                             B.NonQualifiedCall
                               "->"
                               [w $ B.List [w (B.Integer 4)], w (B.Integer 5)]
                           ]
                       ]
                   ]
               ])
        parseAndReify2
          "{:case, [], [1, [do: [{:->, [], [[2], 3]}, {:->, [], [[4], 5]}]]]}" `shouldBe`
          Right
            (C.NonQualifiedCall
               "case"
               [ w (C.Integer 1)
               , w $
                 C.List
                   [ w $
                     C.Tuple
                       [ w $ C.Atom "do"
                       , w $
                         C.List
                           [ w $
                             C.NonQualifiedCall
                               "->"
                               [w $ C.List [w (C.Integer 2)], w (C.Integer 3)]
                           , w $
                             C.NonQualifiedCall
                               "->"
                               [w $ C.List [w (C.Integer 4)], w (C.Integer 5)]
                           ]
                       ]
                   ]
               ])
      it "parses a cond" $ do
        parseAndReify "{:cond, [], [[do: [{:->, [], [[true], false]}]]]}" `shouldBe`
          Right
            (B.NonQualifiedCall
               "cond"
               [ w $
                 B.List
                   [ w $
                     B.Tuple
                       [ w $ B.Atom "do"
                       , w $
                         B.List
                           [ w $
                             B.NonQualifiedCall
                               "->"
                               [ w $ B.List [w (B.Atom "true")]
                               , w (B.Atom "false")
                               ]
                           ]
                       ]
                   ]
               ])
        parseAndReify2 "{:cond, [], [[do: [{:->, [], [[true], false]}]]]}" `shouldBe`
          Right
            (C.NonQualifiedCall
               "cond"
               [ w $
                 C.List
                   [ w $
                     C.Tuple
                       [ w $ C.Atom "do"
                       , w $
                         C.List
                           [ w $
                             C.NonQualifiedCall
                               "->"
                               [ w $ C.List [w (C.Atom "true")]
                               , w (C.Atom "false")
                               ]
                           ]
                       ]
                   ]
               ])
      it "parses a multi-head anonymous function" $ do
        parseAndReify
          "{:fn, [], [{:->, [], [[1, 2], 3]}, {:->, [], [[4, 5], 6]}]}" `shouldBe`
          Right
            (B.Fn
               [ (w $
                  B.NonQualifiedCall
                    "->"
                    [ w $ B.List [w (B.Integer 1), w (B.Integer 2)]
                    , w (B.Integer 3)
                    ])
               , w $
                 B.NonQualifiedCall
                   "->"
                   [ w $ B.List [w (B.Integer 4), w (B.Integer 5)]
                   , w (B.Integer 6)
                   ]
               ])
        parseAndReify2
          "{:fn, [], [{:->, [], [[1, 2], 3]}, {:->, [], [[4, 5], 6]}]}" `shouldBe`
          Right
            (C.Fn
               [ (w $
                  C.NonQualifiedCall
                    "->"
                    [ w $ C.List [w (C.Integer 1), w (C.Integer 2)]
                    , w (C.Integer 3)
                    ])
               , w $
                 C.NonQualifiedCall
                   "->"
                   [ w $ C.List [w (C.Integer 4), w (C.Integer 5)]
                   , w (C.Integer 6)
                   ]
               ])
    describe "ast parser with meta" $ do
      it
        "parses a non qualified function call with no args and saves the metadata" $
        wParseAndReify "{:sum, [line: 15], []}" `shouldBe`
        (Right $ w' (B.NonQualifiedCall "sum" []) (M.Meta 15 []))

parseAndReify :: String -> Either P.ParseError B.Expr
parseAndReify a = fst . runWriter <$> (A.reify <$> parseAST a)

parseAndReify2 :: String -> Either P.ParseError C.Expr
parseAndReify2 a = fst . runWriter <$> (B.reify <$> A.reify <$> parseAST a)

wParseAndReify :: String -> Either P.ParseError B.ExprW
wParseAndReify a = fmap A.reify $ parseAST a

w :: a -> M.MetaW a
w e = writer (e, M.Empty)

w' :: B.Expr -> M.Meta -> B.ExprW
w' e m = writer (e, m)
