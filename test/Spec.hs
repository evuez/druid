import Control.Monad.Writer (runWriter, writer)
import qualified Expr.AST as A (reify)
import qualified Expr.Base as B (BlockVal(..), Expr(..), WExpr)
import Lib (parseAST)
import qualified Meta as M (Meta(..))
import qualified Parser as P (ParseError)
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "ast parser" $ do
      it "parses a non qualified function call with no args" $
        parseAndReify "{:sum, [], []}" `shouldBe`
        Right (B.NonQualifiedCall "sum" [])
      it "parses a non qualified function call for an operator" $
        parseAndReify "{:+, [], [1, 2]}" `shouldBe`
        Right (B.NonQualifiedCall "+" [w (B.Integer 1), w (B.Integer 2)])
      it "parses a variable" $
        parseAndReify "{:sum, [], Elixir}" `shouldBe` Right (B.Variable "sum")
      it "parses a remote call" $
        parseAndReify "{{:., [], [{:foo, [], Elixir}, :bar]}, [], [1, 2, 3]}" `shouldBe`
        Right
          (B.QualifiedCall
             (w $ B.Variable "foo")
             "bar"
             [w (B.Integer 1), w (B.Integer 2), w (B.Integer 3)])
      it "parses an anonymous function call" $
        parseAndReify "{{:., [], [{:foo, [], Elixir}]}, [], [1, 2, 3]}" `shouldBe`
        Right
          (B.AnonymousCall
             (w $ B.Variable "foo")
             [w (B.Integer 1), w (B.Integer 2), w (B.Integer 3)])
      it "parses an alias" $
        parseAndReify "{:__aliases__, [], [:Foo, :Bar, :Baz]}" `shouldBe`
        Right (B.Alias (w (B.Atom "Foo"), ["Bar", "Baz"]))
      it "parses an alias starting with __MODULE__" $
        parseAndReify
          "{:__aliases__, [], [{:__MODULE__, [], Elixir}, :Bar, :Baz]}" `shouldBe`
        Right (B.Alias (w (B.Variable "__MODULE__"), ["Bar", "Baz"]))
      it "parses a list" $
        parseAndReify "[1, 2, 3]" `shouldBe`
        Right (B.List [w (B.Integer 1), w (B.Integer 2), w (B.Integer 3)])
      it "parses a keywords list" $
        parseAndReify "[a: 1, b: 2]" `shouldBe`
        Right
          (B.List
             [ w $ B.Tuple [w (B.Atom "a"), w (B.Integer 1)]
             , w $ B.Tuple [w (B.Atom "b"), w (B.Integer 2)]
             ])
      it "parses a pair" $
        parseAndReify "{1, 2}" `shouldBe`
        Right (B.Tuple [w (B.Integer 1), w (B.Integer 2)])
      it "parses a tuple" $
        parseAndReify "{:{}, [], [1, 2, 3]}" `shouldBe`
        Right (B.Tuple [w (B.Integer 1), w (B.Integer 2), w (B.Integer 3)])
      it "parses a binary" $
        parseAndReify "{:<<>>, [], [1, 2, 3]}" `shouldBe`
        Right (B.Binary [w (B.Integer 1), w (B.Integer 2), w (B.Integer 3)])
      it "parses a map" $
        parseAndReify "{:%{}, [], [{1, 2}, {3, 4}]}" `shouldBe`
        Right
          (B.Map
             [ (w (B.Integer 1), w (B.Integer 2))
             , (w (B.Integer 3), w (B.Integer 4))
             ])
      it "parses a block" $
        parseAndReify "{:__block__, [], [1, 2, 3]}" `shouldBe`
        Right
          (B.Block $
           B.BlockVal [w (B.Integer 1), w (B.Integer 2), w (B.Integer 3)])
      it "parses a case" $
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
      it "parses a cond" $
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
      it "parses a multi-head anonymous function" $
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
    describe "ast parser with meta" $ do
      it
        "parses a non qualified function call with no args and saves the metadata" $
        wParseAndReify "{:sum, [line: 15], []}" `shouldBe`
        (Right $ w' (B.NonQualifiedCall "sum" []) (M.Meta 15 []))

parseAndReify :: String -> Either P.ParseError B.Expr
parseAndReify a = fst . runWriter <$> (fmap A.reify $ parseAST a)

wParseAndReify :: String -> Either P.ParseError B.WExpr
wParseAndReify a = fmap A.reify $ parseAST a

w :: B.Expr -> B.WExpr
w e = writer (e, M.Empty)

w' :: B.Expr -> M.Meta -> B.WExpr
w' e m = writer (e, m)
