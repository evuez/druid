import Control.Monad.Writer (runWriter, writer)
import qualified Meta as M (Meta(..))
import qualified Expr.Concrete as C (Expr(..), WExpr, BlockVal(..))
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
        Right (C.NonQualifiedCall "sum" [])
      it "parses a non qualified function call for an operator" $
        parseAndReify "{:+, [], [1, 2]}" `shouldBe`
        Right (C.NonQualifiedCall "+" [w (C.Integer 1), w (C.Integer 2)])
      it "parses a variable" $
        parseAndReify "{:sum, [], Elixir}" `shouldBe` Right (C.Variable "sum")
      it "parses a remote call" $
        parseAndReify "{{:., [], [{:foo, [], Elixir}, :bar]}, [], [1, 2, 3]}" `shouldBe`
        Right
          (C.QualifiedCall
             (w $ C.Variable "foo")
             "bar"
             [w (C.Integer 1), w (C.Integer 2), w (C.Integer 3)])
      it "parses an anonymous function call" $
        parseAndReify "{{:., [], [{:foo, [], Elixir}]}, [], [1, 2, 3]}" `shouldBe`
        Right
          (C.AnonymousCall
             (w $ C.Variable "foo")
             [w (C.Integer 1), w (C.Integer 2), w (C.Integer 3)])
      it "parses an alias" $
        parseAndReify "{:__aliases__, [], [:Foo, :Bar, :Baz]}" `shouldBe`
        Right (C.Alias (w (C.Atom "Foo"), ["Bar", "Baz"]))
      it "parses an alias starting with __MODULE__" $
        parseAndReify
          "{:__aliases__, [], [{:__MODULE__, [], Elixir}, :Bar, :Baz]}" `shouldBe`
        Right (C.Alias (w (C.Variable "__MODULE__"), ["Bar", "Baz"]))
      it "parses a list" $
        parseAndReify "[1, 2, 3]" `shouldBe`
        Right (C.List [w (C.Integer 1), w (C.Integer 2), w (C.Integer 3)])
      it "parses a keywords list" $
        parseAndReify "[a: 1, b: 2]" `shouldBe`
        Right
          (C.List
             [ w $ C.Tuple [w (C.Atom "a"), w (C.Integer 1)]
             , w $ C.Tuple [w (C.Atom "b"), w (C.Integer 2)]
             ])
      it "parses a pair" $
        parseAndReify "{1, 2}" `shouldBe`
        Right (C.Tuple [w (C.Integer 1), w (C.Integer 2)])
      it "parses a tuple" $
        parseAndReify "{:{}, [], [1, 2, 3]}" `shouldBe`
        Right (C.Tuple [w (C.Integer 1), w (C.Integer 2), w (C.Integer 3)])
      it "parses a binary" $
        parseAndReify "{:<<>>, [], [1, 2, 3]}" `shouldBe`
        Right (C.Binary [w (C.Integer 1), w (C.Integer 2), w (C.Integer 3)])
      it "parses a map" $
        parseAndReify "{:%{}, [], [{1, 2}, {3, 4}]}" `shouldBe`
        Right
          (C.Map
             [ (w (C.Integer 1), w (C.Integer 2))
             , (w (C.Integer 3), w (C.Integer 4))
             ])
      it "parses a block" $
        parseAndReify "{:__block__, [], [1, 2, 3]}" `shouldBe`
        Right (C.Block $ C.BlockVal [w (C.Integer 1), w (C.Integer 2), w (C.Integer 3)])
      it "parses a case" $
        parseAndReify
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
      it "parses a cond" $
        parseAndReify "{:cond, [], [[do: [{:->, [], [[true], false]}]]]}" `shouldBe`
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
      it "parses a multi-head anonymous function" $
        parseAndReify
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
        (Right $ w' (C.NonQualifiedCall "sum" []) (M.Meta 15))

parseAndReify :: String -> Either P.ParseError C.Expr
parseAndReify a = fst . runWriter <$> (fmap reify $ parseAST a)

wParseAndReify :: String -> Either P.ParseError C.WExpr
wParseAndReify a = fmap reify $ parseAST a

w :: C.Expr -> C.WExpr
w e = writer (e, M.Empty)

w' :: C.Expr -> M.Meta -> C.WExpr
w' e m = writer (e, m)
