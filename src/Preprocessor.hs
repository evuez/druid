module Preprocessor where

import Control.Applicative ((<|>), empty, liftA2, liftA3, optional)
import Control.Monad (MonadPlus, (<$!>), void)
import Control.Monad.Combinators.Expr (makeExprParser)
import Control.Monad.Combinators.Expr (Operator(InfixL, InfixR, Prefix))
import Data.Char (isSpace)
import Data.Void
import Text.Megaparsec
  ( Parsec
  , (<?>)
  , between
  , count'
  , eof
  , lookAhead
  , many
  , manyTill
  , notFollowedBy
  , oneOf
  , parse
  , sepBy
  , sepBy1
  , sepEndBy
  , sepEndBy1
  , takeWhile1P
  , try
  )
import qualified Text.Megaparsec.Char as C
  ( char
  , digitChar
  , eol
  , letterChar
  , lowerChar
  , space1
  , string
  , upperChar
  )
import qualified Text.Megaparsec.Char.Lexer as L
  ( charLiteral
  , decimal
  , float
  , lexeme
  , skipLineComment
  , space
  , symbol
  )
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void String

data Mode
  = Normal
  | Match String

preprocess :: Mode -> String -> String -> String
preprocess Normal (x:xs) p
  | x `elem` ['&', '%', '"', '['] = preprocess (Match [x]) xs (x : p)
  | otherwise = preprocess Normal xs (x : p)
preprocess (Match "&") (' ':xs) p = preprocess (Match "&") xs p
preprocess (Match "&") ('&':xs) p = preprocess (Match "& &") xs ('&' : '(' : p)
preprocess (Match "& &") (' ':xs) p = preprocess Normal xs (' ' : ')' : p)
preprocess (Match "& &") [] p = preprocess Normal [] (')' : p)
preprocess (Match "& &") (x:xs) p = preprocess (Match "& &") xs (x : p)
preprocess (Match "%") ('{':xs) p = preprocess (Match "%{") xs ('{' : p)
preprocess (Match "%") (' ':xs) p = preprocess (Match "%") xs (' ' : p)
preprocess (Match "%") (x:xs) p = preprocess (Match "%") xs (x : p)
preprocess (Match "%{") ('|':'|':'|':xs) p = preprocess (Match "%{") xs ('|' : '|' : '|' : p)
preprocess (Match "%{") ('|':'|':xs) p = preprocess (Match "%{") xs ('|' : '|' : p)
preprocess (Match "%{") ('|':'>':xs) p = preprocess (Match "%{") xs ('>' : '|' : p)
preprocess (Match "%{") ('|':xs) p = preprocess Normal xs ('~' : '|' : '~' : p)
preprocess (Match "%{") (x:xs) p = preprocess (Match "%{") xs (x : p)
preprocess (Match "[") (x:xs) p@('[':';':_) = preprocess Normal xs (x : p)
preprocess (Match "[") (x:xs) p@('[':'\n':_) = preprocess Normal xs (x : p)
preprocess (Match "[") (x:xs) p@('[':' ':_) = preprocess Normal xs (x : p)
preprocess (Match "[") (']':xs) p = preprocess Normal xs (']' : p) -- Syntax error
-- preprocess (Match "[") ('[':xs) p = preprocess (Match "[*") xs (x : p) -- count inner `[`
preprocess (Match "[") (x:xs) p = preprocess (Match "[*") xs (x : p)
preprocess (Match "[*") (']':xs) p = preprocess Normal xs (']' : p)
preprocess (Match "[*") (x:xs) p = preprocess (Match "[*") xs (x : p)
-- preprocess (Match "\"") ('#':'{':xs) p = preprocess (Match "\"#{") xs ('>' : '<' : '"' : p)
-- preprocess (Match "\"") ('"':xs) p = preprocess Normal xs ('"' : p)
-- preprocess (Match "\"") ('\\':'"':xs) p = preprocess (Match "\"") xs ('"' : '\\' : p)
-- preprocess (Match "\"") (x:xs) p = preprocess (Match "\"") xs (x : p)
-- preprocess (Match "\"#{") ('}':xs) p = preprocess (Match "\"") xs ('\"' : '>' : '<' : p)
preprocess (Match _) (x:xs) p = preprocess Normal xs (x : p)
preprocess _ [] p = reverse p
---------------------------
  -------------------


--
-- Lexer
--
spaceConsumer :: Parser ()
spaceConsumer = L.space space lineComment blockComment
  where
    lineComment = L.skipLineComment "#"
    blockComment = empty

spaceConsumer' :: Parser ()
spaceConsumer' = L.space C.space1 lineComment blockComment
  where
    lineComment = L.skipLineComment "#"
    blockComment = empty

space :: Parser ()
space = void $ oneOf " \t"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

lexeme' :: Parser a -> Parser a
lexeme' = L.lexeme spaceConsumer'

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

symbol' :: String -> Parser String
symbol' = L.symbol spaceConsumer'

parens :: Parser a -> Parser a
parens = between (symbol' "(") (symbol' ")")

braces :: Parser a -> Parser a
braces = between (symbol' "{") (symbol' "}")

chevrons :: Parser a -> Parser a
chevrons = between (symbol "<<") (symbol ">>")

squareBrackets :: Parser a -> Parser a
squareBrackets = between (symbol' "[") (lexeme (optional C.eol) *> symbol' "]")

doEnd :: Parser a -> Parser a
doEnd = between (symbol' "do") (symbol' "end")

fnEnd :: Parser a -> Parser a
fnEnd = between (symbol' "fn") (symbol' "end")

identifier :: Parser String
identifier = many (C.letterChar <|> C.digitChar)


-------
  -----------------------

prepAccess :: Parser String
prepAccess = do
  expr <- identifier
  void $ symbol "["
  index <- C.string "i"
  void $ symbol "]"
  pure $ concat ["Access.get(", expr, ", ", index, ")"]
