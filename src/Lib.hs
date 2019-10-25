module Lib where

import Control.Applicative ((<|>), empty, liftA2, liftA3, optional)
import Control.Monad (MonadPlus, (<$!>), void)
import Control.Monad.Combinators.Expr (makeExprParser)
import Control.Monad.Combinators.Expr (Operator(InfixL, InfixR, Prefix))
import Data.Char (isSpace)
import Data.Void
import qualified Expr as E (EExpr(..), Operator(..))
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

sepEndBy2 :: MonadPlus f => f a -> f sep -> f [a]
sepEndBy2 p sep = liftA2 (:) (p <* sep) (sepEndBy1 p sep)

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

braces :: Parser a -> Parser a
braces = between (symbol' "{") (symbol' "}")

squareBrackets :: Parser a -> Parser a
squareBrackets = between (symbol' "[") (lexeme (optional C.eol) *> symbol' "]")

integer :: Parser Integer
integer = lexeme L.decimal -- TODO: should allow `_`

float :: Parser Float
float = lexeme L.float -- TODO: should allow `_`

string :: Parser String
string = lexeme strictString

strictString :: Parser String
strictString = C.char '"' *> manyTill L.charLiteral (C.char '"')

charlist :: Parser String
charlist = lexeme strictCharlist

strictCharlist :: Parser String
strictCharlist = C.char '\'' *> manyTill L.charLiteral (C.char '\'')

unquotedAtomHead :: Parser Char
unquotedAtomHead = C.letterChar <|> C.char '_'

unquotedAtomTail :: Parser String
unquotedAtomTail =
  (++) <$> many (C.letterChar <|> C.digitChar <|> oneOf "_@") <*>
  count' 0 1 (oneOf "!?")

unquotedAtomBody :: Parser String
unquotedAtomBody = (:) <$> unquotedAtomHead <*> unquotedAtomTail

unquotedAtom :: Parser String
unquotedAtom =
  lexeme $
  C.char ':' *>
  ((C.string "~~~") <|> (C.string "~>>") <|> (C.string "~>") <|>
   (C.string "|||") <|>
   (C.string "||") <|>
   (C.string "|>") <|>
   (C.string "|") <|>
   (C.string "^^^") <|>
   (C.string "^") <|>
   (C.string "\\") <|>
   (C.string "@") <|>
   (C.string ">>>") <|>
   (C.string ">=") <|>
   (C.string ">") <|>
   (C.string "=~") <|>
   (C.string "===") <|>
   (C.string "==") <|>
   (C.string "=") <|>
   (C.string "<~>") <|>
   (C.string "<~") <|>
   (C.string "<|>") <|>
   (C.string "<>") <|>
   (C.string "<=") <|>
   (C.string "<<~") <|>
   (C.string "<<<") <|>
   (C.string "<-") <|>
   (C.string "<") <|>
   (C.string "::") <|>
   (C.string "/") <|>
   (C.string "..") <|>
   (C.string ".") <|>
   (C.string "->") <|>
   (C.string "--") <|>
   (C.string "-") <|>
   (C.string "-") <|>
   (C.string "++") <|>
   (C.string "+") <|>
   (C.string "+") <|>
   (C.string "*") <|>
   (C.string "&&&") <|>
   (C.string "&&") <|>
   (C.string "&") <|>
   (C.string "!==") <|>
   (C.string "!=") <|>
   (C.string "!") <|>
   unquotedAtomBody) <|>
  (C.string "Access")

quotedAtomBody :: Parser String
quotedAtomBody = charlist <|> string

quotedAtom :: Parser String
quotedAtom = lexeme $ C.char ':' *> quotedAtomBody

specialAtom :: Parser String
specialAtom = lexeme $ symbol "true" <|> symbol "false" <|> symbol "nil"

--
keywords :: (E.EExpr -> E.EExpr -> b) -> Parser b
keywords wrapper = do
  key <- E.Atom <$!> (quotedAtomBody <|> unquotedAtomBody)
  void $ symbol' ":"
  value <- parseAny
  void $ optional (lexeme C.eol)
  pure $ wrapper key value

listKeywords :: Parser E.EExpr
listKeywords = keywords (\k v -> E.Tuple [k, v])

commaSeparated :: Parser a -> Parser [a]
commaSeparated parser = parser `sepBy` (symbol' ",")

--
-- Parser
--
parseAtom :: Parser E.EExpr
parseAtom = E.Atom <$!> (try unquotedAtom <|> quotedAtom <|> specialAtom)

parseInteger :: Parser E.EExpr
parseInteger = E.Integer <$> integer

parseFloat :: Parser E.EExpr
parseFloat = E.Float <$> float

parseString :: Parser E.EExpr
parseString = E.String <$!> string

parseCharlist :: Parser E.EExpr
parseCharlist = E.Charlist <$!> charlist

parseList :: Parser E.EExpr
parseList = E.List <$> (try regularList <|> keywordList)
  where
    regularList = squareBrackets $ commaSeparated parseAny
    keywordList = squareBrackets $ commaSeparated listKeywords

parseTuple :: Parser E.EExpr
parseTuple = E.Tuple <$> braces (commaSeparated parseAny)

parseAny :: Parser E.EExpr
parseAny =
  (parseAtom <?> "struct") <|> (parseInteger <?> "integer") <|>
  (parseFloat <?> "float") <|>
  (parseString <?> "string") <|>
  (parseCharlist <?> "charlist") <|>
  (parseList <?> "list") <|>
  (parseTuple <?> "tuple")

exprParser :: Parser E.EExpr
exprParser = between spaceConsumer eof parseAny

--
-- REPL
--
readAST :: String -> IO ()
readAST s =
  case parse exprParser "" s of
    Left e -> putStr $ errorBundlePretty e
    Right ast -> putStr $ show ast ++ "\n"
