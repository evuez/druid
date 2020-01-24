module Parser
  ( parser
  , ParseError
  ) where

import Control.Applicative ((<|>), empty, liftA2, liftA3)
import Control.Monad ((<$!>))
import Data.Void (Void)
import qualified Expr.AST as A (Expr(..))
import Text.Megaparsec
  ( Parsec
  , (<?>)
  , between
  , count'
  , eof
  , many
  , manyTill
  , oneOf
  , sepBy
  , try
  )
import qualified Text.Megaparsec.Char as C
  ( char
  , digitChar
  , letterChar
  , space1
  , string
  )
import qualified Text.Megaparsec.Char.Lexer as L
  ( charLiteral
  , decimal
  , float
  , lexeme
  , space
  , symbol
  )
import Text.Megaparsec.Error (ParseErrorBundle)

type Parser = Parsec Void String

type ParseError = ParseErrorBundle String Void

--
-- Lexer
--
spaceConsumer :: Parser ()
spaceConsumer = L.space C.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

squareBrackets :: Parser a -> Parser a
squareBrackets = between (symbol "[") (symbol "]")

comma :: Parser String
comma = symbol ","

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Float
float = lexeme L.float

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
   (C.string "{}") <|>
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
   (C.string "<<>>") <|>
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
   (C.string "++") <|>
   (C.string "+") <|>
   (C.string "+") <|>
   (C.string "*") <|>
   (C.string "&&&") <|>
   (C.string "&&") <|>
   (C.string "&") <|>
   (C.string "%{}") <|>
   (C.string "!==") <|>
   (C.string "!=") <|>
   (C.string "!") <|>
   unquotedAtomBody) <|>
  (C.string "Access") <|>
  (C.string "Elixir")

quotedAtomBody :: Parser String
quotedAtomBody = charlist <|> string

quotedAtom :: Parser String
quotedAtom = lexeme $ C.char ':' *> quotedAtomBody

specialAtom :: Parser String
specialAtom = lexeme $ symbol "true" <|> symbol "false" <|> symbol "nil"

--
keywords :: Parser (A.Expr, A.Expr)
keywords =
  liftA2
    (\k v -> (k, v))
    (A.Atom <$!> (quotedAtomBody <|> unquotedAtomBody))
    (symbol ": " *> parseAny)

pair :: Parser (A.Expr, A.Expr)
pair = braces (liftA2 (,) (parseAny <* comma) parseAny)

commaSeparated :: Parser a -> Parser [a]
commaSeparated parser' = parser' `sepBy` comma

--
-- Parser
--
parseAtom :: Parser A.Expr
parseAtom = A.Atom <$!> (try unquotedAtom <|> quotedAtom <|> specialAtom)

parseInteger :: Parser A.Expr
parseInteger = A.Integer <$> integer

parseFloat :: Parser A.Expr
parseFloat = A.Float <$> float

parseString :: Parser A.Expr
parseString = A.String <$!> string

parseCharlist :: Parser A.Expr
parseCharlist = A.Charlist <$!> charlist

parseList :: Parser A.Expr
parseList =
  try emptyList <|> try pairsList <|> try regularList <|> parseKeywords
  where
    emptyList = A.List <$> (squareBrackets $ many empty)
    pairsList = A.Keywords <$> (squareBrackets $ commaSeparated pair)
    regularList = A.List <$> (squareBrackets $ commaSeparated parseAny)

parseKeywords :: Parser A.Expr
parseKeywords = A.Keywords <$> (squareBrackets $ commaSeparated keywords)

parsePair :: Parser A.Expr
parsePair = A.Pair <$> braces (liftA2 (,) (parseAny <* comma) parseAny)

parseTriple :: Parser A.Expr
parseTriple =
  A.Triple <$>
  braces
    (liftA3
       (,,)
       (parseAny <* comma)
       (parseKeywords <* comma)
       (parseList <|> parseAtom))

parseAny :: Parser A.Expr
parseAny =
  (parseAtom <?> "struct") <|> (parseInteger <?> "integer") <|>
  (parseFloat <?> "float") <|>
  (parseString <?> "string") <|>
  (parseCharlist <?> "charlist") <|>
  (parseList <?> "list") <|>
  (try parseTriple <?> "tripe") <|>
  (parsePair <?> "pair")

parser :: Parser A.Expr
parser = between spaceConsumer eof parseAny
