module Parser
  ( parser
  , ParseError
  ) where

import Control.Applicative ((<|>), empty, liftA2, liftA3)
import Control.Monad ((<$!>))
import Data.Void (Void)
import qualified Expr as E (AExpr(..))
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
keywords :: Parser (E.AExpr, E.AExpr)
keywords =
  liftA2
    (\k v -> (k, v))
    (E.AAtom <$!> (quotedAtomBody <|> unquotedAtomBody))
    (symbol ": " *> parseAny)

pair :: Parser (E.AExpr, E.AExpr)
pair = braces (liftA2 (,) (parseAny <* comma) parseAny)

commaSeparated :: Parser a -> Parser [a]
commaSeparated parser' = parser' `sepBy` comma

--
-- Parser
--
parseAtom :: Parser E.AExpr
parseAtom = E.AAtom <$!> (try unquotedAtom <|> quotedAtom <|> specialAtom)

parseInteger :: Parser E.AExpr
parseInteger = E.AInteger <$> integer

parseFloat :: Parser E.AExpr
parseFloat = E.AFloat <$> float

parseString :: Parser E.AExpr
parseString = E.AString <$!> string

parseCharlist :: Parser E.AExpr
parseCharlist = E.ACharlist <$!> charlist

parseList :: Parser E.AExpr
parseList = try emptyList <|> try pairsList <|> try regularList <|> keywordsList
  where
    emptyList = E.AList <$> (squareBrackets $ many empty)
    pairsList = E.AKeywords <$> (squareBrackets $ commaSeparated pair)
    regularList = E.AList <$> (squareBrackets $ commaSeparated parseAny)
    keywordsList = E.AKeywords <$> (squareBrackets $ commaSeparated keywords)

parsePair :: Parser E.AExpr
parsePair = E.APair <$> braces (liftA2 (,) (parseAny <* comma) parseAny)

parseTriple :: Parser E.AExpr
parseTriple =
  E.ATriple <$>
  braces
    (liftA3
       (,,)
       (parseAny <* comma)
       (parseList <* comma)
       (parseList <|> parseAtom))

parseAny :: Parser E.AExpr
parseAny =
  (parseAtom <?> "struct") <|> (parseInteger <?> "integer") <|>
  (parseFloat <?> "float") <|>
  (parseString <?> "string") <|>
  (parseCharlist <?> "charlist") <|>
  (parseList <?> "list") <|>
  (try parseTriple <?> "tripe") <|>
  (parsePair <?> "pair")

parser :: Parser E.AExpr
parser = between spaceConsumer eof parseAny
