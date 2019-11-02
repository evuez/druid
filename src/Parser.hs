module Parser
  ( parser
  ) where

import Control.Applicative ((<|>), empty, liftA2)
import Control.Monad ((<$!>))
import Data.Void
import qualified Expr as E (EExpr(..), Operator(..))
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

type Parser = Parsec Void String

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
  (C.string "Access") <|>
  (C.string "Elixir")

quotedAtomBody :: Parser String
quotedAtomBody = charlist <|> string

quotedAtom :: Parser String
quotedAtom = lexeme $ C.char ':' *> quotedAtomBody

specialAtom :: Parser String
specialAtom = lexeme $ symbol "true" <|> symbol "false" <|> symbol "nil"

--
keywords :: (E.EExpr -> E.EExpr -> b) -> Parser b
keywords wrapper =
  liftA2
    wrapper
    (E.Atom <$!> (quotedAtomBody <|> unquotedAtomBody))
    (symbol ": " *> parseAny)

listKeywords :: Parser E.EExpr
listKeywords = keywords (\k v -> E.Tuple [k, v])

commaSeparated :: Parser a -> Parser [a]
commaSeparated parser' = parser' `sepBy` (symbol ",")

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

parser :: Parser E.EExpr
parser = between spaceConsumer eof parseAny
