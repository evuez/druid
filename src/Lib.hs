{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import Control.Applicative ((<|>))
import Control.Exception (Exception, SomeException, fromException, throw)
import Control.Monad ((<$!>), void)
import Control.Monad.Reader (MonadReader, ReaderT(..), ask, local)
import qualified Data.Map as M (Map, foldrWithKey, fromList, insert, lookup)
import qualified Data.Text as T (Text, concat, intercalate, pack, unpack)
import Data.Typeable (Typeable)
import Data.Void
import Text.Megaparsec
  ( ParseErrorBundle
  , Parsec
  , between
  , choice
  , count
  , count'
  , endBy
  , eof
  , many
  , manyTill
  , notFollowedBy
  , oneOf
  , parse
  , sepBy
  , sepBy1
  , skipCount
  , try
  )
import qualified Text.Megaparsec.Char as C
  ( char
  , digitChar
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
  , skipBlockComment
  , skipLineComment
  , space
  , symbol
  )
import Text.Megaparsec.Debug (dbg)
import Text.Megaparsec.Stream (Token)

data EExpr
  = Atom T.Text
  | Alias [T.Text]
  | Integer Integer
  | Float Float
  | String T.Text
  | Charlist T.Text
  | Map [(EExpr, EExpr)]
  | Struct { alias' :: EExpr
           , map :: [(EExpr, EExpr)] }
  | Tuple [EExpr]
  | List [EExpr]
  | Variable T.Text
  | QualifiedCall { alias' :: EExpr
                  , name :: T.Text
                  , args :: [EExpr] }
  | NonQualifiedCall { name :: T.Text
                     , args :: [EExpr] }
  deriving (Typeable, Eq)

type Env = M.Map T.Text EExpr

type Parser = Parsec Void T.Text

type ParseError = ParseErrorBundle T.Text Void

newtype Eval a =
  Eval (ReaderT Env IO a)
  deriving (Monad, Applicative, Functor, MonadReader Env)

instance Show EExpr where
  show = T.unpack . showExpr

-- TODO: Display ast form instead
showExpr :: EExpr -> T.Text
showExpr (Atom atom) = T.concat [":", atom]
showExpr (Alias alias') = T.intercalate "." alias'
showExpr (Integer integer) = T.pack $ show integer
showExpr (Float float) = T.pack $ show float
showExpr (String text) = T.concat ["\"", text, "\""]
showExpr (Charlist text) = T.concat ["'", text, "'"]
showExpr (Variable name) = name
showExpr (Tuple [expr1, expr2]) = T.concat ["{", showExpr expr1, ", ", showExpr expr2, "}"] -- WAT?
showExpr (Tuple exprs) = T.concat ["{:{}, [], [", T.intercalate ", " $ showExpr <$> exprs, "]"]
showExpr (List exprs) =
  T.concat ["[", T.intercalate ", " $ showExpr <$> exprs, "]"]
showExpr (Map keyValues) =
  T.concat
    [ "%{"
    , T.intercalate ", " $
      (\(k, v) -> T.concat [showExpr k, " => ", showExpr v]) <$>
      keyValues
    , "}"
    ]
showExpr (Struct alias' keyValues) =
  T.concat
    [ "%"
    , showExpr alias'
    , "{"
    , T.intercalate ", " $
      (\(k, v) -> T.concat [showExpr k, " => ", showExpr v]) <$>
      keyValues
    , "}"
    ]
showExpr (QualifiedCall alias' name args) =
  T.concat
    [showExpr alias', name, "(", T.intercalate ", " $ showExpr <$> args, ")"]
showExpr (NonQualifiedCall name args) =
  T.concat [name, "(", T.intercalate ", " $ showExpr <$> args, ")"]

showEnv :: Env -> T.Text
showEnv env =
  T.concat $
  M.foldrWithKey (\k v a -> "(" : k : " " : showExpr v : "), " : a) [] env

--
-- Data
--
reservedWords :: [[Char]]
reservedWords =
  [ "true"
  , "false"
  , "nil"
  , "when"
  , "and"
  , "or"
  , "not"
  , "in"
  , "fn"
  , "do"
  , "end"
  , "catch"
  , "rescue"
  , "after"
  , "else"
  ]

--
-- Lexer
--
spaceConsumer :: Parser ()
spaceConsumer = L.space C.space1 lineComment blockComment
  where
    lineComment = L.skipLineComment "#"
    blockComment = L.skipBlockComment "removeme" "removeme" -- TODO: Remove

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: T.Text -> Parser T.Text
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

squareBrackets :: Parser a -> Parser a
squareBrackets = between (symbol "[") (symbol "]")

integer :: Parser Integer
integer = lexeme L.decimal -- should allow `_`

float :: Parser Float
float = lexeme L.float -- should allow `_`

rword :: T.Text -> Parser ()
rword w =
  lexeme . try $ C.string w *> notFollowedBy (C.lowerChar <|> C.char '_')

variable :: Parser String
variable = lexeme identifier

identifier :: Parser String
identifier =
  (++) <$>
  ((:) <$> (C.lowerChar <|> C.char '_') <*>
   many (C.letterChar <|> C.digitChar <|> C.char '_')) <*>
  count' 0 1 (oneOf ("!?" :: [Char]))

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
  (++) <$> many (C.letterChar <|> C.digitChar <|> oneOf ("_@" :: [Char])) <*>
  count' 0 1 (oneOf ("!?" :: [Char]))

unquotedAtom :: Parser String
unquotedAtom =
  lexeme $ C.char ':' *> ((:) <$> unquotedAtomHead <*> unquotedAtomTail)

quotedAtom :: Parser String
quotedAtom = lexeme $ C.char ':' *> (charlist <|> string)

alias :: Parser [String]
alias = lexeme $ aliasChunk `sepBy1` (C.char '.')

aliasChunk :: Parser String
aliasChunk =
  (:) <$> C.upperChar <*> many (C.letterChar <|> C.digitChar <|> C.char '_')

--
-- Parser
--
-- Helpers
keyValue :: Parser (EExpr, EExpr)
keyValue = do
  key <- parseExpr
  void $ symbol "=>"
  value <- parseExpr
  return (key, value)

parensArgs :: Parser [EExpr]
parensArgs = between (symbol "(") (symbol ")") (commaSeparated parseExpr)

spacesArgs :: Parser [EExpr]
spacesArgs = C.char ' ' >> commaSeparated parseExpr

commaSeparated :: Parser a -> Parser [a]
commaSeparated parser = parser `sepBy` (lexeme $ C.char ',')

-- Parsers
listParser :: Parser EExpr
listParser = between spaceConsumer eof parseList

exprParser :: Parser EExpr
exprParser = between spaceConsumer eof parseExpr

parseAlias :: Parser EExpr
parseAlias = Alias <$> fmap T.pack <$> alias

parseList :: Parser EExpr
parseList = List <$> squareBrackets (commaSeparated parseExpr)

parseTuple :: Parser EExpr
parseTuple = Tuple <$> braces (commaSeparated parseExpr)

parseMap :: Parser EExpr
parseMap = do
  void $ symbol "%"
  Map <$> braces (commaSeparated keyValue)

parseStruct :: Parser EExpr
parseStruct = do
  void $ symbol "%"
  alias' <- parseAlias
  Struct alias' <$> braces (commaSeparated keyValue) -- Should only allow atoms for keys, I think?

parseAtom :: Parser EExpr
parseAtom = Atom . T.pack <$!> (unquotedAtom <|> quotedAtom)

parseString :: Parser EExpr
parseString = String . T.pack <$!> string

parseCharlist :: Parser EExpr
parseCharlist = Charlist . T.pack <$!> charlist

parseVariable :: Parser EExpr
parseVariable = Variable . T.pack <$!> variable

parseInteger :: Parser EExpr -- notFollowedByIdentifierStart?
parseInteger = Integer <$> integer

parseFloat :: Parser EExpr -- notFollowedByIdentifierStart?
parseFloat = Float <$> float

parseNonQualifiedCall :: Parser EExpr
parseNonQualifiedCall = do
  name <- T.pack <$> identifier
  args <- parensArgs <|> spacesArgs
  return $ NonQualifiedCall name args

parseQualifiedCall :: Parser EExpr
parseQualifiedCall = do
  alias' <- parseAlias
  void $ C.char '.'
  name <- T.pack <$> (identifier <|> strictString <|> strictCharlist)
  args <- parensArgs <|> spacesArgs
  return $ QualifiedCall alias' name args

parseData :: Parser EExpr
parseData =
  try parseStruct <|> try parseMap <|> parseTuple <|> parseList <|> parseInteger <|>
  parseFloat <|>
  parseAtom <|>
  parseString <|>
  parseCharlist <|>
  parseVariable <|>
  parseAlias

parseExpr :: Parser EExpr
parseExpr = parseData <|> parseNonQualifiedCall <|> parseQualifiedCall

--
-- REPL
--
readExpr :: T.Text -> Either ParseError EExpr
readExpr = parse exprParser ""

readList :: T.Text -> Either ParseError EExpr
readList = parse listParser ""
