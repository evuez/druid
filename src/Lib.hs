{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import Control.Applicative ((<|>))
import Control.Exception (Exception, SomeException, fromException, throw)
import Control.Monad ((<$!>), void)
import Control.Monad.Combinators.Expr (makeExprParser)
import qualified Control.Monad.Combinators.Expr as E
  ( Operator(InfixL, InfixR, Prefix)
  )
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
  | Binary [EExpr]
  | Sigil { ident :: Char
          , contents :: T.Text
          , modifiers :: [Char] }
  | Variable T.Text
  | QualifiedCall { alias' :: EExpr
                  , name :: T.Text
                  , args :: [EExpr] }
  | NonQualifiedCall { name :: T.Text
                     , args :: [EExpr] }
  | BinaryOp Operator
             EExpr
             EExpr
  | UnaryOp Operator
            EExpr
  deriving (Typeable, Eq)

data Operator
  = And
  | Application
  | Assignment
  | Attribute
  | Bang
  | BitwiseAnd
  | BitwiseNot
  | BitwiseOr
  | BitwiseXor
  | BooleanAnd
  | BooleanOr
  | Capture
  | ChevronPipeChevron
  | ChevronTilde
  | ChevronTildeChevron
  | Concat
  | DefaultArg
  | Difference
  | Division
  | DoubleChevronTilde
  | Equal
  | GreaterThan
  | GreaterThanOrEqual
  | Id
  | In
  | LeftArrow
  | LessThan
  | LessThanOrEqual
  | Negation
  | Not
  | NotEqual
  | NotIn
  | Or
  | Pin
  | Pipe
  | PipeRight
  | Product
  | Range
  | RegexEqual
  | ShiftLeft
  | ShiftRight
  | SpecType
  | StrictEqual
  | StrictNotEqual
  | StringConcat
  | Subtraction
  | Sum
  | TildeChevron
  | TildeDoubleChevron
  | When
  deriving (Eq)

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
showExpr (Variable name) = T.concat ["{:", name, ", [], Elixir}"]
showExpr (Tuple [expr1, expr2]) =
  T.concat ["{", showExpr expr1, ", ", showExpr expr2, "}"]
showExpr (Tuple exprs) =
  T.concat ["{:{}, [], [", T.intercalate ", " $ showExpr <$> exprs, "]}"]
showExpr (Binary exprs) =
  T.concat ["{:<<>>, [], [", T.intercalate ", " $ showExpr <$> exprs, "]}"]
showExpr (Sigil ident contents modifiers) =
  T.concat
    [ "{:sigil_"
    , T.pack [ident]
    , ", [], [{:<<>>, [], [\""
    , contents
    , "\"]}, '"
    , T.pack modifiers
    , "']}"
    ]
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
    [ "{:., [], [{:"
    , showExpr alias'
    , ", [], Elixir}, :"
    , name
    , "]}, [], ["
    , T.intercalate ", " $ showExpr <$> args
    , "]}"
    ]
showExpr (NonQualifiedCall name args) =
  T.concat ["{:", name, ", [], [", T.intercalate ", " $ showExpr <$> args, "]}"]
showExpr (BinaryOp op a b) =
  T.concat [showExpr a, " ", showOp op, " ", showExpr b]
showExpr (UnaryOp op a) = T.concat [showOp op, showExpr a]

showOp :: Operator -> T.Text
showOp And = "&&"
showOp Application = "."
showOp Assignment = "="
showOp Attribute = "&"
showOp Bang = "!"
showOp BitwiseAnd = "&&&"
showOp BitwiseNot = "~~~"
showOp BitwiseOr = "|||"
showOp BitwiseXor = "^^^"
showOp BooleanAnd = "and"
showOp BooleanOr = "or"
showOp Capture = "&"
showOp ChevronPipeChevron = "<|>"
showOp ChevronTilde = "<~"
showOp ChevronTildeChevron = "<~>"
showOp Concat = "++"
showOp DefaultArg = "\\"
showOp Difference = "--"
showOp Division = "/"
showOp DoubleChevronTilde = "<<~"
showOp Equal = "=="
showOp GreaterThan = ">"
showOp GreaterThanOrEqual = ">="
showOp Id = "+"
showOp In = "in"
showOp LeftArrow = "<-"
showOp LessThan = "<"
showOp LessThanOrEqual = "<="
showOp Negation = "-"
showOp Not = "not"
showOp NotEqual = "!="
showOp NotIn = "not in"
showOp Or = "||"
showOp Pin = "^"
showOp Pipe = "|"
showOp PipeRight = "|>"
showOp Product = "*"
showOp Range = ".."
showOp RegexEqual = "=~"
showOp ShiftLeft = "<<<"
showOp ShiftRight = ">>>"
showOp SpecType = "::"
showOp StrictEqual = "==="
showOp StrictNotEqual = "!=="
showOp StringConcat = "<>"
showOp Subtraction = "-"
showOp Sum = "+"
showOp TildeChevron = "~>"
showOp TildeDoubleChevron = "~>>"
showOp When = "when"

showEnv :: Env -> T.Text
showEnv env =
  T.concat $
  M.foldrWithKey (\k v a -> "(" : k : " " : showExpr v : "), " : a) [] env

--
-- Data
--
reservedWords :: [String]
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
-- Operators
--
opsTable :: [[E.Operator Parser EExpr]]
opsTable =
  [ [prefix "@" Attribute]
  , [infixl' "." Application]
  , [ prefix "+" Id
    , prefix "-" Negation
    , prefix "!" Bang
    , prefix "^" Pin
    , prefix "not" Not
    , prefix "~~~" BitwiseNot
    ]
  , [infixl' "*" Product, infixl' "/" Division]
  , [infixl' "+" Sum, infixl' "-" Subtraction]
  , [ infixr' "++" Concat
    , infixr' "--" Difference
    , infixr' ".." Range
    , infixr' "<>" StringConcat
    ]
  , [infixl' "^^^" BitwiseXor]
  , [infixl' "in" In, infixl' "not in" NotIn]
  , [ infixl' "|>" PipeRight
    , infixl' "<<<" ShiftLeft
    , infixl' ">>>" ShiftRight
    , infixl' "<<~" DoubleChevronTilde
    , infixl' "~>>" TildeDoubleChevron
    , infixl' "<~" ChevronTilde
    , infixl' "~>" TildeChevron
    , infixl' "<~>" ChevronTildeChevron
    , infixl' "<|>" ChevronPipeChevron
    ]
  , [ infixl' "<" LessThan
    , infixl' ">" GreaterThan
    , infixl' "<=" LessThanOrEqual
    , infixl' ">=" GreaterThanOrEqual
    ]
  , [ infixl' "===" StrictEqual
    , infixl' "!==" StrictNotEqual
    , infixl' "==" Equal
    , infixl' "!=" NotEqual
    , infixl' "=~" RegexEqual
    ]
  , [infixl' "&&" And, infixl' "&&&" BitwiseAnd, infixl' "and" BooleanAnd]
  , [infixl' "||" Or, infixl' "|||" BitwiseOr, infixl' "or" BooleanOr]
  , [infixr' "=" Assignment]
  , [prefix "&" Capture]
  , [infixr' "|" Pipe]
  , [infixr' "::" SpecType]
  , [infixr' "when" When]
  , [infixl' "<-" LeftArrow, infixl' "\\" DefaultArg]
  ]

prefix :: T.Text -> Operator -> E.Operator Parser EExpr
prefix name f = E.Prefix (UnaryOp f <$ symbol name)

infixl' :: T.Text -> Operator -> E.Operator Parser EExpr
infixl' name f = E.InfixL (BinaryOp f <$ symbol name)

infixr' :: T.Text -> Operator -> E.Operator Parser EExpr
infixr' name f = E.InfixR (BinaryOp f <$ symbol name)

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

chevrons :: Parser a -> Parser a
chevrons = between (symbol "<<") (symbol ">>")

squareBrackets :: Parser a -> Parser a
squareBrackets = between (symbol "[") (symbol "]")

sigilContents :: Parser String
sigilContents =
  sigilDelimiters '(' ')' <|> sigilDelimiters '{' '}' <|>
  sigilDelimiters '[' ']' <|>
  sigilDelimiters '<' '>' <|>
  sigilDelimiters '|' '|' <|>
  sigilDelimiters '/' '/' <|>
  sigilDelimiters '"' '"' <|>
  sigilDelimiters '\'' '\''

sigilDelimiters :: Char -> Char -> Parser String
sigilDelimiters a b = C.char a *> manyTill L.charLiteral (C.char b)

integer :: Parser Integer
integer = lexeme L.decimal -- TODO: should allow `_`

float :: Parser Float
float = lexeme L.float -- TODO: should allow `_`

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

unquotedAtomBody :: Parser String
unquotedAtomBody = (:) <$> unquotedAtomHead <*> unquotedAtomTail

unquotedAtom :: Parser String
unquotedAtom = lexeme $ C.char ':' *> unquotedAtomBody

quotedAtomBody :: Parser String
quotedAtomBody = charlist <|> string

quotedAtom :: Parser String
quotedAtom = lexeme $ C.char ':' *> quotedAtomBody

specialAtom :: Parser String
specialAtom =
  lexeme $ T.unpack <$> (symbol "true" <|> symbol "false" <|> symbol "nil")

alias :: Parser [String]
alias = lexeme $ (:) <$> aliasStart <*> many (try aliasChunk)

aliasStart :: Parser String
aliasStart =
  (:) <$> C.upperChar <*> many (C.letterChar <|> C.digitChar <|> C.char '_')

aliasChunk :: Parser String
aliasChunk =
  (:) <$> dotUpper <*> many (C.letterChar <|> C.digitChar <|> C.char '_')
  where
    dotUpper = do
      void $ C.char '.'
      upper <- C.upperChar
      return upper

--
-- Parser
--
-- Helpers
keyValue :: Parser EExpr -> Parser (EExpr, EExpr)
keyValue keyParser = do
  key <- keyParser
  void $ symbol "=>"
  value <- parseExpr
  return (key, value)

regularKeyValue :: Parser (EExpr, EExpr)
regularKeyValue = keyValue parseExpr

atomKeyValue :: Parser (EExpr, EExpr)
atomKeyValue = keyValue parseAtom

keywords :: (EExpr -> EExpr -> b) -> Parser b
keywords wrapper = do
  key <- Atom . T.pack <$!> (quotedAtomBody <|> unquotedAtomBody)
  void $ symbol ":"
  value <- parseExpr
  return $ wrapper key value

mapKeywords :: Parser (EExpr, EExpr)
mapKeywords = keywords (,)

listKeywords :: Parser EExpr
listKeywords = keywords (\k v -> Tuple [k, v])

parensArgs :: Parser [EExpr]
parensArgs = between (symbol "(") (symbol ")") (commaSeparated parseExpr)

spacesArgs :: Parser [EExpr]
spacesArgs = C.char ' ' >> commaSeparated1 parseExpr

commaSeparated :: Parser a -> Parser [a]
commaSeparated parser = parser `sepBy` (lexeme $ C.char ',')

commaSeparated1 :: Parser a -> Parser [a]
commaSeparated1 parser = parser `sepBy1` (lexeme $ C.char ',')

-- Parsers
listParser :: Parser EExpr
listParser = between spaceConsumer eof parseList

exprParser :: Parser EExpr
exprParser = between spaceConsumer eof parseExpr

parseAlias :: Parser EExpr
parseAlias = Alias <$> fmap T.pack <$> alias

parseList :: Parser EExpr
parseList = List <$> (try regularList <|> keywordList)
  where
    regularList = squareBrackets $ commaSeparated parseExpr
    keywordList = squareBrackets $ commaSeparated listKeywords

parseTuple :: Parser EExpr
parseTuple = Tuple <$> braces (commaSeparated parseExpr)

parseBinary :: Parser EExpr
parseBinary = Binary <$> chevrons (commaSeparated parseExpr)

parseSigil :: Parser EExpr
parseSigil = do
  void $ symbol "~"
  ident <- C.upperChar <|> C.lowerChar
  contents <- T.pack <$> sigilContents
  modifiers <- many C.letterChar
  return $ Sigil ident contents modifiers

parseMap :: Parser EExpr
parseMap = do
  void $ symbol "%"
  Map <$>
    braces (try (commaSeparated regularKeyValue) <|> commaSeparated mapKeywords)

parseStruct :: Parser EExpr
parseStruct = do
  void $ symbol "%"
  alias' <- parseAlias
  let arrow = commaSeparated1 atomKeyValue
      keywords = commaSeparated mapKeywords
  Struct alias' <$> braces (try arrow <|> keywords)

parseAtom :: Parser EExpr
parseAtom = Atom . T.pack <$!> (try unquotedAtom <|> quotedAtom <|> specialAtom)

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

parseExpr :: Parser EExpr
parseExpr =
  try parseStruct <|> try parseMap <|> parseSigil <|> parseTuple <|> parseList <|>
  parseBinary <|>
  try parseFloat <|>
  parseInteger <|>
  parseAtom <|>
  parseString <|>
  parseCharlist <|>
  try parseNonQualifiedCall <|>
  try parseQualifiedCall <|>
  parseVariable <|>
  parseAlias

-- TODO: Clean up ltr / rtl / unary ops
parseExpr' :: Parser EExpr
parseExpr' = makeExprParser parseExpr opsTable

--
-- REPL
--
readExpr :: T.Text -> Either ParseError EExpr
readExpr = parse exprParser ""

readList :: T.Text -> Either ParseError EExpr
readList = parse listParser ""
