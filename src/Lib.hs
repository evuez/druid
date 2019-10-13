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
opsTable :: [[Operator Parser E.EExpr]]
opsTable =
  [ [prefix "@" E.Attribute]
  , [infixlNotFollowedBy "." E.Application "."]
  , [ prefix "+" E.Id
    , prefix "-" E.Negation
    , prefix "!" E.Bang
    , prefix "^" E.Pin
    , prefix "not" E.Not
    , prefix "~~~" E.BitwiseNot
    ]
  , [infixl' "*" E.Product, infixl' "/" E.Division]
  , [ infixlNotFollowedBy "+" E.Sum "+"
    , infixlNotFollowedBy "-" E.Subtraction ">-"
    ]
  , [ infixr' "++" E.Concat
    , infixr' "--" E.Difference
    , infixr' ".." E.Range
    , infixr' "<>" E.StringConcat
    ]
  , [infixl' "^^^" E.BitwiseXor]
  , [infixl' "in" E.In, infixl' "not in" E.NotIn]
  , [ infixlPrecededByEol "|>" E.PipeRight
    , infixl' "<<<" E.ShiftLeft
    , infixl' ">>>" E.ShiftRight
    , infixl' "<<~" E.DoubleChevronTilde
    , infixl' "~>>" E.TildeDoubleChevron
    , infixlNotFollowedBy "<~" E.ChevronTilde ">"
    , infixl' "~>" E.TildeChevron
    , infixl' "<~>" E.ChevronTildeChevron
    , infixl' "<|>" E.ChevronPipeChevron
    ]
  , [ infixlNotFollowedBy "<" E.LessThan "-="
    , infixlNotFollowedBy ">" E.GreaterThan ">="
    , infixl' "<=" E.LessThanOrEqual
    , infixl' ">=" E.GreaterThanOrEqual
    ]
  , [ infixl' "===" E.StrictEqual
    , infixl' "!==" E.StrictNotEqual
    , infixl' "==" E.Equal
    , infixl' "!=" E.NotEqual
    , infixl' "=~" E.RegexEqual
    ]
  , [ infixlNotFollowedBy "&&" E.And "&"
    , infixl' "&&&" E.BitwiseAnd
    , infixl' "and" E.BooleanAnd
    ]
  , [ infixlNotFollowedBy "||" E.Or "|"
    , infixl' "|||" E.BitwiseOr
    , infixl' "or" E.BooleanOr
    ]
  , [infixrNotFollowedBy "=" E.Assignment ">"]
  , [prefix "&" E.Capture]
  , [infixrPrecededByEol "|" E.Pipe]
  , [infixr' "::" E.SpecType]
  , [infixrPrecededByEol "when" E.When]
  , [infixl' "<-" E.LeftArrow, infixl' "\\\\" E.DefaultArg]
  ]

prefix :: String -> E.Operator -> Operator Parser E.EExpr
prefix name f = Prefix (E.UnaryOp f <$ symbol' name)

infixl' :: String -> E.Operator -> Operator Parser E.EExpr
infixl' name f = InfixL (E.BinaryOp f <$ symbol' name)

infixr' :: String -> E.Operator -> Operator Parser E.EExpr
infixr' name f = InfixR (E.BinaryOp f <$ symbol' name)

infixlNotFollowedBy :: String -> E.Operator -> String -> Operator Parser E.EExpr
infixlNotFollowedBy name f chars =
  InfixL (E.BinaryOp f <$ try (symbol' name <* notFollowedBy (oneOf chars)))

infixrNotFollowedBy :: String -> E.Operator -> String -> Operator Parser E.EExpr
infixrNotFollowedBy name f chars =
  InfixR (E.BinaryOp f <$ try (symbol' name <* notFollowedBy (oneOf chars)))

-- Differentiating multi-line expressions and blocks is a pain.
infixlPrecededByEol :: String -> E.Operator -> Operator Parser E.EExpr
infixlPrecededByEol name f =
  InfixL (E.BinaryOp f <$ try (lexeme (optional C.eol) *> symbol' name))

infixrPrecededByEol :: String -> E.Operator -> Operator Parser E.EExpr
infixrPrecededByEol name f =
  InfixR (E.BinaryOp f <$ try (lexeme (optional C.eol) *> symbol' name))

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

variable :: Parser String
variable = lexeme $ identifier <* notFollowedBy (C.char ':')

semicolon :: Parser String
semicolon = symbol ";"

blockSep :: Parser String
blockSep = semicolon <|> lexeme C.eol

identifier :: Parser String
identifier = p >>= check
  where
    p =
      (++) <$>
      ((:) <$> (C.lowerChar <|> C.char '_') <*>
       many (C.letterChar <|> C.digitChar <|> C.char '_')) <*>
      count' 0 1 (oneOf "!?")
    check x =
      if x `elem` reservedWords
        then fail $ show x ++ " is a reserved keyword"
        else pure x

string :: Parser String
string = lexeme strictString

strictString :: Parser String
strictString = C.char '"' *> manyTill L.charLiteral (C.char '"')

multiString :: Parser String
multiString = C.string "\"\"\"" *> manyTill L.charLiteral (C.string "\"\"\"")

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
unquotedAtom = lexeme $ C.char ':' *> unquotedAtomBody

quotedAtomBody :: Parser String
quotedAtomBody = charlist <|> string

quotedAtom :: Parser String
quotedAtom = lexeme $ C.char ':' *> quotedAtomBody

specialAtom :: Parser String
specialAtom = lexeme $ symbol "true" <|> symbol "false" <|> symbol "nil"

alias :: Parser [String]
alias = lexeme $ (:) <$> aliasStart <*> many (try aliasChunk)

aliasStart :: Parser String
aliasStart =
  (:) <$> C.upperChar <*> many (C.letterChar <|> C.digitChar <|> C.char '_')

aliasChunk :: Parser String
aliasChunk =
  (:) <$> (C.char '.' *> C.upperChar) <*>
  many (C.letterChar <|> C.digitChar <|> C.char '_')

rightArrow :: Parser String
rightArrow = symbol' "->"

--
-- Parser
--
-- Helpers
keyValue :: Parser E.EExpr -> Parser (E.EExpr, E.EExpr)
keyValue keyParser = do
  key <- keyParser
  void $ symbol' "=>"
  value <- parseExpr
  void $ optional (lexeme C.eol)
  pure (key, value)

regularKeyValue :: Parser (E.EExpr, E.EExpr)
regularKeyValue = keyValue parseExpr

atomKeyValue :: Parser (E.EExpr, E.EExpr)
atomKeyValue = keyValue parseAtom

keywords :: (E.EExpr -> E.EExpr -> b) -> Parser b
keywords wrapper = do
  key <- E.Atom <$!> (quotedAtomBody <|> unquotedAtomBody)
  void $ symbol' ":"
  value <- parseExpr
  void $ optional (lexeme C.eol)
  pure $ wrapper key value

mapKeywords :: Parser (E.EExpr, E.EExpr)
mapKeywords = keywords (,)

listKeywords :: Parser E.EExpr
listKeywords = keywords (\k v -> E.Tuple [k, v])

parensArgs :: Parser [E.EExpr]
parensArgs = parens $ lexeme' (commaSeparated $ try parseExpr <|> keywordArgs)
  where
    keywordArgs = E.List <$> commaSeparated1 listKeywords

spacesArgs :: Parser [E.EExpr]
spacesArgs = do
  void $ C.char ' '
  args <- commaSeparated $ try parseExpr <|> try keywordArgs
  doBlock <- count' 0 1 parseDoBlock
  case args ++ doBlock of
    [] -> fail "missing params in function call"
    args' -> pure args'
  where
    keywordArgs = E.List <$> commaSeparated1 listKeywords

spacesArgs' :: Parser [E.EExpr]
spacesArgs' = commaSeparated1 parseExpr

commaSeparated :: Parser a -> Parser [a]
commaSeparated parser = parser `sepBy` (symbol' ",")

commaSeparated1 :: Parser a -> Parser [a]
commaSeparated1 parser = parser `sepBy1` (symbol' ",")

-- Parsers
exprParser :: Parser E.EExpr
exprParser = between spaceConsumer eof parseExpr

parseBlock :: Parser E.EExpr
parseBlock = E.Block <$> (clauses <|> exprs)
  where
    exprs = try parseExpr `sepEndBy` (many blockSep)
    clauses = try parseRightArrow `sepEndBy1` (many blockSep)

parseBlock2 :: Parser E.EExpr
parseBlock2 =
  E.Block <$>
  try (parseExpr <* notFollowedBy rightArrow) `sepEndBy2` (many blockSep)

parseDoBlock :: Parser E.EExpr
parseDoBlock = do
  doBlock <-
    doEnd $ do
      block <- parseBlock
      alts <- optional $ many (wrapper <$> alternatives <*> parseBlock)
      pure $
        case alts of
          Just alts' -> (wrapper "do" block) : alts'
          Nothing -> [wrapper "do" block]
  pure $ E.List doBlock
  where
    alternatives =
      symbol' "catch" <|> symbol' "rescue" <|> symbol' "after" <|>
      symbol' "else"
    wrapper f x = E.Tuple [E.Atom f, x]

parseFn :: Parser E.EExpr
parseFn = E.Fn <$> fnEnd (try parseRightArrow `sepEndBy` (many blockSep))

parseAlias :: Parser E.EExpr
parseAlias = E.Alias <$> alias

parseList :: Parser E.EExpr
parseList = E.List <$> (try regularList <|> keywordList)
  where
    regularList = squareBrackets $ commaSeparated parseExpr
    keywordList = squareBrackets $ commaSeparated listKeywords

parseTuple :: Parser E.EExpr
parseTuple = E.Tuple <$> braces (commaSeparated parseExpr)

parseBinary :: Parser E.EExpr
parseBinary = E.Binary <$> chevrons (commaSeparated parseExpr)

parseSigil :: Parser E.EExpr
parseSigil =
  symbol "~" *>
  liftA3
    E.Sigil
    (C.upperChar <|> C.lowerChar)
    sigilContents
    (lexeme $ many C.letterChar)

parseMap :: Parser E.EExpr
parseMap = do
  void $ symbol "%"
  E.Map <$>
    braces (try (commaSeparated regularKeyValue) <|> commaSeparated mapKeywords)

parseMapUpdate :: Parser E.EExpr
parseMapUpdate = do
  void $ symbol "%"
  mapUpdate <-
    braces $ do
      lhs <- lexeme' parseMapExpr
      void $ symbol' "|"
      rhs <- try (commaSeparated regularKeyValue) <|> commaSeparated mapKeywords
      pure $ E.MapUpdate lhs rhs
  pure mapUpdate

parseStruct :: Parser E.EExpr
parseStruct = do
  void $ symbol "%"
  alias' <- parseAlias
  E.Struct alias' <$> braces (try arrow <|> keywords')
  where
    arrow = commaSeparated1 atomKeyValue
    keywords' = commaSeparated mapKeywords

parseStructUpdate :: Parser E.EExpr
parseStructUpdate = do
  void $ symbol "%"
  alias' <- parseAlias
  mapUpdate <-
    braces $ do
      lhs <- lexeme' parseMapExpr
      void $ symbol' "|"
      rhs <- try arrow <|> keywords'
      pure $ E.StructUpdate alias' lhs rhs
  pure mapUpdate
  where
    arrow = commaSeparated1 atomKeyValue
    keywords' = commaSeparated mapKeywords

parseAtom :: Parser E.EExpr
parseAtom = E.Atom <$!> (try unquotedAtom <|> quotedAtom <|> specialAtom)

parseString :: Parser E.EExpr
parseString = E.String <$!> (try multiString <|> string)

parseCharlist :: Parser E.EExpr
parseCharlist = E.Charlist <$!> charlist

parseVariable :: Parser E.EExpr
parseVariable = E.Variable <$!> variable

parseInteger :: Parser E.EExpr -- notFollowedByIdentifierStart?
parseInteger = E.Integer <$> integer

parseFloat :: Parser E.EExpr -- notFollowedByIdentifierStart?
parseFloat = E.Float <$> float

parseNonQualifiedCall :: Parser E.EExpr
parseNonQualifiedCall =
  liftA2 E.NonQualifiedCall identifier (parensArgs <|> spacesArgs)

parseQualifiedCall :: Parser E.EExpr
parseQualifiedCall = do
  alias' <- parseAlias
  void $ C.char '.'
  name <- identifier <|> strictString <|> strictCharlist
  args <- parensArgs <|> spacesArgs
  pure $ E.QualifiedCall alias' name args

parseRightArrow :: Parser E.EExpr
parseRightArrow = do
  void $ optional (C.char ' ')
  lhs <- optional (try $ parensArgs <|> spacesArgs')
  void $ rightArrow
  rhs <- try parseBlock2 <|> parseExpr
  case lhs of
    Just lhs' -> pure $ E.BinaryOp E.RightArrow (E.List lhs') rhs
    Nothing -> pure $ E.BinaryOp E.RightArrow (E.List []) rhs

-- This is wrong in so many ways...
parseAccess :: Parser E.EExpr
parseAccess = do
  void $
    lookAhead
      (takeWhile1P Nothing (not . (\c -> isSpace c || c == '[')) <* C.char '[')
  expr <- parseAccessExpr
  void $ symbol "["
  index <- parseExpr
  void $ symbol "]"
  pure $ E.QualifiedCall (E.Alias ["Access"]) "get" [expr, index]

parseAny :: Parser E.EExpr
parseAny =
  (try parseStruct <?> "struct") <|> (try parseMap <?> "map") <|>
  (try parseStructUpdate <?> "map update") <|>
  (parseMapUpdate <?> "map update") <|>
  (parseSigil <?> "sigil") <|>
  (parseTuple <?> "tuple") <|>
  (parseList <?> "list") <|>
  (parseBinary <?> "binary") <|>
  (try parseFloat <?> "float") <|>
  (parseInteger <?> "integer") <|>
  (parseAtom <?> "atom") <|>
  (parseString <?> "string") <|>
  (parseCharlist <?> "charlist") <|>
  (try parseNonQualifiedCall <?> "non-qualified call") <|>
  (try parseQualifiedCall <?> "qualified call") <|>
  (parseFn <?> "fn") <|>
  (parseVariable <?> "variable") <|>
  (parseAlias <?> "alias")

parseExpr :: Parser E.EExpr
parseExpr =
  makeExprParser
    ((try parseAccess <?> "access") <|> (try $ parens parseRightArrow) <|>
     parens parseExpr <|>
     parseAny)
    opsTable

parseAccessExpr :: Parser E.EExpr
parseAccessExpr =
  makeExprParser
    ((try $ parens parseRightArrow) <|> parens parseAccessExpr <|> parseAny)
    opsTable

-- Ugly hack to avoid parsing | as a binary op in structs and maps
parseMapExpr :: Parser E.EExpr
parseMapExpr =
  makeExprParser
    ((try $ parens parseRightArrow) <|> parens parseMapExpr <|> parseAny)
    opsTableWithNoPipe
  where
    opsTableWithNoPipe = xs ++ ys
    (xs, (_:ys)) = splitAt 15 opsTable

--
-- REPL
--
readExpr :: String -> IO ()
readExpr s =
  case parse exprParser "" s of
    Left e -> putStr $ errorBundlePretty e
    Right ast -> putStr $ show ast ++ "\n"

readSource :: String -> IO ()
readSource p = readFile p >>= readExpr
