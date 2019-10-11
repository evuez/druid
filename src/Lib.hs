{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import Control.Applicative ((<|>), empty, liftA2, optional)
import Control.Exception (Exception, SomeException, fromException, throw)
import Control.Monad (MonadPlus, (<$!>), void)
import Control.Monad.Combinators.Expr (makeExprParser)
import qualified Control.Monad.Combinators.Expr as E
  ( Operator(InfixL, InfixR, Prefix)
  )
import Control.Monad.Reader (MonadReader, ReaderT(..), ask, local)
import Data.Char (isSpace)
import Data.List (intercalate)
import qualified Data.Map as M (Map, foldrWithKey, fromList, insert, lookup)
import Data.Typeable (Typeable)
import Data.Void
import Text.Megaparsec
  ( ParseErrorBundle
  , Parsec
  , (<?>)
  , between
  , choice
  , count
  , count'
  , endBy
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
  , skipCount
  , some
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
  , skipBlockComment
  , skipLineComment
  , space
  , symbol
  )
import Text.Megaparsec.Debug (dbg)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Megaparsec.Stream (Token)

data EExpr
  = Atom String
  | Alias [String]
  | Binary [EExpr]
  | BinaryOp Operator
             EExpr
             EExpr
  | Block [EExpr]
  | Charlist String
  | Float Float
  | Fn [EExpr]
  | Integer Integer
  | List [EExpr]
  | Map [(EExpr, EExpr)]
  | MapUpdate { expr :: EExpr
              , updates :: [(EExpr, EExpr)] }
  | NonQualifiedCall { name :: String
                     , args :: [EExpr] }
  | QualifiedCall { alias' :: EExpr
                  , name :: String
                  , args :: [EExpr] }
  | Sigil { ident :: Char
          , contents :: String
          , modifiers :: [Char] }
  | String String
  | Struct { alias' :: EExpr
           , map :: [(EExpr, EExpr)] }
  | StructUpdate { alias' :: EExpr
                 , expr :: EExpr
                 , updates :: [(EExpr, EExpr)] }
  | Tuple [EExpr]
  | UnaryOp Operator
            EExpr
  | Variable String
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
  | RightArrow
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

type Env = M.Map String EExpr

type Parser = Parsec Void String

type ParseError = ParseErrorBundle String Void

newtype Eval a =
  Eval (ReaderT Env IO a)
  deriving (Monad, Applicative, Functor, MonadReader Env)

instance Show EExpr where
  show = showExpr

-- TODO: Display ast form instead
showExpr :: EExpr -> String
showExpr (Atom atom) = concat [":", atom]
showExpr (Alias alias') =
  concat ["{:__aliases__, [], [:", intercalate ", :" alias', "]}"]
showExpr (Block exprs) =
  concat ["{:__block__, [], [", intercalate ", " $ showExpr <$> exprs, "]}"]
showExpr (Integer integer) = show integer
showExpr (Float float) = show float
showExpr (String text) = concat ["\"", text, "\""]
showExpr (Charlist text) = concat ["'", text, "'"]
showExpr (Variable name) = concat ["{:", name, ", [], Elixir}"]
showExpr (Tuple [expr1, expr2]) =
  concat ["{", showExpr expr1, ", ", showExpr expr2, "}"]
showExpr (Tuple exprs) =
  concat ["{:{}, [], [", intercalate ", " $ showExpr <$> exprs, "]}"]
showExpr (Binary exprs) =
  concat ["{:<<>>, [], [", intercalate ", " $ showExpr <$> exprs, "]}"]
showExpr (Sigil ident contents modifiers) =
  concat
    [ "{:sigil_"
    , [ident]
    , ", [], [{:<<>>, [], [\""
    , contents
    , "\"]}, '"
    , modifiers
    , "']}"
    ]
showExpr (List exprs) = concat ["[", intercalate ", " $ showExpr <$> exprs, "]"]
showExpr (Map keyValues) =
  concat
    [ "%{"
    , intercalate ", " $
      (\(k, v) -> concat [showExpr k, " => ", showExpr v]) <$> keyValues
    , "}"
    ]
showExpr (MapUpdate expr updates) =
  concat
    [ "{:%{}, [], [{:|, [], ["
    , showExpr expr
    , ", ["
    , intercalate ", " $
      (\(k, v) -> concat ["{", showExpr k, ", ", showExpr v, "}"]) <$> updates
    , "]]}]}"
    ]
showExpr (Struct alias' keyValues) =
  concat
    [ "%"
    , showExpr alias'
    , "{"
    , intercalate ", " $
      (\(k, v) -> concat [showExpr k, " => ", showExpr v]) <$> keyValues
    , "}"
    ]
showExpr (StructUpdate alias' expr updates) =
  concat
    [ "{:%, [], [{:__aliases__, [], [:"
    , showExpr alias'
    , "]}, {:%{}, [], [{:|, [], ["
    , showExpr expr
    , ", ["
    , intercalate ", " $
      (\(k, v) -> concat ["{", showExpr k, ", ", showExpr v, "}"]) <$> updates
    , "]]}]}]}"
    ]
showExpr (QualifiedCall alias' name args) =
  concat
    [ "{:., [], ["
    , showExpr alias'
    , ", :"
    , name
    , "]}, [], ["
    , intercalate ", " $ showExpr <$> args
    , "]}"
    ]
showExpr (NonQualifiedCall name args) =
  concat ["{:", name, ", [], [", intercalate ", " $ showExpr <$> args, "]}"]
showExpr (BinaryOp op a b) =
  concat ["{:", showOp op, ", [], [", showExpr a, ", ", showExpr b, "]}"]
showExpr (UnaryOp op a) = concat ["{:", showOp op, ", [], [", showExpr a, "]}"]
showExpr (Fn exprs) =
  concat ["{:fn, [], [", intercalate ", " $ showExpr <$> exprs, "]}"]

showOp :: Operator -> String
showOp And = "&&"
showOp Application = "."
showOp Assignment = "="
showOp Attribute = "@"
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
showOp RightArrow = "->"
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

showEnv :: Env -> String
showEnv env =
  concat $
  M.foldrWithKey (\k v a -> "(" : k : " " : showExpr v : "), " : a) [] env

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
opsTable :: [[E.Operator Parser EExpr]]
opsTable =
  [ [prefix "@" Attribute]
  , [infixlNotFollowedBy "." Application "."]
  , [ prefix "+" Id
    , prefix "-" Negation
    , prefix "!" Bang
    , prefix "^" Pin
    , prefix "not" Not
    , prefix "~~~" BitwiseNot
    ]
  , [infixl' "*" Product, infixl' "/" Division]
  , [infixlNotFollowedBy "+" Sum "+", infixlNotFollowedBy "-" Subtraction ">-"]
  , [ infixr' "++" Concat
    , infixr' "--" Difference
    , infixr' ".." Range
    , infixr' "<>" StringConcat
    ]
  , [infixl' "^^^" BitwiseXor]
  , [infixl' "in" In, infixl' "not in" NotIn]
  , [ infixlPrecededByEol "|>" PipeRight
    , infixl' "<<<" ShiftLeft
    , infixl' ">>>" ShiftRight
    , infixl' "<<~" DoubleChevronTilde
    , infixl' "~>>" TildeDoubleChevron
    , infixlNotFollowedBy "<~" ChevronTilde ">"
    , infixl' "~>" TildeChevron
    , infixl' "<~>" ChevronTildeChevron
    , infixl' "<|>" ChevronPipeChevron
    ]
  , [ infixlNotFollowedBy "<" LessThan "-="
    , infixlNotFollowedBy ">" GreaterThan ">="
    , infixl' "<=" LessThanOrEqual
    , infixl' ">=" GreaterThanOrEqual
    ]
  , [ infixl' "===" StrictEqual
    , infixl' "!==" StrictNotEqual
    , infixl' "==" Equal
    , infixl' "!=" NotEqual
    , infixl' "=~" RegexEqual
    ]
  , [ infixlNotFollowedBy "&&" And "&"
    , infixl' "&&&" BitwiseAnd
    , infixl' "and" BooleanAnd
    ]
  , [ infixlNotFollowedBy "||" Or "|"
    , infixl' "|||" BitwiseOr
    , infixl' "or" BooleanOr
    ]
  , [infixrNotFollowedBy "=" Assignment ">"]
  , [prefix "&" Capture]
  , [infixrPrecededByEol "|" Pipe]
  , [infixr' "::" SpecType]
  , [infixrPrecededByEol "when" When]
  , [infixl' "<-" LeftArrow, infixl' "\\\\" DefaultArg]
  ]

prefix :: String -> Operator -> E.Operator Parser EExpr
prefix name f = E.Prefix (UnaryOp f <$ symbol' name)

infixl' :: String -> Operator -> E.Operator Parser EExpr
infixl' name f = E.InfixL (BinaryOp f <$ symbol' name)

infixr' :: String -> Operator -> E.Operator Parser EExpr
infixr' name f = E.InfixR (BinaryOp f <$ symbol' name)

infixlNotFollowedBy :: String -> Operator -> String -> E.Operator Parser EExpr
infixlNotFollowedBy name f chars =
  E.InfixL (BinaryOp f <$ try (symbol' name <* notFollowedBy (oneOf chars)))

infixrNotFollowedBy :: String -> Operator -> String -> E.Operator Parser EExpr
infixrNotFollowedBy name f chars =
  E.InfixR (BinaryOp f <$ try (symbol' name <* notFollowedBy (oneOf chars)))

-- Differentiating multi-line expressions and blocks is a pain.
infixlPrecededByEol :: String -> Operator -> E.Operator Parser EExpr
infixlPrecededByEol name f =
  E.InfixL (BinaryOp f <$ try (lexeme (optional C.eol) >> symbol' name))

infixrPrecededByEol :: String -> Operator -> E.Operator Parser EExpr
infixrPrecededByEol name f =
  E.InfixR (BinaryOp f <$ try (lexeme (optional C.eol) >> symbol' name))

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
squareBrackets = between (symbol' "[") (lexeme (optional C.eol) >> symbol' "]")

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

rword :: String -> Parser ()
rword w =
  lexeme . try $ C.string w *> notFollowedBy (C.lowerChar <|> C.char '_')

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
        else return x

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
  (:) <$> dotUpper <*> many (C.letterChar <|> C.digitChar <|> C.char '_')
  where
    dotUpper = do
      void $ C.char '.'
      upper <- C.upperChar
      return upper

rightArrow :: Parser String
rightArrow = symbol' "->"

--
-- Parser
--
-- Helpers
keyValue :: Parser EExpr -> Parser (EExpr, EExpr)
keyValue keyParser = do
  key <- keyParser
  void $ symbol' "=>"
  value <- parseExpr
  void $ optional (lexeme C.eol)
  return (key, value)

regularKeyValue :: Parser (EExpr, EExpr)
regularKeyValue = keyValue parseExpr

atomKeyValue :: Parser (EExpr, EExpr)
atomKeyValue = keyValue parseAtom

keywords :: (EExpr -> EExpr -> b) -> Parser b
keywords wrapper = do
  key <- Atom <$!> (quotedAtomBody <|> unquotedAtomBody)
  void $ symbol' ":"
  value <- parseExpr
  void $ optional (lexeme C.eol)
  return $ wrapper key value

mapKeywords :: Parser (EExpr, EExpr)
mapKeywords = keywords (,)

listKeywords :: Parser EExpr
listKeywords = keywords (\k v -> Tuple [k, v])

parensArgs :: Parser [EExpr]
parensArgs = parens $ lexeme' (commaSeparated $ try parseExpr <|> keywordArgs)
  where
    keywordArgs = List <$> commaSeparated1 listKeywords

spacesArgs :: Parser [EExpr]
spacesArgs = do
  void $ C.char ' '
  args <- commaSeparated $ try parseExpr <|> try keywordArgs
  doBlock <- count' 0 1 parseDoBlock
  case args ++ doBlock of
    [] -> fail "missing params in function call"
    args' -> return args'
  where
    keywordArgs = List <$> commaSeparated1 listKeywords

spacesArgs' :: Parser [EExpr]
spacesArgs' = commaSeparated1 parseExpr

commaSeparated :: Parser a -> Parser [a]
commaSeparated parser = parser `sepBy` (symbol' ",")

commaSeparated1 :: Parser a -> Parser [a]
commaSeparated1 parser = parser `sepBy1` (symbol' ",")

-- Parsers
exprParser :: Parser EExpr
exprParser = between spaceConsumer eof parseExpr

parseBlock :: Parser EExpr
parseBlock = Block <$> (clauses <|> exprs)
  where
    exprs = try parseExpr `sepEndBy` (many blockSep)
    clauses = try parseRightArrow `sepEndBy1` (many blockSep)

parseBlock2 :: Parser EExpr
parseBlock2 =
  Block <$>
  try (parseExpr <* notFollowedBy rightArrow) `sepEndBy2` (many blockSep)

parseDoBlock :: Parser EExpr
parseDoBlock = do
  doBlock <-
    doEnd $ do
      block <- parseBlock
      alts <- optional $ many (wrapper <$> alts <*> parseBlock)
      return $
        case alts of
          Just alts' -> (wrapper "do" block) : alts'
          Nothing -> [wrapper "do" block]
  return $ List doBlock
  where
    alts =
      symbol' "catch" <|> symbol' "rescue" <|> symbol' "after" <|>
      symbol' "else"
    wrapper f x = Tuple [Atom f, x]

parseFn :: Parser EExpr
parseFn = Fn <$> fnEnd (try parseRightArrow `sepEndBy` (many blockSep))

parseAlias :: Parser EExpr
parseAlias = Alias <$> alias

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
  contents <- sigilContents
  modifiers <- lexeme $ many C.letterChar
  return $ Sigil ident contents modifiers

parseMap :: Parser EExpr
parseMap = do
  void $ symbol "%"
  Map <$>
    braces (try (commaSeparated regularKeyValue) <|> commaSeparated mapKeywords)

parseMapUpdate = do
  void $ symbol "%"
  mapUpdate <-
    braces $ do
      lhs <- lexeme' parseMapExpr
      void $ symbol' "|"
      rhs <- try (commaSeparated regularKeyValue) <|> commaSeparated mapKeywords
      return $ MapUpdate lhs rhs
  return mapUpdate

parseStruct :: Parser EExpr
parseStruct = do
  void $ symbol "%"
  alias' <- parseAlias
  Struct alias' <$> braces (try arrow <|> keywords)
  where
    arrow = commaSeparated1 atomKeyValue
    keywords = commaSeparated mapKeywords

parseStructUpdate = do
  void $ symbol "%"
  alias' <- parseAlias
  mapUpdate <-
    braces $ do
      lhs <- lexeme' parseMapExpr
      void $ symbol' "|"
      rhs <- try arrow <|> keywords
      return $ StructUpdate alias' lhs rhs
  return mapUpdate
  where
    arrow = commaSeparated1 atomKeyValue
    keywords = commaSeparated mapKeywords

parseAtom :: Parser EExpr
parseAtom = Atom <$!> (try unquotedAtom <|> quotedAtom <|> specialAtom)

parseString :: Parser EExpr
parseString = String <$!> (try multiString <|> string)

parseCharlist :: Parser EExpr
parseCharlist = Charlist <$!> charlist

parseVariable :: Parser EExpr
parseVariable = Variable <$!> variable

parseInteger :: Parser EExpr -- notFollowedByIdentifierStart?
parseInteger = Integer <$> integer

parseFloat :: Parser EExpr -- notFollowedByIdentifierStart?
parseFloat = Float <$> float

parseNonQualifiedCall :: Parser EExpr
parseNonQualifiedCall = do
  name <- identifier
  args <- parensArgs <|> spacesArgs
  return $ NonQualifiedCall name args

parseQualifiedCall :: Parser EExpr
parseQualifiedCall = do
  alias' <- parseAlias
  void $ C.char '.'
  name <- identifier <|> strictString <|> strictCharlist
  args <- parensArgs <|> spacesArgs
  return $ QualifiedCall alias' name args

parseRightArrow :: Parser EExpr
parseRightArrow = do
  void $ optional (C.char ' ')
  lhs <- optional (try $ parensArgs <|> spacesArgs')
  void $ rightArrow
  rhs <- try parseBlock2 <|> parseExpr
  case lhs of
    Just lhs' -> return $ BinaryOp RightArrow (List lhs') rhs
    Nothing -> return $ BinaryOp RightArrow (List []) rhs

-- This is wrong in so many ways...
parseAccess :: Parser EExpr
parseAccess = do
  void $
    lookAhead
      (takeWhile1P Nothing (not . (\c -> isSpace c || c == '[')) <* C.char '[')
  expr <- parseAccessExpr
  void $ symbol "["
  index <- parseExpr
  void $ symbol "]"
  return $ QualifiedCall (Alias ["Access"]) "get" [expr, index]

parseAny :: Parser EExpr
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

parseExpr :: Parser EExpr
parseExpr =
  makeExprParser
    ((try parseAccess <?> "access") <|> (try $ parens parseRightArrow) <|>
     parens parseExpr <|>
     parseAny)
    opsTable

parseAccessExpr :: Parser EExpr
parseAccessExpr =
  makeExprParser
    ((try $ parens parseRightArrow) <|> parens parseAccessExpr <|> parseAny)
    opsTable

-- Ugly hack to avoid parsing | as a binary op in structs and maps
parseMapExpr :: Parser EExpr
parseMapExpr =
  makeExprParser
    ((try $ parens parseRightArrow) <|> parens parseMapExpr <|> parseAny)
    opsTableWithNoPipe
  where
    opsTableWithNoPipe = xs ++ ys
    (xs, (y:ys)) = splitAt 15 opsTable

--
-- REPL
--
readExpr :: String -> IO ()
readExpr e =
  case parse exprParser "" e of
    Left e -> putStr $ errorBundlePretty e
    Right ast -> putStr $ show ast ++ "\n"

readSource :: String -> IO ()
readSource p = do
  source <- readFile p
  readExpr source
