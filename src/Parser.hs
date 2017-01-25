module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr  as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

int :: Parser Expr
int = do
  n <- integer
  return $ Float (fromInteger n)

floating :: Parser Expr
float =
  Float <$> float

binary s f assoc =
  Ex.Infix (reservedOp s >> return (BinOp f)) assoc

binops =
  [
    [ binary "*" Ex.AssocLeft
    , binary "/" Ex.AssocLeft
    ]
    ,
    [ binary "+" Ex.AssocLeft
    , binary "-" Ex.AssocLeft
    ]
  ]


floating :: Parser Expr
floating = do
  Float <$> float

expr :: Parser Expr
expr =
  Ex.buildExpressionParser table factor

variable :: Parser Expr
variable = do
  var <- identifier
  return $ Var var

function :: Parser Expr
function = do
  reserved "func"
  name <- identifier
  args <- parens $ many variable
  body <- expr
  return $ Extern name args

extern :: Parser Expr
extern = do
  reserved "import"
  name <- identifier
  args <- parens $ many variable
  return $ Extern name args

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

factor :: Parser Expr
factor =
      try floating
  <|> try int
  <|> try extern
  <|> try function
  <|> try call
  <|> variable

defn :: Parser Expr
defn = try extern
  <|> try function
  <|> expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many $ do
  def <- defn
  reservedOp ";"
  return def

parseExpr :: String -> Either ParseError Expr
parseExpr s =
  parse (contents expr) "<stdin>" s

parseTopLevel :: String -> Either ParseError [Expr]
parseTopLevel s =
  parse (contents toplevel) "<stdnin>" s
