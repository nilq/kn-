module Lexer where

import Text.Parsec.String   (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops =
      [ "+"
      , "*"
      , "-"
      , ";"
      , ","
      , "<"
      ]

    names =
      [ "func"
      , "summon"
      , "if"
      , "then"
      , "else"
      ]

    style = emptyDef
      { Tok.commentLine = "#"
      , Tok.reservedOpNames = ops
      , Tok.reservedNames = names
      }

integer =
  Tok.integer lexer

float =
  Tok.float lexer

parens =
  Tok.parens lexer

commaSep =
  Tok.commaSep lexer

semiSep =
  Tok.semiSep lexer

identifier =
  Tok.identifier lexer

reserved =
  Tok.reserved lexer

reservedOp =
  Tok.reservedOp lexer

whitespace =
  Tok.whiteSpace lexer
