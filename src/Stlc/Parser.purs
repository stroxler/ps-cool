module Stlc.Parser
  where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Control.Lazy (fix)
import Data.Array.NonEmpty as NonEmpty
import Data.CodePoint.Unicode (GeneralCategory(..), isLower, isUpper)
import Data.CodePoint.Unicode as Char
import Data.Identity (Identity)
import Data.Maybe as Maybe
import Data.String (codePointAt, length)
import Data.String.Unsafe (charAt)
import Data.String.Unsafe as String
import Parsing (ParserT)
import Parsing.Language as Lang
import Parsing.Token as Token
import Stlc.Syntax (Symbol(..), Term(..), Type(..))


-- Lexing

tokenDef :: Token.LanguageDef
tokenDef = Token.LanguageDef (Token.unGenLanguageDef Lang.emptyDef)
  { commentStart = "{-"
  , commentEnd = "-}"
  , commentLine = "--"
  , nestedComments = true
  , identStart = Token.letter
  , identLetter = Token.alphaNum <|> Token.oneOf ['_', '\'']
  , opStart = op'
  , opLetter = op'
  , reservedOpNames = [
    -- type constructor operators
    "->", ".", "+", "*",
    -- application and type declaration operators
    "\\", ":"
    ]
  , reservedNames = ["type"]
  , caseSensitive = true
  }
  where
    op' :: forall m . (Monad m) => ParserT String m Char
    op' = Token.oneOf [':', '\\', '.', '+', '*', '>']


lexer :: Token.TokenParser
lexer = Token.makeTokenParser tokenDef


-- Parsing

type Parser = ParserT String Identity


data SymbolKind = UpperCase | LowerCase

parseSymbol :: SymbolKind -> Parser Symbol
parseSymbol symbolKind = do
  name <- lexer.identifier
  guard $ Maybe.maybe false f $ codePointAt 0 name
  pure $ Symbol name
  where
    f = case symbolKind of
      UpperCase -> isUpper
      LowerCase -> isLower

parseType :: Parser Type
parseType = do
  lexer.reserved "type"
  symbol <- parseSymbol UpperCase
  pure $ TyAtom symbol


parseVariable :: Parser Term
parseVariable = do
  symbol <- parseSymbol LowerCase
  pure $ TmVariable symbol


parseAbstraction :: Parser Term -> Parser Term
parseAbstraction termParser = do
  lexer.reservedOp "\\"
  symbols <- NonEmpty.some (parseSymbol LowerCase)
  lexer.reservedOp "."
  term <- termParser
  pure $ TmAbstraction symbols term


parseTerm :: Parser Term
parseTerm = fix \t ->
  parseAbstraction t <|> parseVariable