module Stlc.Parser
  where

import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Control.Lazy (fix)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.CodePoint.Unicode (isLower, isUpper)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.NonEmpty as NonEmpty
import Data.String (codePointAt)
import Parsing (ParserT)
import Parsing.Language as Lang
import Parsing.Token as Token
import Stlc.Syntax (Symbol(..), Term(..), Type(..))


-- Lexing - could be defined manually but LanguageDef makes this easy :)

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


-- Parsing - this we do define by hand in terms of the combinators

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
  TyAtom <$> parseSymbol UpperCase

parseVariable :: Parser Term
parseVariable = do
  symbol <- parseSymbol LowerCase
  pure $ TmVariable symbol


parseAbstraction :: Parser Term -> Parser Term
parseAbstraction termParser = do
  lexer.reservedOp "\\"
  parameters <- NonEmptyArray.some (parseSymbol LowerCase)
  lexer.reservedOp "."
  term <- termParser
  pure $ TmAbstraction parameters term



parseWrapped :: Parser Term ->  Parser Term
parseWrapped termParser = lexer.parens termParser



{- Application parsing is pretty tricky for two related but
   distinct reasons:
   - the prefix of an application is any term, which means a naive
     implementation where we just parse a term for the LHS will blow the stack,
     since it will call back into itself.
   - we want it to be left-associative (this is where the problem above
     originates). The most obvious way to solve the problem above is to force
     the LHS to be a simple term, but that makes it right-associative.
   
   The solution here is to abandon the simple recursive-descent monadic
   parsing style at the term top-level and instead parse a top-level term
   as a leading simple term plus possibly a tail of additional terms.

   We then manually construct the left-associative application sequence
   if there are multiple terms, or return the single simpleTerm if not.
   -}
parseTermFixable :: Parser Term -> Parser Term
parseTermFixable t = do
  term0 <- parseSimpleTerm
  parseApplication term0 <|> pure term0
  where
    parseApplication :: Term -> Parser Term
    parseApplication term0 = do
      makeApplication term0 <$> NonEmptyArray.some parseSimpleTerm
    makeApplication :: Term -> (NonEmptyArray Term) -> Term
    makeApplication term0 arguments =
      case NonEmptyArray.fromArray maybeMoreTerms of
        Just moreTerms -> makeApplication thisApplication moreTerms
        _ -> thisApplication
      where
        (NonEmpty.NonEmpty term1 maybeMoreTerms) = NonEmptyArray.toNonEmpty arguments
        thisApplication = TmApplication term0 term1
    parseSimpleTerm :: Parser Term
    parseSimpleTerm =
      parseVariable <|>
      parseAbstraction t <|>
      parseWrapped t


parseTerm :: Parser Term
parseTerm = fix parseTermFixable