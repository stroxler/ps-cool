module Test.Main
  ( main
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Class.Console (log)
import Parsing (runParser)
import Stlc.Parser as Parser

verboseParserTests :: Boolean
verboseParserTests = true

parserTestCase :: forall a. Show a => Parser.Parser a -> String -> Effect Unit
parserTestCase parser code =
  case runParser code parser of
  Left err -> do
    log $ "could not parse `" <> code <> "`: "
    log $ "  " <> show err
  Right ty ->
    if verboseParserTests
    then log $ show ty
    else pure unit


traverseTestCases :: forall a. Show a => Parser.Parser a -> Array String -> Effect Unit
traverseTestCases parser = traverse_ (parserTestCase parser)


main :: Effect Unit
main = do
  traverseTestCases Parser.parseType
    ["type A"
    , "type AbC"
    , "type oops"
    ]
  traverseTestCases Parser.parseVariable
    ["x"
    , "xYz"
    , "X"]
  {- Note: the space after the . is optional unless the next
     symbol is an operator. But to avoid surprises on `.\` code (which
     will be parsed as one symbol if there's no space) it's a good
     idea to *always* put a blank space. -}
  traverseTestCases (Parser.parseAbstraction Parser.parseTerm)
    [ "\\x. x"
    , "\\x y. x"
    , "\\x. \\y. y"
    ]
  {- For convenience it was easier not to write a stand-alone
     parseApplication, so we just use parseTerm. -}
  traverseTestCases Parser.parseTerm
    [ "f a"
    , "f \\g x. g x"
    ]
  traverseTestCases Parser.parseTerm
    [ "(x)"
    , "f a"
    , "(f a) b"
    , "f a b"
    ]