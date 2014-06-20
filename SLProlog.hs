{-# LANGUAGE FlexibleInstances #-}
module SLProlog where

import Control.Applicative (pure, (<*), (*>), (<*>), (<$>))
import Text.Parsec
import Text.Parsec.String
import Text.Printf

main :: IO ()
main = mainWithFile "example.spl"

data Backend = WAM | SWIPL
             deriving (Eq, Show)

mainWithFile :: Backend -> String -> IO ()
mainWithFile backend filename = do
    ast <- SLProlog.parse filename
    putStrLn $ toProlog ast

data Program = Program String [Import] [String] [TypeDecl] [Clause] deriving (Eq, Show)

data Import = Import deriving (Eq, Show)

data TypeDecl = Bogus
              deriving (Eq, Show)

data Clause = Clause Atom [UTerm] Term
            deriving (Eq, Show)

data Term = UnifiableTerm UTerm
          | Conjunction
          | Disjunction
          deriving (Eq, Show)

data UTerm = MkUTerm 
           | AnAtom Atom
           deriving (Eq, Show)

data Atom = MkAtom String
          deriving (Eq, Show)

type SLParseError = String

-- This is partial because we want to tear down the whole world when he hit
-- a parse error.
parse :: String -> IO Program
parse filename = do
    input <- readFile filename 
    case runParser programParser () filename input of
      Left e -> error $ show e
      Right r -> return r

whitespace :: Parser ()
whitespace = skipMany $ space

programParser :: Parser Program
programParser = do
    whitespace
    -- tds <- many typeDeclP <* whitespace
    clauses <- many $ clauseP <* whitespace
    return $ Program {- tds -} "DummyModule" [] [] [] clauses 

typeDeclP :: Parser TypeDecl
typeDeclP = return Bogus

clauseP :: Parser Clause
clauseP = do
    name <- atomP <* whitespace
    string ":-" <* whitespace
    body <- bodyP
    return $ Clause name [] body

bodyP :: Parser Term
bodyP = UnifiableTerm <$> AnAtom <$> atomP <* char '.'

atomP :: Parser Atom
atomP = MkAtom <$> atomString
  where atomString = pure (:) <*> lower <*> (many letter)

class ToProlog a where
    toProlog :: a -> String

instance ToProlog Program where
    toProlog (Program name imports exports types clauses) = 
      concat $ ["; Program\n", (show types), "\n", (toProlog clauses)]
   
instance ToProlog [Clause] where
    toProlog cs = concat $ map ((++ "\n") . toProlog) cs

instance ToProlog Clause where
    toProlog (Clause atom args body) = printf "%s%s :- %s.\n" (show atom) args' (show body)
      where args' = case args of
                      [] -> ""
                      as -> show as
