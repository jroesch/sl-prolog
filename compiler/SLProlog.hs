{-# LANGUAGE FlexibleInstances #-}
module SLProlog where

import Control.Applicative (pure, (<*), (*>), (<*>), (<$>))
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Text.Parsec
import Text.Parsec.String
import Text.Printf

main :: IO ()
main = mainWithFile SWIPL "example.spl"

data Backend = WAM | SWIPL
             deriving (Eq, Show)

mainWithFile :: Backend -> String -> IO ()
mainWithFile backend filename = 
    case backend of
      SWIPL -> do
        ast <- SLProlog.parse filename
        putStrLn $ toProlog ast
      WAM -> error "not yet implemented"

data Program = Program Atom [Import] [Atom] [TypeDecl] [Clause] deriving (Eq, Show)

data Import = Import deriving (Eq, Show)

data TypeDecl = TypeDecl Atom [Constructor]
              deriving (Eq, Show)
              
data Constructor = MkCons Atom [Type] deriving (Eq, Show) 

data Type = T deriving (Eq, Show)

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
    (modname, imports, exports) <- moduleP
    tds <- many typeDeclP <* whitespace
    clauses <- many $ clauseP <* whitespace
    return $ Program modname [] [] tds clauses 

moduleP :: Parser (Atom, [Import], [String]) 
moduleP = do
    string "module" <* whitespace
    atom <- atomP <* whitespace
    char '.' <* whitespace
    return (atom, [], [])

typeDeclP :: Parser TypeDecl
typeDeclP = do
    string "type" <* whitespace
    name <- atomP <* whitespace
    string ":-" <* whitespace
    cons <- consP `sepBy1` (whitespace *> (char '|') <* whitespace)
    char '.'
    return $ TypeDecl name cons

consP :: Parser Constructor
consP = do
        name <- atomP
        types <- optionMaybe $ parens $ typeP `sepBy1` (char ',' <* whitespace)
        return $ MkCons name (fromMaybe [] types)

typeP :: Parser Type
typeP = atomP >> return T

parens :: Parser a -> Parser a
parens p = between (char '(' <* whitespace) (char ')' <* whitespace) p

clauseP :: Parser Clause
clauseP = do
    name <- atomP <* whitespace
    string ":-" <* whitespace
    body <- bodyP
    return $ Clause name [] body

bodyP :: Parser Term
bodyP = UnifiableTerm <$> utermP 

utermP :: Parser UTerm 
utermP = AnAtom <$> atomP <* char '.'

atomP :: Parser Atom
atomP = MkAtom <$> atomString
  where atomString = pure (:) <*> lower <*> (many letter)

class ToProlog a where
    toProlog :: a -> String

instance ToProlog Program where
    toProlog (Program name imports exports types clauses) = 
      concat $ [genModule name imports, toProlog types, "\n", (toProlog clauses)]

genModule :: Atom -> [Import] -> String
genModule name imports = printf ":- module(%s, %s).\n\n" (toProlog name) (show imports)

instance ToProlog [TypeDecl] where
    toProlog cs = concat $ map ((++ "\n") . toProlog) cs

instance ToProlog TypeDecl where
    toProlog (TypeDecl name cons) = printf "; type %s ::= %s.\n" (toProlog name) (toProlog cons)

instance ToProlog [Constructor] where
    toProlog cs = intercalate " | " $ map toProlog cs
    
instance ToProlog Constructor where
    toProlog (MkCons atom args) = 
      case args of
        [] -> toProlog atom
        _  -> printf "%s(%s)" (toProlog atom) (intercalate "," $ map toProlog args)
        
instance ToProlog Type where
    toProlog T = "someType" 

instance ToProlog [Clause] where
    toProlog cs = concat $ map ((++ "\n") . toProlog) cs

instance ToProlog Clause where
    toProlog (Clause atom args body) = printf "%s%s :- %s.\n" (toProlog atom) args' (toProlog body)
      where args' = case args of
                      [] -> ""
                      as -> show as

instance ToProlog Term where
    toProlog (UnifiableTerm uterm) = toProlog uterm
    toProlog Conjunction = error "conj"
    toProlog Disjunction = error "disj"

instance ToProlog UTerm where
    toProlog (AnAtom atom) = toProlog atom

instance ToProlog Atom where
    toProlog (MkAtom str) = str
