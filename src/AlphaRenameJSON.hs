
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# OPTIONS_GHC -Wall #-}

module AlphaRenameJSON where

import Control.Monad.State
import Data.Char (isSpace, isPunctuation, isSymbol, isAlpha, isDigit)
import Data.Map  (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- 1. Define the Task data structure
--------------------------------------------------------------------------------

data Task = Task
  { task_id      :: Text
  , poly_type    :: Text
  , signature    :: Text
  , code         :: Text
  , dependencies :: [Text]
  } deriving (Show, Generic)

instance FromJSON Task
instance ToJSON   Task

--------------------------------------------------------------------------------
-- 2. The renaming environment
--------------------------------------------------------------------------------

data RenameEnv = RenameEnv
  { varEnv      :: Map Text Text  -- ^ rename table for lower-case vars/funs
  , typeEnv     :: Map Text Text  -- ^ rename table for uppercase names
  , classEnv    :: Map Text Text  -- ^ (optionally separate for classes)
  , opEnv       :: Map Text Text  -- ^ rename table for operators
  , nextVarNum  :: Int
  , nextTypeNum :: Int
  , nextClassNum:: Int
  , nextOpNum   :: Int
  } deriving (Show)

initRenameEnv :: RenameEnv
initRenameEnv = RenameEnv
  { varEnv      = Map.empty
  , typeEnv     = Map.empty
  , classEnv    = Map.empty
  , opEnv       = Map.empty
  , nextVarNum  = 1
  , nextTypeNum = 1
  , nextClassNum= 1
  , nextOpNum   = 1
  }

type Renamer = State RenameEnv

--------------------------------------------------------------------------------
-- 3. Public function to rename a single Task
--------------------------------------------------------------------------------

alphaRenameTask :: Task -> Task
alphaRenameTask task =
  evalState (do
    sig'  <- renameString (signature task)
    code' <- renameString (code task)
    deps' <- mapM renameString (dependencies task)
    pure task
      { signature    = sig'
      , code         = code'
      , dependencies = deps'
      }
  ) initRenameEnv

--------------------------------------------------------------------------------
-- 4. Reserved keywords/operators to skip rewriting
--------------------------------------------------------------------------------

reservedKeywords :: [Text]
reservedKeywords =
  [ "data", "newtype", "type", "class", "instance"
  , "deriving", "let", "in", "where", "do", "of"
  , "case", "module", "import", "qualified", "as", "hiding"
  ]

reservedOperators :: [Text]
reservedOperators =
  [ "::", "->", "=", "=>", "<-", "()", "..", "\\", "|"
  ]

isReserved :: Text -> Bool
isReserved tok =
  tok `elem` reservedKeywords || tok `elem` reservedOperators

--------------------------------------------------------------------------------
-- 5. Naive text-based rename:
--    We tokenize the string, then handle special cases like `error "<msg>"`.
--------------------------------------------------------------------------------

renameString :: Text -> Renamer Text
renameString txt = do
  let tokens = tokenize txt
  renamed <- renameTokens tokens
  pure (T.concat renamed)

--------------------------------------------------------------------------------
-- 5a. The main loop for processing tokens
--------------------------------------------------------------------------------

renameTokens :: [Text] -> Renamer [Text]
renameTokens [] = pure []
renameTokens [t] = do
  t' <- renameToken t
  pure [t']
renameTokens (t1 : t2 : rest)
  -- If we see the exact token "error" followed (immediately) by a string literal:
  | t1 == "error" && isStringLiteral t2 =
      -- Keep them exactly as-is: no renaming for 'error' or the literal
      (t1 :) . (t2 :) <$> renameTokens rest

  -- Otherwise, rename t1 as usual, then proceed
  | otherwise = do
      t1' <- renameToken t1
      more <- renameTokens (t2 : rest)
      pure (t1' : more)

--------------------------------------------------------------------------------
-- 6. Tokenization
--    We'll do a naive approach but add special handling for string literals
--    starting with a double quote, to keep them as a single token if possible.
--------------------------------------------------------------------------------

tokenize :: Text -> [Text]
tokenize = go []
  where
    go acc t
      | T.null t = reverse acc
      | otherwise =
          let (tok, rest) = nextToken t
          in go (tok : acc) rest

-- nextToken tries:
--    - If we see '"' => parse a string literal up to the next '"'
--    - If we see whitespace => that's one token
--    - Else parse either an alphanumeric chunk or punctuation chunk
nextToken :: Text -> (Text, Text)
nextToken text
  | T.null text = ("", "")
  | isSpace (T.head text) =
      -- just one whitespace char is a token
      let ws = T.take 1 text
      in (ws, T.drop 1 text)
  | T.head text == '"' =
      -- parse a naive string literal up to the next unescaped '"'
      let (literal, rest) = grabStringLiteral text
      in (literal, rest)
  | otherwise =
      -- parse either word-like or operator chunk
      let (chunk, rest) = splitFirstChunk text
      in (chunk, rest)

-- Grabs everything from the initial '"' to the next '"' (or end of string).
-- This is naive (doesn't handle escaped quotes).
grabStringLiteral :: Text -> (Text, Text)
grabStringLiteral txt =
  -- We assume txt starts with '"'
  let (_, tail1) = T.splitAt 1 txt  -- skip the initial quote
      (inside, rest) = T.breakOn "\"" tail1
  in case T.uncons rest of
       Just ('"', afterQuote) ->
         -- we found a closing quote
         let lit = "\"" <> inside <> "\""  -- put them back
         in (lit, afterQuote)
       _ ->
         -- no closing quote => just return everything
         (txt, "")  -- fallback

-- Splits one chunk: if first char is alphanumeric or '_', parse a "word",
-- otherwise parse punctuation/symbol chunk.
splitFirstChunk :: Text -> (Text, Text)
splitFirstChunk t =
  if isWordChar (T.head t)
     then T.span isWordChar t
     else T.span (\c -> isPunctuation c || isSymbol c) t

isWordChar :: Char -> Bool
isWordChar c = isAlpha c || isDigit c || c == '_'

-- Checks if a token looks like a string literal (starts & ends with double quotes)
isStringLiteral :: Text -> Bool
isStringLiteral tk =
  T.length tk >= 2
  && T.head tk == '"'
  && T.last tk == '"'

--------------------------------------------------------------------------------
-- 7. renameToken
--------------------------------------------------------------------------------

renameToken :: Text -> Renamer Text
renameToken tok
  -- 1. If it's all whitespace, keep as-is
  | T.all isSpace tok
  = pure tok

  -- 2. If it's in our reserved set, keep as-is
  | isReserved tok
  = pure tok

  -- 3. If purely punctuation/symbol => rename operator
  | T.all (\c -> isPunctuation c || isSymbol c) tok
  , not (T.null tok)
  = renameOperator tok

  -- 4. If uppercase first letter => rename type/constructor/class
  | let c = T.head tok
  , c >= 'A' && c <= 'Z'
  = renameTypeOrClass tok

  -- 5. If lowercase first letter => rename variable
  | let c = T.head tok
  , c >= 'a' && c <= 'z'
  = renameVar tok

  -- 6. Otherwise, keep as-is (numbers, string-literals, etc.)
  | otherwise
  = pure tok

--------------------------------------------------------------------------------
-- 7a. renameOperator
--------------------------------------------------------------------------------

renameOperator :: Text -> Renamer Text
renameOperator orig = do
  env <- get
  case Map.lookup orig (opEnv env) of
    Just newName -> pure newName
    Nothing -> do
      let n       = nextOpNum env
          freshOp = T.pack ("op" <> show n)
      put env { opEnv = Map.insert orig freshOp (opEnv env)
              , nextOpNum = n + 1 }
      pure freshOp

--------------------------------------------------------------------------------
-- 7b. renameTypeOrClass
--     For simplicity, unify type constructors and classes in one map.
--------------------------------------------------------------------------------

renameTypeOrClass :: Text -> Renamer Text
renameTypeOrClass orig = do
  env <- get
  -- check if it’s known
  case Map.lookup orig (typeEnv env) of
    Just newTy -> pure newTy
    Nothing -> do
      let n       = nextTypeNum env
          freshTy = T.pack ("T" <> show n)
      put env
        { typeEnv = Map.insert orig freshTy (typeEnv env)
        , nextTypeNum = n + 1
        }
      pure freshTy

--------------------------------------------------------------------------------
-- 7c. renameVar
--------------------------------------------------------------------------------

renameVar :: Text -> Renamer Text
renameVar orig = do
  env <- get
  case Map.lookup orig (varEnv env) of
    Just newName -> pure newName
    Nothing -> do
      let n       = nextVarNum env
          freshFn = "f" <> T.pack (show n)
      put env
        { varEnv = Map.insert orig freshFn (varEnv env)
        , nextVarNum = n + 1
        }
      pure freshFn
