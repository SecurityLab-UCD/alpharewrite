{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module AlphaRewrite (alphaRewriteTask) where

import Control.Monad.State
import Data.Char (isSpace, isPunctuation, isSymbol, isAlpha, isDigit)
import Data.Map  (Map)
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Text as T


import Task

--------------------------------------------------------------------------------
-- 2. The renaming environment
--------------------------------------------------------------------------------

data RewriteEnv = RewriteEnv
  { varEnv      :: Map Text Text  -- ^ rewrite table for lower-case vars/funs
  , typeEnv     :: Map Text Text  -- ^ rewrite table for uppercase names
  , classEnv    :: Map Text Text  -- ^ (optionally separate for classes)
  , opEnv       :: Map Text Text  -- ^ rewrite table for operators
  , nextVarNum  :: Int
  , nextTypeNum :: Int
  , nextClassNum:: Int
  , nextOpNum   :: Int
  } deriving (Show)

initRewriteEnv :: RewriteEnv
initRewriteEnv = RewriteEnv
  { varEnv      = Map.empty
  , typeEnv     = Map.empty
  , classEnv    = Map.empty
  , opEnv       = Map.empty
  , nextVarNum  = 1
  , nextTypeNum = 1
  , nextClassNum= 1
  , nextOpNum   = 1
  }

type Rewriter = State RewriteEnv

--------------------------------------------------------------------------------
-- 3. Public function to rewrite a single Task
--------------------------------------------------------------------------------

alphaRewriteTask :: Task -> Task
alphaRewriteTask task =
  evalState (do
    sig'  <- rewriteString (signature task)
    code' <- rewriteString (code task)
    deps' <- mapM rewriteString (dependencies task)
    pure task
      { signature    = sig'
      , code         = code'
      , dependencies = deps'
      }
  ) initRewriteEnv

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
-- 5. Naive text-based rewrite:
--    We tokenize the string, then handle special cases like `error "<msg>"`.
--------------------------------------------------------------------------------

rewriteString :: Text -> Rewriter Text
rewriteString txt = do
  let tokens = tokenize txt
  rewrited <- rewriteTokens tokens
  pure (T.concat rewrited)

--------------------------------------------------------------------------------
-- 5a. The main loop for processing tokens
--------------------------------------------------------------------------------

rewriteTokens :: [Text] -> Rewriter [Text]
rewriteTokens [] = pure []
rewriteTokens [t] = do
  t' <- rewriteToken t
  pure [t']
rewriteTokens (t1 : t2 : rest)
  -- If we see the exact token "error" followed (immediately) by a string literal:
  | t1 == "error" && isStringLiteral t2 =
      -- Keep them exactly as-is: no renaming for 'error' or the literal
      (t1 :) . (t2 :) <$> rewriteTokens rest

  -- Otherwise, rewrite t1 as usual, then proceed
  | otherwise = do
      t1' <- rewriteToken t1
      more <- rewriteTokens (t2 : rest)
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
-- 7. rewriteToken
--------------------------------------------------------------------------------

rewriteToken :: Text -> Rewriter Text
rewriteToken tok
  -- 1. If it's all whitespace, keep as-is
  | T.all isSpace tok
  = pure tok

  -- 2. If it's in our reserved set, keep as-is
  | isReserved tok
  = pure tok

  -- 3. If purely punctuation/symbol => rewrite operator
  | T.all (\c -> isPunctuation c || isSymbol c) tok
  , not (T.null tok)
  = rewriteOperator tok

  -- 4. If uppercase first letter => rewrite type/constructor/class
  | let c = T.head tok
  , c >= 'A' && c <= 'Z'
  = rewriteTypeOrClass tok

  -- 5. If lowercase first letter => rewrite variable
  | let c = T.head tok
  , c >= 'a' && c <= 'z'
  = rewriteVar tok

  -- 6. Otherwise, keep as-is (numbers, string-literals, etc.)
  | otherwise
  = pure tok

--------------------------------------------------------------------------------
-- 7a. rewriteOperator
--------------------------------------------------------------------------------

rewriteOperator :: Text -> Rewriter Text
rewriteOperator orig = do
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
-- 7b. rewriteTypeOrClass
--     For simplicity, unify type constructors and classes in one map.
--------------------------------------------------------------------------------

rewriteTypeOrClass :: Text -> Rewriter Text
rewriteTypeOrClass orig = do
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
-- 7c. rewriteVar
--------------------------------------------------------------------------------

rewriteVar :: Text -> Rewriter Text
rewriteVar orig = do
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
