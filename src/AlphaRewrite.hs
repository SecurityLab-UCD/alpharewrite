{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module AlphaRewrite (alphaRewriteTask) where

import GHC.Generics (Generic)
import Language.Haskell.Exts
  ( -- Parsing
    ParseMode(..), ParseResult(..), defaultParseMode, parseDeclWithMode
  , Extension(..), KnownExtension(..)
    -- AST Types
  , Decl(..), Name(..), SrcSpanInfo
  , prettyPrint
  )
import qualified Data.Text as T
import Data.List (foldl')
import Control.Monad (forM)
import Data.Generics (everywhere, mkT)
import Task (Task(..))

--------------------------------------------------------------------------------
-- 1) Decl type alias for convenience
--------------------------------------------------------------------------------
type MyDecl = Decl SrcSpanInfo

--------------------------------------------------------------------------------
-- 2) Parsing a single top-level Haskell declaration
--------------------------------------------------------------------------------

parseOneDecl :: String -> Either String MyDecl
parseOneDecl src =
  case parseDeclWithMode defaultMode src of
    ParseOk d        -> Right d
    ParseFailed _ err -> Left ("Parse error: " ++ err)
  where
    defaultMode :: ParseMode
    defaultMode = defaultParseMode
      { extensions = map EnableExtension
          [ MultiParamTypeClasses
          , FlexibleContexts
          , FlexibleInstances
          -- Further work: support more type extensions
          -- , GADTs
          -- , TypeFamilies
          ]
      }

--------------------------------------------------------------------------------
-- 3) Convert a Decl back to string
--------------------------------------------------------------------------------

declToString :: MyDecl -> String
declToString = prettyPrint

--------------------------------------------------------------------------------
-- 4) Extract names (before "::") from a type signature
--    E.g. "(==) :: Eq a => a -> a -> Bool" -> ["(==)"]
--         "f, g :: Int -> Int" -> ["f", "g"]
--         "not :: Bool -> Bool" -> ["not"]
--------------------------------------------------------------------------------

extractNamesFromTypeSig :: String -> Either String [String]
extractNamesFromTypeSig src = do
  decl <- parseOneDecl src
  case decl of
    TypeSig _ names _ -> Right (map nameToString names)
    _ -> Left ("Not a type signature: " ++ src)

-- Convert Name (with annotation) to a user-friendly string:
--   Ident "not"   -> "not"
--   Symbol "=="   -> "(==)"
nameToString :: Name SrcSpanInfo -> String
nameToString (Ident  _ s) = s
nameToString (Symbol _ s) = "(" ++ s ++ ")"

--------------------------------------------------------------------------------
-- 5) AST-based Renaming
--------------------------------------------------------------------------------

-- | Rename all occurrences of one old name with a new name in an AST of type
--   'Decl SrcSpanInfo'.
renameName :: String -> String -> MyDecl -> MyDecl
renameName old new = everywhere (mkT goName)
  where
    goName :: Name SrcSpanInfo -> Name SrcSpanInfo
    goName (Ident l s)
      -- If the name is exactly 'old', rename it
      | s == old             = Ident l new
      -- Sometimes the 'old' might be e.g. "(==)", so compare with parentheses removed
      | "(" ++ s ++ ")" == old = Ident l new
      | otherwise            = Ident l s

    goName (Symbol l s)
      | s == old             = Symbol l new
      | "(" ++ s ++ ")" == old = Symbol l new
      | otherwise            = Symbol l s

-- | Apply multiple renamings in sequence.
renameAll :: [(String, String)] -> MyDecl -> MyDecl
renameAll pairs decl =
  foldl' (\acc (o, n) -> renameName o n acc) decl pairs

--------------------------------------------------------------------------------
-- 6) The main function: alphaRewriteTask
--
--    - Gathers function/operator names from the signature + dependencies
--      in the order they appear.
--    - Builds a rename map (e.g. [("(==)","f1"), ("not","f2"), ("(/=)","f3")]).
--    - Parses & renames 'signature', 'dependencies', and 'code'.
--    - Returns a new 'Task' with updated fields.
--------------------------------------------------------------------------------

alphaRewriteTask :: Task -> Either String Task
alphaRewriteTask t = do
  ----------------------------------------------------------------
  -- (A) Gather all old names from:
  --     1) The 'signature'
  --     2) Each line in 'dependencies'
  ----------------------------------------------------------------
  sigNames <- extractNamesFromTypeSig (T.unpack $ signature t)

  depNamesList <- forM (dependencies t) $ \depStr ->
    extractNamesFromTypeSig (T.unpack depStr)

  let allNames = sigNames ++ concat depNamesList
      renameMap = zip allNames [ "f" ++ show i | i <- [1..] ]
       -- e.g. [("(==)","f1"),("not","f2"),("(/=)","f3"),...]

  ----------------------------------------------------------------
  -- (B) Parse & rename the signature
  ----------------------------------------------------------------
  sigDecl <- parseOneDecl (T.unpack $ signature t)
  let sigRen  = renameAll renameMap sigDecl
      newSig  = declToString sigRen

  ----------------------------------------------------------------
  -- (C) Parse & rename each dependency
  ----------------------------------------------------------------
  depDecls <- mapM (parseOneDecl . T.unpack) (dependencies t)
  let depRens = map (renameAll renameMap) depDecls
      newDeps = map declToString depRens

  ----------------------------------------------------------------
  -- (D) Parse & rename the code
  --     (Assumes 'code' is also a single top-level declaration)
  ----------------------------------------------------------------
  codeDecl <- parseOneDecl (T.unpack $ code t)
  let codeRen = renameAll renameMap codeDecl
      newCode = declToString codeRen

  ----------------------------------------------------------------
  -- (E) Return updated Task
  ----------------------------------------------------------------
  pure t
    { signature    = T.pack newSig
    , dependencies = map T.pack newDeps
    , code         = T.pack newCode
    }
