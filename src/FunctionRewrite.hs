{-# LANGUAGE OverloadedStrings #-}

module FunctionRewrite (renameFunctions, extractNamesFromTypeSig, nameToString) where


-- AST Types

import Control.Monad (forM)
import Data.Generics (everywhere, mkT)
import Data.List (foldl', isPrefixOf, isSuffixOf)
import qualified Data.Text as T
import Language.Haskell.Exts
  ( Decl (..),
    Extension (..),
    KnownExtension (..),
    Name (..),
    ParseMode (..),
    ParseResult (..),
    SrcSpanInfo,
    defaultParseMode,
    parseDeclWithMode,
    prettyPrint,
  )
import Task (Task (..))
import DeclParser (MyDecl, parseOneDecl, declToString)

--------------------------------------------------------------------------------
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
    _ -> Right []

-- Left ("Not a type signature: " ++ src)

-- Convert Name (with annotation) to a user-friendly string:
--   Ident "not"   -> "not"
--   Symbol "=="   -> "(==)"
nameToString :: Name SrcSpanInfo -> String
nameToString (Ident _ s) = s
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
      | s == old = Ident l new
      -- Sometimes the 'old' might be e.g. "(==)", so compare with parentheses removed
      | "(" ++ s ++ ")" == old = Ident l new
      | otherwise = Ident l s
    goName (Symbol l s)
      | s == old = Symbol l new
      | "(" ++ s ++ ")" == old = Symbol l new
      | otherwise = Symbol l s

-- | Apply multiple renamings in sequence.
renameAll :: [(String, String)] -> MyDecl -> MyDecl
renameAll pairs decl =
  foldl' (\acc (o, n) -> renameName o n acc) decl pairs

--------------------------------------------------------------------------------
-- 6) The main function: renameFunctions
--
--    - Gathers function/operator names from the signature + dependencies
--      in the order they appear.
--    - Builds a rename map (e.g. [("(==)","f1"), ("not","f2"), ("(/=)","f3")]).
--    - Parses & renames 'signature', 'dependencies', and 'code'.
--    - Returns a new 'Task' with updated fields.
--------------------------------------------------------------------------------

renameFunctions :: Task -> Either String Task
renameFunctions t = do
  ----------------------------------------------------------------
  -- (A) Gather all old names from:
  --     1) The 'signature'
  --     2) Each line in 'dependencies'
  ----------------------------------------------------------------
  sigNames <- extractNamesFromTypeSig (T.unpack $ signature t)

  depNamesList <- forM (dependencies t) $ \depStr ->
    extractNamesFromTypeSig (T.unpack depStr)

  let allNames = sigNames ++ concat depNamesList
      renameMap = zip allNames ["f" ++ show i | i <- [1..]]
      -- Identify which renamed names came from original operators (e.g. "(==)")
      operatorRenames =
        [ new
        | (orig, new) <- renameMap
        , "(" `isPrefixOf` orig && ")" `isSuffixOf` orig
        ]

  ----------------------------------------------------------------
  -- (B) Parse & rename the signature
  ----------------------------------------------------------------
  sigDecl <- parseOneDecl (T.unpack $ signature t)
  let sigRen = renameAll renameMap sigDecl
      newSig = unParenOps (declToString sigRen)

  ----------------------------------------------------------------
  -- (C) Parse & rename each dependency
  ----------------------------------------------------------------
  depDecls <- mapM (parseOneDecl . T.unpack) (dependencies t)
  let depRens = map (renameAll renameMap) depDecls
      newDeps = map (unParenOps . declToString) depRens

  ----------------------------------------------------------------
  -- (D) Parse & rename the code
  --     (Assumes 'code' is also a single top-level declaration)
  ----------------------------------------------------------------
  codeDecl <- parseOneDecl (T.unpack $ code t)
  let codeRen = renameAll renameMap codeDecl
      newCode =
        let addTicks w
              | w `elem` operatorRenames = "`" ++ w ++ "`"
              | otherwise = w
        in unwords . map addTicks . words . declToString $ codeRen

  ----------------------------------------------------------------
  -- (E) Return updated Task
  ----------------------------------------------------------------
  pure
    t
      { signature = T.pack newSig
      , dependencies = map T.pack newDeps
      , code = T.pack newCode
      }

unParenOps :: String -> String
unParenOps input =
  let removeParen s
        | "(" `isPrefixOf` s && ")" `isSuffixOf` s = tail (init s)
        | otherwise = s
  in unwords . map removeParen . words $ input
