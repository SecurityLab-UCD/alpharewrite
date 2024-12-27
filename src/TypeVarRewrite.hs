{-# LANGUAGE OverloadedStrings #-}

module TypeVarRewrite
  ( rewriteTypeVars,
    rewriteOneTypeSignature,
    extractTypeFromDecl,
    gatherTypeVars,
    renameTypeVars,
    rewriteDependencies
  ) where

import Control.Monad (forM)
import Data.Generics (everywhere, mkT, listify)
import Data.List (nub)
import qualified Data.Text as T
import Language.Haskell.Exts
  ( Decl (..),
    Extension (..),
    KnownExtension (..),
    Name (..),
    ParseMode (..),
    ParseResult (..),
    SrcSpanInfo,
    TyVarBind (..),
    Type (..),
    defaultParseMode,
    parseDeclWithMode,
    prettyPrint
  )
import Task (Task(..))
import DeclParser (MyDecl, parseOneDecl, declToString)

--------------------------------------------------------------------------------
-- 1) Extract the 'Type SrcSpanInfo' from a 'TypeSig' declaration if present
--------------------------------------------------------------------------------

extractTypeFromDecl :: MyDecl -> Maybe (Type SrcSpanInfo)
extractTypeFromDecl (TypeSig _ _ ty) = Just ty
extractTypeFromDecl _                = Nothing

--------------------------------------------------------------------------------
-- 2) Gather all type variables (e.g. a, b, c...) from a 'Type'
--    ignoring the list constructor [] and any capitalized types
--------------------------------------------------------------------------------

gatherTypeVars :: Type SrcSpanInfo -> [String]
gatherTypeVars ty =
  let allVars = listify isTyVar ty
      varNames = [ nameToString n | TyVar _ n <- allVars ]
  in nub varNames
  where
    isTyVar :: Type SrcSpanInfo -> Bool
    isTyVar (TyVar _ _) = True
    isTyVar _           = False

    nameToString :: Name SrcSpanInfo -> String
    nameToString (Ident _ s)  = s
    nameToString (Symbol _ s) = s

--------------------------------------------------------------------------------
-- 3) Renaming type variables in a 'Type'
--------------------------------------------------------------------------------

renameTypeVars :: [(String, String)] -> Type SrcSpanInfo -> Type SrcSpanInfo
renameTypeVars pairs = everywhere (mkT go)
  where
    go :: Type SrcSpanInfo -> Type SrcSpanInfo
    go t@(TyVar l n) =
      let old = nameToString n
       in case lookup old pairs of
            Just new -> TyVar l (replaceName n new)
            Nothing  -> t
    go t = t

    nameToString :: Name SrcSpanInfo -> String
    nameToString (Ident _ s)  = s
    nameToString (Symbol _ s) = s

    replaceName :: Name SrcSpanInfo -> String -> Name SrcSpanInfo
    replaceName (Ident l _)  new = Ident l new
    replaceName (Symbol l _) new = Symbol l new

--------------------------------------------------------------------------------
-- 4) Rewrite a single type signature
--------------------------------------------------------------------------------

rewriteOneTypeSignature :: String -> Either String String
rewriteOneTypeSignature src = do
  decl <- parseOneDecl src
  case extractTypeFromDecl decl of
    Nothing -> Right src
    Just ty ->
      let vars      = gatherTypeVars ty
          renameMap = zip vars [ "t" ++ show i | i <- [1..] ]
          tyRen     = renameTypeVars renameMap ty
          newDecl   = case decl of
                        TypeSig l names _ -> TypeSig l names tyRen
                        _                 -> decl
      in Right (declToString newDecl)

--------------------------------------------------------------------------------
-- 5) Rewrite dependencies
--------------------------------------------------------------------------------

rewriteDependencies :: [T.Text] -> Either String [T.Text]
rewriteDependencies deps = do
  forM deps $ \depLine -> do
    let depStr = T.unpack depLine
    newDepStr <- rewriteOneTypeSignature depStr
    pure (T.pack newDepStr)

--------------------------------------------------------------------------------
-- 6) Main entry point
--------------------------------------------------------------------------------

rewriteTypeVars :: Task -> Either String Task
rewriteTypeVars t = do
  -- (A) Rewrite the signature
  newSig <- rewriteOneTypeSignature (T.unpack $ signature t)

  -- (B) Rewrite each dependency
  newDeps <- rewriteDependencies (dependencies t)

  -- (C) Return updated Task (code left as is)
  pure t
    { signature    = T.pack newSig
    , dependencies = newDeps
    , code         = code t
    }
