{-# LANGUAGE OverloadedStrings #-}

module AtomicTypeRewrite
  ( rewriteAtomicTypes
  ) where

import Control.Monad (forM)
import Data.Char (isUpper)
import Data.Generics (everywhere, mkT, listify)
import Data.List (nub)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as Builder
import Language.Haskell.Exts
  ( Decl (..),
    ConDecl (..),
    QualConDecl (..),
    DeclHead (..),
    QName (..),
    Name (..),
    ParseMode (..),
    SrcSpanInfo,
    Type (..),
    prettyPrint
  )
import Task (Task(..))
import DeclParser (MyDecl, parseOneDecl, declToString)

--------------------------------------------------------------------------------
-- 1) Gather all atomic types from a type signature
--------------------------------------------------------------------------------
gatherAtomicTypes :: Type SrcSpanInfo -> [String]
gatherAtomicTypes ty =
  let allAtoms = listify isAtomicType ty
  in nub [qNameToString n | TyCon _ n <- allAtoms]
  where
    isAtomicType :: Type SrcSpanInfo -> Bool
    isAtomicType (TyCon _ n) = case qNameToString n of
      (x:_) -> isUpper x
      _     -> False
    isAtomicType _           = False

    qNameToString :: QName SrcSpanInfo -> String
    qNameToString (UnQual _ (Ident _ s))  = s
    qNameToString (UnQual _ (Symbol _ s)) = s
    qNameToString _                       = ""

--------------------------------------------------------------------------------
-- 2) Rewrite atomic types in a type signature
--------------------------------------------------------------------------------
renameAtomicTypes :: [(String, String)] -> Type SrcSpanInfo -> Type SrcSpanInfo
renameAtomicTypes pairs = everywhere (mkT go)
  where
    go :: Type SrcSpanInfo -> Type SrcSpanInfo
    go t@(TyCon l n) =
      let old = qNameToString n
       in case lookup old pairs of
            Just new -> TyCon l (replaceQName n new)
            Nothing  -> t
    go t = t

    qNameToString :: QName SrcSpanInfo -> String
    qNameToString (UnQual _ (Ident _ s))  = s
    qNameToString (UnQual _ (Symbol _ s)) = s
    qNameToString _                       = ""

    replaceQName :: QName SrcSpanInfo -> String -> QName SrcSpanInfo
    replaceQName (UnQual l (Ident _ _))  new = UnQual l (Ident l new)
    replaceQName (UnQual l (Symbol _ _)) new = UnQual l (Symbol l new)
    replaceQName qn _                     = qn

--------------------------------------------------------------------------------
-- 3) Rewrite algebraic data types and their constructors
--------------------------------------------------------------------------------
rewriteAlgebraicDataType :: [(String, String)] -> Decl SrcSpanInfo -> Decl SrcSpanInfo
rewriteAlgebraicDataType pairs (DataDecl l d ctx declHead cons derivs) =
  let newDeclHead = renameDeclHead pairs declHead
      newCons = map (renameCon pairs) cons
   in DataDecl l d ctx newDeclHead newCons derivs
rewriteAlgebraicDataType _ decl = decl

renameDeclHead :: [(String, String)] -> DeclHead SrcSpanInfo -> DeclHead SrcSpanInfo
renameDeclHead pairs (DHead l name) = DHead l (renameDataName pairs name)
renameDeclHead pairs (DHApp l dh name) = DHApp l (renameDeclHead pairs dh) name
renameDeclHead _ dh = dh

renameDataName :: [(String, String)] -> Name SrcSpanInfo -> Name SrcSpanInfo
renameDataName pairs name =
  case lookup (nameToString name) pairs of
    Just new -> replaceName name new
    Nothing  -> name

renameCon :: [(String, String)] -> QualConDecl SrcSpanInfo -> QualConDecl SrcSpanInfo
renameCon pairs (QualConDecl l tyVars ctx (ConDecl l' name args)) =
  let newName = renameDataName pairs name
      newArgs = map (renameAtomicTypes pairs) args
   in QualConDecl l tyVars ctx (ConDecl l' newName newArgs)
renameCon pairs con = con

nameToString :: Name SrcSpanInfo -> String
nameToString (Ident _ s)  = s
nameToString (Symbol _ s) = s

replaceName :: Name SrcSpanInfo -> String -> Name SrcSpanInfo
replaceName (Ident l _)  new = Ident l new
replaceName (Symbol l _) new = Symbol l new

--------------------------------------------------------------------------------
-- 4) Extract type from declaration
--------------------------------------------------------------------------------
extractTypeFromDecl :: MyDecl -> Maybe (Type SrcSpanInfo)
extractTypeFromDecl (TypeSig _ _ ty) = Just ty
extractTypeFromDecl _                = Nothing

--------------------------------------------------------------------------------
-- 5) Rewrite the task
--------------------------------------------------------------------------------
rewriteAtomicTypes :: Task -> Either String Task
rewriteAtomicTypes t = do
  -- (A) Rewrite the signature
  decl <- parseOneDecl (T.unpack $ signature t)
  let Just ty = extractTypeFromDecl decl
      atomics = gatherAtomicTypes ty
      renameMap = zip atomics ["T" ++ show i | i <- [1 ..]]
      newTy = renameAtomicTypes renameMap ty
      newDecl = case decl of
                  TypeSig l names _ -> TypeSig l names newTy
                  _                 -> decl

  -- (B) Rewrite dependencies
  newDeps <- forM (dependencies t) $ \depLine -> do
    let depStr = T.unpack depLine
    case parseOneDecl depStr of
      Right d@(DataDecl {}) ->
        let rewritten = rewriteAlgebraicDataType renameMap d
         in Right (T.pack $ declToString rewritten)
      Right d -> do
        let Just depTy = extractTypeFromDecl d
            newDepTy = renameAtomicTypes renameMap depTy
            newDepDecl = case d of
                           TypeSig l names _ -> TypeSig l names newDepTy
                           _                 -> d
        Right (T.pack $ declToString newDepDecl)
      Left err -> Left err

  -- (C) Rewrite code, including constructors
  pure t
    { signature    = T.pack $ declToString newDecl
    , dependencies = newDeps
    , code         = rewriteCode renameMap (code t)
    }

--------------------------------------------------------------------------------
-- 6) Rewrite code, ensuring whole word matches
--------------------------------------------------------------------------------
rewriteCode :: [(String, String)] -> T.Text -> T.Text
rewriteCode renameMap code =
  foldr replaceWord code renameMap
  where
    replaceWord (old, new) acc =
      let pattern = TL.toStrict . Builder.toLazyText $ "\\b" <> Builder.fromText (T.pack old) <> "\\b"
      in T.replace pattern (T.pack new) acc
