module TypeRewrite where

import Task (Task (..))
import DeclParser (MyDecl, parseOneDecl, declToString)
import Language.Haskell.Exts
  ( Decl (..),
    Extension (..),
    KnownExtension (..),
    Name (..),
    Type (..),
    ParseMode (..),
    ParseResult (..),
    SrcSpanInfo,
    defaultParseMode,
    parseDeclWithMode,
    prettyPrint,
  )
