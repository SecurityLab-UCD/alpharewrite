
module DeclParser (MyDecl, parseOneDecl, declToString) where


-- Parsing

import Language.Haskell.Exts
  ( Decl (..),
    Extension (..),
    KnownExtension (..),
    ParseMode (..),
    ParseResult (..),
    SrcSpanInfo,
    defaultParseMode,
    parseDeclWithMode,
    prettyPrint,
  )

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
    ParseOk d -> Right d
    ParseFailed _ err -> Left ("Parse error: " ++ err)
  where
    defaultMode :: ParseMode
    defaultMode =
      defaultParseMode
        { extensions =
            map
              EnableExtension
              [ MultiParamTypeClasses,
                FlexibleContexts,
                FlexibleInstances, 
                TypeFamilies
                -- Further work: support more type extensions
                -- , GADTs
              ]
        }

--------------------------------------------------------------------------------
-- 3) Convert a Decl back to string
--------------------------------------------------------------------------------

declToString :: MyDecl -> String
declToString = prettyPrint

