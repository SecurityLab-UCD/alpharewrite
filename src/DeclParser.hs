
module DeclParser (MyDecl, parseOneDecl, declToString) where


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

type MyDecl = Decl SrcSpanInfo


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


declToString :: MyDecl -> String
declToString = prettyPrint

