module Language.Swift.Pretty where

import Text.PrettyPrint

import Language.Swift.Syntax

prettyPrint :: Pretty a => a -> String
prettyPrint = show . pretty

parenPrec :: Int -> Int -> Doc -> Doc
parenPrec inheritedPrec currentPrec t
    | inheritedPrec <= 0          = t
    | inheritedPrec < currentPrec = parens t
    | otherwise                   = t

class Pretty a where
    pretty :: a -> Doc
    pretty = prettyPrec 0

    prettyPrec :: Int -> a -> Doc
    prettyPrec _ = pretty
