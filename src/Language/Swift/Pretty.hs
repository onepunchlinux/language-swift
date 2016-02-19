module Language.Swift.Pretty where

import Text.PrettyPrint
import qualified Data.List.NonEmpty as NE

import Language.Swift.Syntax


prettyPrint :: Pretty a => a -> String
prettyPrint = show . pretty

class Pretty a where
    pretty :: a -> Doc
    pretty = prettyPrec 0

    prettyPrec :: Int -> a -> Doc
    prettyPrec _ = pretty


-----------------------------------------------------------------------
-- Helpers

maybePP :: Pretty a => Int -> Maybe a -> Doc
maybePP p = maybe empty (prettyPrec p)

braceBlock :: [Doc] -> Doc
braceBlock xs = lbrace $+$ nest 2 (vcat xs) $+$ rbrace


-----------------------------------------------------------------------
-- Statements

instance Pretty Stmt where
    prettyPrec p (ExpStmt exp)   = prettyPrec p exp
    prettyPrec p (DeclStmt decl) = prettyPrec p decl
    prettyPrec p (LoopStmt loop) = prettyPrec p loop
    prettyPrec p (IfStmt ifStmt) = prettyPrec p ifStmt
    prettyPrec p (GuardStmt conditionClause codeBlock) =
        text "guard"
                 <+> prettyPrec p conditionClause
                 <+> text "else"
                 <+> prettyPrec p codeBlock

    prettyPrec p (SwitchStmt switch) = prettyPrec p switch
    prettyPrec p (LabeledStmt ident labelableStmt) =
        prettyPrec p ident <+> text ":" <+> prettyPrec p labelableStmt

    prettyPrec p (Break optIdent) = text "break" <+> maybePP p optIdent
    prettyPrec p (Continue optIdent) = text "continue" <+> maybePP p optIdent
    prettyPrec _ FallThrough = text "fallthrough"
    prettyPrec p (Return optExp) = text "return" <+> maybePP p optExp

    prettyPrec p (Throw exp) = text "throw" <+> prettyPrec p exp
    prettyPrec p (Defer codeBlock) = text "defer" <+> prettyPrec p codeBlock
    prettyPrec p (Do codeBlock catchClauses) =
                               vcat $ text "do" : prettyPrec p codeBlock : map (prettyPrec p) catchClauses

    prettyPrec p (BuildConfigStmt buildConfig stmts buildConfigElseIfClauses optBuildConfigElseClause) =
        vcat [ text "#if" <+> prettyPrec p buildConfig
             , vcat (map (prettyPrec p) stmts)
             , vcat (map (prettyPrec p) buildConfigElseIfClauses)
             , maybePP p optBuildConfigElseClause
             , text "#endif"
             ]

    prettyPrec p (LineCtrl Nothing) = text "#line"
    prettyPrec p (LineCtrl (Just (lineNumber, fileName))) = hsep [text "#line", int lineNumber, text fileName]

instance Pretty Loop where
    prettyPrec p (ForStmt optForInit optCond optInc codeBlock) =
        hsep [ text "for"
             , maybePP p optForInit
             , text ";"
             , maybePP p optCond
             , text ";"
             , maybePP p optInc
             ] $+$ prettyPrec p codeBlock

    prettyPrec p (ForInStmt hasCase pattern exp optWhereClause codeBlock) =
        hsep [ text "for"
             , if hasCase then text "case" else empty
             , prettyPrec p pattern
             , prettyPrec p exp
             , maybePP p optWhereClause
             ] $+$ prettyPrec p codeBlock

    prettyPrec p (WhileStmt condClause codeBlock) =
        text "while" <+> prettyPrec p condClause $+$ prettyPrec p codeBlock

    prettyPrec p (RepeatStmt codeBlock exp) =
        vcat [ text "repeat"
             , prettyPrec p codeBlock
             , text "while" <+> prettyPrec p exp
             ]

instance Pretty ForInit where
    prettyPrec p (ForInitVarDecl varDecl) = prettyPrec p varDecl
    prettyPrec p (ForInitExpList expList) =
        hsep $ NE.toList (NE.intersperse (text ",") (NE.map (prettyPrec p) expList))

instance Pretty WhereClause where
    prettyPrec p (WhereClause exp) = prettyPrec p exp


instance Pretty ConditionClause where

instance Pretty CatchClause where
    prettyPrec p (Catch optPattern optWhereClause codeBlock) =
        hsep [ text "catch"
             , maybePP p optPattern
             , maybePP p optWhereClause]
        $+$ prettyPrec p codeBlock

instance Pretty CodeBlock where
    prettyPrec p (CodeBlock stmts) = braceBlock (map (prettyPrec p) stmts)



-----------------------------------------------------------------------
-- Expressions


-----------------------------------------------------------------------
-- Declarations
