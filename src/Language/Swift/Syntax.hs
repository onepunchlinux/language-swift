{-# LANGUAGE CPP, DeriveDataTypeable, DeriveGeneric #-}

module Language.Swift.Syntax where

import Data.Data
import GHC.Generics (Generic)
import Data.List.NonEmpty

#define DERIVE deriving (Eq,Ord,Show,Typeable,Generic,Data)

-----------------------------------------------------------------------
-- Statements

data Stmt
    = ExpStmt Exp
    | DeclStmt Decl
    | LoopStmt Loop
    | IfStmt If
    | GuardStmt ConditionClause CodeBlock
    | SwitchStmt Switch
    | LabeledStmt Ident LabelAbleStmt
    | Break (Maybe Ident)
    | Continue (Maybe Ident)
    | FallThrough
    | Return (Maybe Ident)
    | Throw Exp
    | Defer CodeBlock
    | Do CodeBlock [CatchClause]
    | BuildConfigStmt BuildConfig [Stmt] [BuildConfigElseIfClause] (Maybe BuildConfigElseClause)
    | LineCtrl
    | LineCtrlWithInfo Int String
      DERIVE

data Loop = ForStmt (Maybe ForInit) (Maybe Exp) (Maybe Exp) CodeBlock
          | ForInStmt Bool Pattern Exp (Maybe WhereClause) CodeBlock
          | WhileStmt ConditionClause CodeBlock
          | RepeatStmt CodeBlock Exp
            DERIVE

data ForInit = ForInitVarDecl VarDecl
             | ForInitExpList (NonEmpty Exp)
               DERIVE

data WhereClause = WhereClause Exp
                   DERIVE

data ConditionClause = JustExp Exp
                     | ExpWithConList Exp (NonEmpty Condition)
                     | JustConList (NonEmpty Condition)
                     | AvailConWithExp AvailabilityCon Exp
                       DERIVE

data Condition = AvailCon AvailabilityCon
               | CaseCon Pattern Initializer WhereClause
               | OptBindingCon OptBindingHead [OptBindingContinuation]
                 DERIVE

data OptBindingHead = LetBindingHead Pattern Initializer
                    | VarBindingHead Pattern Initializer
                      DERIVE

data OptBindingContinuation = PatternCont Pattern Initializer
                            | BindingHeadCont OptBindingHead
                              DERIVE

data AvailabilityCon = AvailableilityCon (NonEmpty AvailabilityArg)
                       DERIVE

data AvailabilityArg = PlatformArg PlatformName PlatformVersion
                     | PlatformArgStar
                       DERIVE

data PlatformName = IOSPName
                  | IOSApplicationExtPName
                  | OSXPName
                  | OSXApplicationExtPName
                  | WatchOSPName
                    DERIVE

data PlatformVersion = OneDigits String
                     | TwoDigits String
                     | ThreeDigits String
                       DERIVE

data If = If ConditionClause CodeBlock ElseClause
          DERIVE

data ElseClause = CodeElse CodeBlock
                | IfElse If
                  DERIVE

data Switch = Switch Exp [SwitchCase]
              DERIVE

data SwitchCase = CaseLabel (NonEmpty (Pattern, Maybe WhereClause)) (NonEmpty Stmt)
                | DefaultLabel (NonEmpty Stmt)
                  DERIVE

data LabelAbleStmt = LabelLoop Loop
                    | LabelIf If
                    | LabelSwitch Switch
                      DERIVE

data CatchClause = Catch (Maybe Pattern) (Maybe WhereClause) CodeBlock
                   DERIVE

data BuildConfigElseIfClause = BuildConfigElseIf BuildConfig [Stmt]
                               DERIVE

data BuildConfigElseClause = BuildConfigElse [Stmt]
                             DERIVE

data BuildConfig = OSPlatformTestingFunc OS
                 | ArchPlatformTestingFunc Arch
                 | BuildConfigIdent Ident
                 | BoolConfig Bool
                 | ParenBuildConfig BuildConfig
                 | ExclamBuildConfig BuildConfig
                 | AndBuildConfig BuildConfig BuildConfig
                 | OrBuildConfig BuildConfig BuildConfig
                   DERIVE

data OS = OSX
        | IOS
        | WatchOS
        | TvOS
          DERIVE

data Arch = I386
          | X86_64
          | ARM
          | ARM64
            DERIVE

-----------------------------------------------------------------------
-- Expressions

data Exp = Exp (Maybe TryOp) PrefixExp (Maybe BinaryExp)
           DERIVE

data PrefixExp = WrappedPostFix (Maybe Op) PostfixExp
               | InOutExp Ident
                 DERIVE

data TryOp = Try
           | TryAsk
           | TryBang
             DERIVE

data BinaryExp = BinWrapped PrefixExp
               | AssignOp (Maybe TryOp) PrefixExp
               | CondOp (Maybe TryOp) Exp (Maybe TryOp) PrefixExp
               | Is Type
               | As Type
               | AsAsk Type
               | AsBang Type
                 DERIVE

data PrimaryExp
    = IdentExp Ident (Maybe GenericArgClause)
    | PrimLitExp LitExp
    | PrimSelfExp SelfExp
    | PrimSuperClassExp SuperClassExp
    | PrimClosureExp ClosureExp
    | PrimParenExp ParenExp
    | PrimImplicitMemberExp Ident
    | PrimWildCardExp
      DERIVE

data LitExp = LitExp Literal
            | ArrayLitExp [Exp]
            | DictLitExp [(Exp,Exp)]
              DERIVE

data SelfExp = PlainSelf
             | IdentSelf Ident
             | ExpSelf (NonEmpty Exp)
             | InitSelf
               DERIVE

data SuperClassExp = IdentSuper Ident
                   | ExpSuper (NonEmpty Exp)
                   | InitSuper
                     DERIVE

data ClosureExp = ClosureExp (Maybe ClosureSig) (NonEmpty Stmt)
                  DERIVE

data ClosureSig = ParamClauseSig (Maybe CaptureList) ParamClause (Maybe FuncResult)
                | IdentListSig (Maybe CaptureList) (NonEmpty Ident) (Maybe FuncResult)
                | OnlyIdentListSig CaptureList
                  DERIVE

type CaptureList = (NonEmpty CaptureListItem)

data CaptureListItem = CaptureListItem (Maybe CaptureSpecifier) Exp
                       DERIVE

data CaptureSpecifier = WeekSpec
                      | UnownedSpec
                      | UnownedSafeSpec
                      | UnownedUnsafeSpec
                        DERIVE

data ParenExp = ParenExp [ExpElem]
                DERIVE

data ExpElem = PlainElemExp Exp
             | IdentElemExp (Ident, Exp)
               DERIVE


data PostfixExp = PostPrimaryExp PrimaryExp (Maybe Op)
                | PostFunExp PostfixExp (Maybe ParenExp) (Maybe ClosureExp) (Maybe Op)
                | PostInitExp PostfixExp (Maybe Op)
                | PostMemberDigitExp PostfixExp Ident (Maybe Op)
                | PostMemberIdentExp PostfixExp Ident (Maybe GenericArgClause) (Maybe Op)
                | PostSelfExp PostfixExp (Maybe Op)
                | PostDynamicTypeExp PostfixExp (Maybe Op)
                | PostSubscriptExp PostfixExp (NonEmpty Exp) (Maybe Op)
                | PostForcedExp PostfixExp (Maybe Op)
                | PostOptExp PostfixExp (Maybe Op)
                  DERIVE


-----------------------------------------------------------------------
-- Declarations

data Decl
    = ImportDecl [Attribute] (Maybe ImportKind) PathIdent
    | ConstantDecl [Attribute] [DeclModifier] (NonEmpty PatternInitializer)
    | VDecl VarDecl
    | TypeAliasDecl TypeAliasDeclHead Type
    | FuncDecl FuncDeclHead Ident (Maybe GenericParamClause) FuncSig CodeBlock
    | EnumDecl [Attribute] (Maybe AccessLevelModifier) EnumStyle
    | StructDecl [Attribute] (Maybe AccessLevelModifier) Ident (Maybe GenericParamClause) (Maybe TypeInheritanceClause) [Decl]
    | ClassDecl [Attribute] (Maybe AccessLevelModifier) Ident (Maybe GenericParamClause) (Maybe TypeInheritanceClause) [Decl]
    | ProtocolDecl [Attribute] (Maybe AccessLevelModifier) Ident (Maybe TypeInheritanceClause) [ProtocolMemberDecl]
    | InitDecl InitDeclHead (Maybe GenericParamClause) ParamClause ThrowMod CodeBlock
    | DeinitDecl [Attribute] CodeBlock
    | ExtDecl (Maybe AccessLevelModifier) TypeIdent (Maybe TypeInheritanceClause) [Decl]
    | SubscriptDecl SubscriptDeclHead SubscriptResult SubscriptBlock
    | PrefixOpDecl Op
    | PostfixOpDecl Op
    | InfixOpDecl Op (Maybe PrecedenceClause) (Maybe AssocClause)
      DERIVE


-- Import

data ImportKind
    = TypealiasKind
    | StructKind
    | ClassKind
    | EnumKind
    | ProtocolKind
    | VarKind
    | FuncKind
      DERIVE


-- Constant

data PatternInitializer = PatternInitializer Pattern (Maybe Initializer)
                          DERIVE

data Initializer = Initializer Exp
                   DERIVE


-- Variable

data VarDecl = VarDecl VarDeclHead VarDeclType
               DERIVE

data VarDeclHead = VarDeclHead [Attribute] [DeclModifier]
                   DERIVE

data VarDeclType = BasicVarDecl (NonEmpty PatternInitializer)
                 | BlockVarDecl Ident TypeAnnotation CodeBlock
                 | GetterSetterVarDecl Ident TypeAnnotation GetterSetterBlock
                 | GetterSetterKeywordVarDecl Ident TypeAnnotation GetterSetterKeywordBlock
                 | WillDidVarDecl Ident Exp WillSetDidSetBlock
                 | WillDidWithTypeVarDecl Ident TypeAnnotation (Maybe Exp) WillSetDidSetBlock
                   DERIVE

data CodeBlock = CodeBlock [Stmt]
                 DERIVE

data GetterSetterBlock = SimpleGSB CodeBlock
                       | OptGSB GetterClause (Maybe SetterClause)
                       | SGB SetterClause GetterClause
                         DERIVE

data GetterSetterKeywordBlock = GSKB GetterKeyWordClause (Maybe SetterKeyWordClause)
                              | SGKB SetterKeyWordClause GetterKeyWordClause
                                DERIVE

data GetterKeyWordClause = GKWS [Attribute]
                           DERIVE
data SetterKeyWordClause = SKWS [Attribute]
                           DERIVE

data GetterClause = GetterCls [Attribute] CodeBlock
                    DERIVE
data SetterClause = SetterCls [Attribute] (Maybe Ident) CodeBlock
                    DERIVE

data WillSetDidSetBlock = WDSB WillSetClause (Maybe DidSetClause)
                     | DWSB DidSetClause (Maybe WillSetClause)
                       DERIVE

data WillSetClause = WillSetCls [Attribute] (Maybe Ident) CodeBlock
                     DERIVE
data DidSetClause = DidSetCls [Attribute] (Maybe Ident) CodeBlock
                    DERIVE


-- TypeAlias

data TypeAliasDeclHead = TypeAliasDeclHead [Attribute] (Maybe AccessLevelModifier) Ident
                         DERIVE


-- Function

data FuncDeclHead = FuncDeclHead [Attribute] [DeclModifier]
                    DERIVE

data FuncSig = FuncSig [ParamClause] ThrowMod FuncResult
               DERIVE

data ThrowMod = ThrowsMod
              | ReThrowsMod
              | NoThrowMod
                DERIVE

data FuncResult = FuncResult [Attribute] Type
                  DERIVE


data ParamClause = ParamClause [Param] IdentOrOp
                   DERIVE

data Param = BasicParam ParamModifier (Maybe IdentOrOp) IdentOrOp TypeAnnotation (Maybe DefaultArgClause)
           | InOutParam (Maybe IdentOrOp) IdentOrOp TypeAnnotation
             DERIVE

data ParamModifier = LetMod
                   | VarMod
                   | NoParamMod
                     DERIVE

data DefaultArgClause = DefaultArgClause Exp
                        DERIVE


-- Enum

data EnumStyle = UnionStyleEnum Bool Ident (Maybe GenericParamClause) (Maybe TypeInheritanceClause) [UnionStyleMember]
               | RawStyleEnum Ident (Maybe GenericParamClause) TypeInheritanceClause (NonEmpty RawStyleMember)
                 DERIVE

data UnionStyleMember = USMember Decl
                      | UnionCaseClause [Attribute] Bool (NonEmpty UnionCase)
                        DERIVE

data UnionCase = UnionCase Ident (Maybe TupleType)
                 DERIVE

data RawStyleMember = RawMember Decl
                    | RawCaseClause [Attribute] (NonEmpty RawCase)
                      DERIVE

data RawCase = RawCase Ident (Maybe RawValueAssignment)
               DERIVE

data RawValueAssignment = RawValueAssignment RawValueLiteral
                          DERIVE


-- Struct

data ProtocolMemberDecl
    = ProtoPropertyDecl VarDeclHead Ident TypeAnnotation GetterSetterKeywordBlock
    | ProtoMethodDecl FuncDeclHead Ident (Maybe GenericParamClause) FuncSig
    | ProtoInitializerDecl InitDeclHead (Maybe GenericParamClause) ParamClause ThrowMod
    | ProtoSubscriptDecl SubscriptDeclHead SubscriptResult GetterSetterKeywordBlock
    | ProtoAssocTypeDecl TypeAliasDeclHead (Maybe TypeInheritanceClause) (Maybe Type)
      DERIVE


-- Initializer

data InitDeclHead = InitDeclHead [Attribute] [DeclModifier] InitMod
                    DERIVE

data InitMod = InitPlain | InitAsk | InitBang
               DERIVE


-- Subscript

data SubscriptDeclHead = SubscriptDeclHead [Attribute] [DeclModifier] ParamClause
                         DERIVE

data SubscriptResult = SubscriptResult [Attribute] Type
                       DERIVE

data SubscriptBlock = SubGSB GetterSetterBlock
                    | SubGSKB GetterSetterKeywordBlock
                    | SubSimple CodeBlock
                      DERIVE


-- Operator

data PrecedenceClause = PrecedenceClause Int
                        DERIVE

data AssocClause = AssocLeft
                 | AssocRight
                 | AssocNone
                   DERIVE

-----------------------------------------------------------------------
-- Modifiers

-- Declaration

data DeclModifier
    = Class
    | Convenience
    | Dynamic
    | Final
    | Infix
    | Lazy
    | Mutating
    | Nonmutating
    | Optional
    | Override
    | Postfix
    | Prefix
    | Required
    | Static
    | Unowned
    | UnownedSafe
    | UnownedUnsafe
    | Weak
    | AccessLevelModifier
      DERIVE

-- Access Control Level

data AccessLevelModifier
    = Internal
    | InternalSet
    | Private
    | PrivateSet
    | Public
    | PublicSet
      DERIVE


-----------------------------------------------------------------------
-- Attributes

data Attribute = Attr Ident [BalancedTokens]
                 DERIVE

data BalancedTokens = BTokens String
                      DERIVE


-----------------------------------------------------------------------
-- Generics

data GenericArgClause = GenericArgClause (NonEmpty Type)
                        DERIVE

data GenericParamClause = GenericParamClause (NonEmpty GenericParam) (Maybe RequirementClause)
                          DERIVE

data GenericParam = TypeName Ident
                  | TypeNameWithTypeIdent Ident TypeIdent
                  | TypeNameWithProtoComp Ident ProtoCompType
                    DERIVE

data RequirementClause = RequirementClause (NonEmpty Requirement)
                         DERIVE

data Requirement = ConformanceReqIdent TypeIdent TypeIdent
                 | ConformanceReqProto TypeIdent ProtoCompType
                 | SameTypeReq Ident Type
                   DERIVE


-----------------------------------------------------------------------
-- Types

data TypeAnnotation = TypeAnnotation [Attribute] Type
                      DERIVE

data Type = ArrayType Type
          | DictType Type Type
          | FuncType Type ThrowMod Type
          | TypeIdentifier TypeIdent
          | TupleType TupleType
          | OptType Type
          | ImplicitlyUnwrappedOptType Type
          | ProtoCompType ProtoCompType
          | MetaType Type
          | MetaProto Type
            DERIVE

type TypeIdent = (NonEmpty (Ident, Maybe GenericArgClause))

data ProtoCompType = Proto [TypeIdent]
                     DERIVE

data TypeInheritanceClause = ClassReqWithTypeList (NonEmpty TypeIdent)
                           | ClassReq
                           | TypeList (NonEmpty TypeIdent)
                             DERIVE

data TupleType = TplTyp [TupleTypeElem]
                 DERIVE

data TupleTypeElem = ElemWithAttrs [Attribute] Bool Type
                   | ElemWithName Bool Ident TypeAnnotation
                     DERIVE


-----------------------------------------------------------------------
-- Patterns

data Pattern = WildCardPattern (Maybe TypeAnnotation)
             | IdentPattern Ident (Maybe TypeAnnotation)
             | ValueBindVar Pattern
             | ValueBindLet Pattern
             | TuplePattern TuplePattern
             | EnumCasePattern (Maybe TypeIdent) Ident (Maybe TuplePattern)
             | OptionalPattern Ident
             | IsPattern Type
             | AsPattern Pattern Type
             | ExpPattern Exp
               DERIVE

data TuplePattern = TuplePtrn [Pattern] (Maybe TypeAnnotation)
                    DERIVE

-----------------------------------------------------------------------
-- Names and identifiers

-- | A single identifier.
data Ident = Ident String
             DERIVE

data IdentOrOp = I Ident | O Op
                 DERIVE

-- | A name, i.e. a period-separated list of identifiers.
data PathIdent = PathIdent (NonEmpty Ident)
                 DERIVE

-----------------------------------------------------------------------
-- Literals

data Literal = Int Sign Int
             | Float Sign Float
             | String String
             | Boolean Bool
             | NilLit
               DERIVE

data RawValueLiteral = RawInt Sign Int
                     | RawFloat Sign Float
                     | RawString String
                     | RawBool Bool
                       DERIVE

data Sign = Neg | Pos
            DERIVE


-----------------------------------------------------------------------
-- Operators

data Op = Op String
          DERIVE
