{
{-# LANGUAGE BangPatterns #-}
module Language.Swift.Lexer (L(..), Token(..), lexer) where

import Numeric
import Data.Char

import Debug.Trace (trace)
}

%wrapper "posn"


$digit      = [0-9]
$nonzero    = [1-9]
$octdig     = [0-7]
$hexdig     = [0-9A-Fa-f]

@lineterm = [\n\r] | \r\n

-- TODO: this doesn't notice a comment that ends "**/"
@tradcomm = "/*" ( ~[\*] | \*+ (~[\/\*] | \n) | \n )* \*+ "/"
@linecomm = "//" .* @lineterm
@comm = @tradcomm | @linecomm

tokens  :-

    $white+         ;
    @comm           ;

    class              { \p _ -> L (pos p) $ KW_Class         }
    deinit             { \p _ -> L (pos p) $ KW_Deinit        }
    enum               { \p _ -> L (pos p) $ KW_Enum          }
    extension          { \p _ -> L (pos p) $ KW_Extension     }
    func               { \p _ -> L (pos p) $ KW_Func          }
    import             { \p _ -> L (pos p) $ KW_Import        }
    init               { \p _ -> L (pos p) $ KW_Init          }
    inout              { \p _ -> L (pos p) $ KW_Inout         }
    internal           { \p _ -> L (pos p) $ KW_Internal      }
    let                { \p _ -> L (pos p) $ KW_Let           }
    operator           { \p _ -> L (pos p) $ KW_Operator      }
    private            { \p _ -> L (pos p) $ KW_Private       }
    protocol           { \p _ -> L (pos p) $ KW_Protocol      }
    public             { \p _ -> L (pos p) $ KW_Public        }
    static             { \p _ -> L (pos p) $ KW_Static        }
    struct             { \p _ -> L (pos p) $ KW_Struct        }
    subscript          { \p _ -> L (pos p) $ KW_Subscript     }
    typealias          { \p _ -> L (pos p) $ KW_Typealias     }
    var                { \p _ -> L (pos p) $ KW_Var           }
    break              { \p _ -> L (pos p) $ KW_Break         }
    case               { \p _ -> L (pos p) $ KW_Case          }
    continue           { \p _ -> L (pos p) $ KW_Continue      }
    default            { \p _ -> L (pos p) $ KW_Default       }
    defer              { \p _ -> L (pos p) $ KW_Defer         }
    do                 { \p _ -> L (pos p) $ KW_Do            }
    else               { \p _ -> L (pos p) $ KW_Else          }
    fallthrough        { \p _ -> L (pos p) $ KW_Fallthrough   }
    for                { \p _ -> L (pos p) $ KW_For           }
    guard              { \p _ -> L (pos p) $ KW_Guard         }
    if                 { \p _ -> L (pos p) $ KW_If            }
    in                 { \p _ -> L (pos p) $ KW_In            }
    repeat             { \p _ -> L (pos p) $ KW_Repeat        }
    return             { \p _ -> L (pos p) $ KW_Return        }
    switch             { \p _ -> L (pos p) $ KW_Switch        }
    where              { \p _ -> L (pos p) $ KW_Where         }
    while              { \p _ -> L (pos p) $ KW_While         }
    as                 { \p _ -> L (pos p) $ KW_As            }
    catch              { \p _ -> L (pos p) $ KW_Catch         }
    dynamicType        { \p _ -> L (pos p) $ KW_DynamicType   }
    is                 { \p _ -> L (pos p) $ KW_Is            }
    rethrows           { \p _ -> L (pos p) $ KW_Rethrows      }
    super              { \p _ -> L (pos p) $ KW_Super         }
    self               { \p _ -> L (pos p) $ KW_Self          }
    Self               { \p _ -> L (pos p) $ KW_self          }
    throw              { \p _ -> L (pos p) $ KW_Throw         }
    throws             { \p _ -> L (pos p) $ KW_Throws        }
    try                { \p _ -> L (pos p) $ KW_Try           }
    __COLUMN__         { \p _ -> L (pos p) $ KW_COLUMN        }
    __FILE__           { \p _ -> L (pos p) $ KW_FILE          }
    __FUNCTION__       { \p _ -> L (pos p) $ KW_FUNCTION      }
    __LINE__           { \p _ -> L (pos p) $ KW_LINE          }
    _                  { \p _ -> L (pos p) $ KW_Underscore    }
    associativity      { \p _ -> L (pos p) $ KW_Associativity }
    convenience        { \p _ -> L (pos p) $ KW_Convenience   }
    dynamic            { \p _ -> L (pos p) $ KW_Dynamic       }
    didSet             { \p _ -> L (pos p) $ KW_DidSet        }
    final              { \p _ -> L (pos p) $ KW_Final         }
    get                { \p _ -> L (pos p) $ KW_Get           }
    infix              { \p _ -> L (pos p) $ KW_Infix         }
    indirect           { \p _ -> L (pos p) $ KW_Indirect      }
    lazy               { \p _ -> L (pos p) $ KW_Lazy          }
    left               { \p _ -> L (pos p) $ KW_Left          }
    mutating           { \p _ -> L (pos p) $ KW_Mutating      }
    none               { \p _ -> L (pos p) $ KW_None          }
    nonmtating         { \p _ -> L (pos p) $ KW_Nonmtating    }
    optional           { \p _ -> L (pos p) $ KW_Optional      }
    override           { \p _ -> L (pos p) $ KW_Override      }
    postfix            { \p _ -> L (pos p) $ KW_Postfix       }
    precedence         { \p _ -> L (pos p) $ KW_Precedence    }
    prefix             { \p _ -> L (pos p) $ KW_Prefix        }
    Protocol           { \p _ -> L (pos p) $ KW_protocol      }
    required           { \p _ -> L (pos p) $ KW_Required      }
    right              { \p _ -> L (pos p) $ KW_Right         }
    set                { \p _ -> L (pos p) $ KW_Set           }
    Type               { \p _ -> L (pos p) $ KW_type          }
    unowned            { \p _ -> L (pos p) $ KW_Unowned       }
    weak               { \p _ -> L (pos p) $ KW_Weak          }
    willSet            { \p _ -> L (pos p) $ KW_WillSet       }

    true               { \p _ -> L (pos p) $ BoolTok True          }
    false              { \p _ -> L (pos p) $ BookTok False         }

    nil                { \p _ -> L (pos p) $ NilTok            }

    \(              { \p _ -> L (pos p) $ OpenParen       }
    \)              { \p _ -> L (pos p) $ CloseParen      }
    \[              { \p _ -> L (pos p) $ OpenSquare      }
    \]              { \p _ -> L (pos p) $ CloseSquare     }
    \{              { \p _ -> L (pos p) $ OpenCurly       }
    \}              { \p _ -> L (pos p) $ CloseCurly      }
    \;              { \p _ -> L (pos p) $ SemiColon       }
    \,              { \p _ -> L (pos p) $ Comma           }
    \.              { \p _ -> L (pos p) $ Period          }


{
data L a = L Pos a
  deriving (Show, Eq)

type Pos (Int, Int)

pos :: AlexPosn -> Pos
pos (AlexPn _ l c) = (l,c)

data Token
    --Keywords

    KW_Class
    KW_Deinit
    KW_Enum
    KW_Extension
    KW_Func
    KW_Import
    KW_Init
    KW_Inout
    KW_Internal
    KW_Let
    KW_Operator
    KW_Private
    KW_Protocol
    KW_Public
    KW_Static
    KW_Struct
    KW_Subscript
    KW_Typealias
    KW_Var
    KW_Break
    KW_Case
    KW_Continue
    KW_Default
    KW_Defer
    KW_Do
    KW_Else
    KW_Fallthrough
    KW_For
    KW_Guard
    KW_If
    KW_In
    KW_Repeat
    KW_Return
    KW_Switch
    KW_Where
    KW_While
    KW_As
    KW_Catch
    KW_DynamicType
    KW_Is
    KW_Nil
    KW_Rethrows
    KW_Super
    KW_Self
    KW_self
    KW_Throw
    KW_Throws
    KW_Try
    KW_COLUMN
    KW_FILE
    KW_FUNCTION
    KW_LINE
    KW_Underscore
    KW_Associativity
    KW_Convenience
    KW_Dynamic
    KW_DidSet
    KW_Final
    KW_Get
    KW_Infix
    KW_Indirect
    KW_Lazy
    KW_Left
    KW_Mutating
    KW_None
    KW_Nonmtating
    KW_Optional
    KW_Override
    KW_Postfix
    KW_Precedence
    KW_Prefix
    KW_protocol
    KW_Required
    KW_Right
    KW_Set
    KW_type
    KW_Unowned
    KW_Weak
    KW_WillSet

    -- Separators
    | OpenParen
    | CloseParen
    | OpenCurly
    | CloseCurly
    | OpenSquare
    | CloseSquare
    | Period
    | Comma
    | SemiColon
    | LambdaArrow

    -- Literals
    | IntTok  Integer
    | FloatTok Float
    | StringTok String
    | BoolTok Bool
    | NilTok

    -- Identifiers
    | IdentTok String

    -- Operators
    | OpTok String

    deriving (Show, Eq)

lexer :: String -> [L Token]
lexer = alexScanTokens

}
