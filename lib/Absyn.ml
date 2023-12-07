type typ =
| Int
| Short
| Long
| Char
| Float
| Void
| Ptr of typ

and binary_op =
| PLUS
| MINUS
| TIMES
| DIVIDE

and expression =
| E_Null
| E_Long of int64
| E_Int of int
| E_Short of int
| E_Float of float
| E_Char of string
| E_Var of string
| E_Binary of binary_op * expression * expression
| E_Call of string * (expression list)
| E_Addr of string
| E_Deref of string
| E_Index of string * expression
| E_Ternary of expression * expression * expression

and statement =
| S_Declare of typ * string
| S_DeclareArray of typ * string * expression
| S_DeclareAssign of typ * string * expression
| S_Assign of string * expression
| S_ArrayAssign of string * expression * expression
| S_Block of statement list
| S_Call of string * (expression list)
| S_Return of expression
| S_BlindReturn
| S_If of expression * statement * statement
| S_While of expression * statement
| S_DoWhile of statement * expression
| S_Break
| S_Continue

and toplevel =
| T_Function of typ * string * (typ * string) list * statement list
| T_Declare of typ * string
| T_DeclareAssign of typ * string * expression