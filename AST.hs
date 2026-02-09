module AST where

-- Identificadores de Variable
type Variable = String

-- Expresiones Generales
data Exp = EInt IntExp
        | EBool BoolExp
        | EObj ObjExp
 deriving (Show, Eq)        

-- Expresiones Aritmeticas
data IntExp = Const Integer
            | Var Variable
            | UMinus IntExp
            | Plus IntExp IntExp
            | Minus IntExp IntExp
            | Times IntExp IntExp
            | Div IntExp IntExp
            | Question BoolExp IntExp IntExp -- a > 0 ? 1 : 2;
            -- Extensión para objetos
            -- | Obj [(String, IntExp)] -- {edad: 20}
            -- | Str String -- "Juan"
            -- | Access IntExp String  -- para implementar persona.edad
            -- | BoolConst Bool -- admito booleanos para el parser de intexp
 deriving (Show,Eq)

-- Expresiones Booleanas
data BoolExp = BTrue
             | BFalse
             | Eq IntExp IntExp
             | Lt IntExp IntExp
             | Gt IntExp IntExp
             | And BoolExp BoolExp
             | Or BoolExp BoolExp
             | Not BoolExp
 deriving (Show,Eq)

-- Expresiones de Objetos
data ObjExp = Obj [(String, Exp)] -- {edad: 20}
            | Str String -- "Juan"
            | Access Exp String  -- para implementar persona.edad
 deriving (Show,Eq)

-- Comandos (sentencias)
-- Observar que solo se permiten variables de un tipo (entero)
data Comm = Skip
          | Let Variable Exp
          | Seq Comm Comm
          | Cond BoolExp Comm Comm
          | Repeat Comm BoolExp
          | Swap Variable Variable -- Comando para intercambiar valores entre dos variables del estado
 deriving (Show,Eq)
