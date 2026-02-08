module EvalObj (eval) where

import AST

-- Defino nuevo tipo de dato Value (Enteros, Booleanos, String y Objetos)
data Value = VInt Integer
            | VBool Bool
            | VStr String
            | VObj [(String, Value)]
            deriving (Show, Eq)

-- Defino Estados para Objetos
type State = [(Variable, Value)]

-- Defino Estado nulo
initState :: State
initState = []

-- Busca el valor de una variable en el objeto
lookfor :: Variable -> State -> Value
lookfor var ((x,y):xs)= if var == x then y
                                    else lookfor var xs
--Caso base
lookfor var [] = error ("Variable: " ++ var ++ " no definida en el estado")

-- Busco un campo en el objeto
findField :: String -> [(String, Value)] -> Value
--caso base
findField campo [] = error("Campo inexistente: " ++ campo)

findField campo ((nombre,val):xs) = if campo == nombre
                                        then val
                                        else findField campo xs

-- Cambiar el valor de una variable en un estado
update :: Variable -> Value -> State -> State
update var val [] = [(var, val)]
update var val ((x,y):xs) = if var == x then (var,val):xs
                                        else (x,y): update var val xs

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = evalComm p initState

-- Evalua un comando en un estado dado
-- Completar definicion
evalComm :: Comm -> State -> State
evalComm Skip s = s

-- Declaramos un valor en el estado
evalComm (Let var exp) s = let val = evalIntExp exp s
                           in evalComm Skip (update var val s)

evalComm (Seq Skip c1) s = evalComm c1 s
evalComm (Seq c0 c1) s = let s' = evalComm c0 s
                                  in evalComm (Seq Skip c1) s'
                                  -- in  evalComm c1 s'

evalComm (Cond b c0 c1) s = if (evalBoolExp b s )
                            then evalComm c0 s
                            else evalComm c1 s

evalComm (Repeat c b) s = evalComm (Seq c (Cond b Skip (Repeat c b))) s
-- evalComm (Repeat c b) s = 
--                             let s' = evalComm c s
--                             in if evalBoolExp b s'
--                                 then s'
--                                 else evalComm (Repeat c b) s'


-- Evalua una expresion entera
-- Completar definicion
evalIntExp :: IntExp -> State -> Value
evalIntExp (Const val) estado = VInt val -- Constante entera en Value

evalIntExp (Var var) estado = lookfor var estado -- Value de una variable en el estado

-- evalIntExp (UMinus expInt) estado = let val = evalIntExp expInt estado
--                                     in (-val) 
--Negación u opuesto de un entero con errores
evalIntExp (UMinus exp) estado = case evalIntExp exp estado of
                                    VInt val -> VInt(-val)
                                    _ -> error "UMinus se aplico a un valor no entero"


-- evalIntExp (Plus exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
--                                          valor2 = evalIntExp exp2 estado
--                                          in valor1 + valor2
-- Suma con manejo de errores
evalIntExp (Plus exp1 exp2) estado = case (evalIntExp exp1 estado, evalIntExp exp2 estado) of
                                        (VInt n1, VInt n2) -> VInt (n1+n2)
                                        _ -> error "Operacion suma sobre valores no entero"

-- evalIntExp (Minus exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
--                                           valor2 = evalIntExp exp2 estado
--                                           in valor1 - valor2
-- Resta con manejo de errores
evalIntExp (Minus exp1 exp2) estado = case (evalIntExp exp1 estado, evalIntExp exp2 estado) of
                                        (VInt n1, VInt n2) -> VInt (n1-n2)
                                        _ -> error "Operacion resta sobre valores no entero"

-- evalIntExp (Times exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
--                                           valor2 = evalIntExp exp2 estado
--                                           in valor1 * valor2
--Multiplicacion con manejo de errores
evalIntExp (Times exp1 exp2) estado = case (evalIntExp exp1 estado, evalIntExp exp2 estado) of
                                        (VInt n1, VInt n2) -> VInt (n1*n2)
                                        _ -> error "Operacion multiplicacion sobre valores no entero"

-- evalIntExp (Div exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
--                                         valor2 = evalIntExp exp2 estado
--                                         in div valor1 valor2
--Division con manejo de errores
evalIntExp (Div exp1 exp2) estado = case (evalIntExp exp1 estado, evalIntExp exp2 estado) of
                                        (VInt n1, VInt n2) -> VInt (div n1 n2)
                                        _ -> error "Operacion division sobre valores no entero"

--Valor del String
evalIntExp (Str s) estado = VStr s

--Valor del operador ternario
evalIntExp (Question b exp1 exp2) estado = if evalBoolExp b estado 
                                            then evalIntExp exp1 estado
                                            else evalIntExp exp2 estado

--Valor/es del objeto -> retorna lista de pares clave valor del objeto
evalIntExp (Obj campos) estado = VObj [(nombre, evalIntExp e estado) | (nombre, e) <- campos]


--Acceso al valor del campo del objeto para azucar sintactica "."
evalIntExp (Access exp campo) estado = case evalIntExp exp estado of  --evaluo el objeto
                                        VObj campos -> findField campo campos --busco el campo
                                        _ -> error "Acceso a campo sobre un valor que no es Objeto"

evalIntExp (BoolConst a) estado = VBool a
                                            
-- Evalua una expresion entera
-- Completar definicion
evalBoolExp :: BoolExp -> State -> Bool
evalBoolExp BTrue estado = True
evalBoolExp BFalse estado = False
evalBoolExp (Eq exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                        valor2 = evalIntExp exp2 estado
                                        in valor1 == valor2

-- evalBoolExp (Lt exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
--                                         valor2 = evalIntExp exp2 estado
--                                         in valor1 < valor2
evalBoolExp (Lt exp1 exp2) estado = case (evalIntExp exp1 estado, evalIntExp exp2 estado) of
                                        (VInt n1, VInt n2) -> n1 < n2
                                        _ -> error "Comparacion < solo se permite entre enteros"

-- evalBoolExp (Gt exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
--                                         valor2 = evalIntExp exp2 estado
--                                         in valor1 > valor2
evalBoolExp (Gt exp1 exp2) estado = case (evalIntExp exp1 estado, evalIntExp exp2 estado) of
                                        (VInt n1, VInt n2) -> n1 > n2
                                        _ -> error "Comparacion > solo se permite entre enteros"

evalBoolExp (And exp1 exp2) estado = let valor1 = evalBoolExp exp1 estado
                                         valor2 = evalBoolExp exp2 estado
                                        in valor1 && valor2

evalBoolExp (Or exp1 exp2) estado = let valor1 = evalBoolExp exp1 estado
                                        valor2 = evalBoolExp exp2 estado
                                        in valor1 || valor2
                                        
evalBoolExp (Not exp1) estado = not (evalBoolExp exp1 estado)



