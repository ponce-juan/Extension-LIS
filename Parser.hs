module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import AST

-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
                  whiteSpace lis
                  t <- p
                  eof
                  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser (emptyDef   { commentStart  = "/*"
                                  , commentEnd    = "*/"
                                  , commentLine   = "//"
                                  , reservedNames = ["true","false","skip","if",
                                                     "then","else","end",
                                                     "while","do", "repeat", "swap"]
                                  , reservedOpNames = [  "+"
                                                       , "-"
                                                       , "*"
                                                       , "/"
                                                       , "<"
                                                       , ">"
                                                       , "&"
                                                       , "|"
                                                       , "="
                                                       , ";"
                                                       , "~"
                                                       , ":="
                                                       , "." -- Azucar sintactica para acceder a los campos de objetos
                                                       , ":" -- Token asignacion valores a objeto
                                                       , "{" --Token apertura de objeto
                                                       , "}" -- Token cierre de objeto
                                                       , "," -- Token separador de campos de objetos
                                                       ]
                                   }
                                 )
-- Parser para expresiones generales
generalExp :: Parser Exp
generalExp = try (do o <- objAccess
                     return (EObj o))
        <|> try (do s <- stringLiteral lis
                    return (EObj (Str s)))
        <|> try (do i <- intexp
                    return (EInt i))
        <|> try (do b <- boolexp
                    return (EBool b))

----------------------------------
--- Parser de expressiones enteras
-----------------------------------
{--
chainl p op x
parsea 0 o más ocurrencias de p separadas por op
Retorna el valor que se obtiene al aplicar todas las
funciones retornadas por op a los valores retornados
por p

t := t + t | m
term = chainl factor (do {symbol "+"; return (+)})
factor = integer <|> parens term
--}

intexp :: Parser IntExp
intexp  = chainl1 term addopp

--Modifico term para implementar el azucar sintactica
term = chainl1 factor multopp
--term = chainl1 factor multopp

factor = try (parens lis intexp)
         <|> try (do reservedOp lis "-"
                     f <- factor
                     return (UMinus f))
         <|> (do n <- integer lis
                 return (Const n)
                <|> do  str <- identifier lis
                        return (Var str))

multopp = do try (reservedOp lis "*")
             return Times
          <|> do try (reservedOp lis "/")
                 return Div

addopp = do try (reservedOp lis "+")
            return Plus
         <|> do try (reservedOp lis "-")
                return Minus


-----------------------------------
--- Parser de expressiones booleanas
------------------------------------
boolexp :: Parser BoolExp
boolexp  = chainl1 boolexp2 (try (do reservedOp lis "|"
                                     return Or))

boolexp2 = chainl1 boolexp3 (try (do reservedOp lis "&"
                                     return And))

boolexp3 = try (parens lis boolexp)
           <|> try (do reservedOp lis "~"
                       b <- boolexp3
                       return (Not b))
           <|> intcomp
           <|> boolvalue

intcomp = try (do i <- intexp
                  c <- compopp
                  j <- intexp
                  return (c i j))

compopp = try (do reservedOp lis "="
                  return Eq)
          <|> try (do reservedOp lis "<"
                      return Lt)
          <|> try (do reservedOp lis ">"
                      return Gt)

boolvalue = try (do reserved lis "true"
                    return BTrue)
            <|> try (do reserved lis "false"
                        return BFalse)

-----------------------------------
--- Parser de comandos
-----------------------------------
comm :: Parser Comm
comm = chainl1 comm2 (try (do reservedOp lis ";"
                              return Seq))

comm2 = try (do reserved lis "swap"
                x <- identifier lis
                y <- identifier lis
                return (Swap x y))   -- Comando Swap
        <|> try (do reserved lis "skip"
                    return Skip)
        <|> try (do reserved lis "if"
                    cond <- boolexp
                    reserved lis "then"
                    case1 <- comm
                    reserved lis "else"
                    case2 <- comm
                    reserved lis "end"
                    return (Cond cond case1 case2))
        <|> try (do reserved lis "repeat"
                    c <- comm
                    reserved lis "until"
                    cond <- boolexp
                    reserved lis "end"
                    return (Repeat c cond))
        <|> try (do str <- identifier lis
                    reservedOp lis ":="
                    e <- generalExp
                    return (Let str e))


------------------------------------
-- Funcion de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)

------------------------------------
-- test de parseo
------------------------------------
-- parseIntExp :: String -> Either ParseError IntExp
-- parseIntExp = parse (totParser intexp) ""

-----------------------------------
--- Parser de Objetos
-----------------------------------
{-
reservedOp lis "{" -> Consume simbolos fijos, consume espacios y falla si no aparece el simbolo
identifier lis -> parsea un identificador del objeto (ej nombre, apellido, edad) y retorna un String
sepBy parser separador -> parsea 0 o mas ocurrencias de *parser*, separadas por el *separador* y retorna una lista
-}

-- Parser de un campo del objeto  
-- retorna (String, IntExp) -> String == nombre del campo, IntExp == valor del campo
-- (Ej. edad = 10) obtiene los datos edad, =, 10 y los parsea name <- edad, value<-10, retorna (edad,10)
objField :: Parser (String, Exp)
objField = do
                name <- identifier lis
                reservedOp lis ":"
                value <- generalExp
                return (name, value)

-- Parser para lista de campos
-- Retorna una lista de par campo, valor; separadas por el caracter ","
-- (Ej. [nombre="pepe",edad=15] -> [(nombre,"pepe"), (edad,15)])
objFields :: Parser [(String,Exp)]
objFields = sepBy objField (reservedOp lis ",")

-- Parser de objeto completo
-- Extraigo {, luego obtengo la lista de campos, extraigo } y al final retorno la lista de campos del objeto
objExp :: Parser ObjExp
objExp = do
            reservedOp lis "{"
            fields <- objFields
            reservedOp lis "}"
            return (Obj fields)


-----------------------------------
--- Azucar sintactica
-----------------------------------
-- Acceso a un solo campo
-- Retorna el nombre del identificador del campo
-- (Ej. persona.edad -> retorna edad)
fieldAccess :: Parser String
fieldAccess = do
                reservedOp lis "."
                identifier lis


objAccess :: Parser ObjExp
objAccess = do 
                base <- try (do obj <- objExp
                                return (EObj obj))
                     <|> do v <- identifier lis
                            return (EInt (Var v))
                fields <- many fieldAccess
                return (buildAccess base fields)

--Ejecucion:
{-
buildAccess (Var "persona") ["dir","calle"]
→ buildAccess (Access (Var "persona") "dir") ["calle"]
→ buildAccess (Access (Access (Var "persona") "dir") "calle") []

resultado
-> Access (Access (Var "persona") "dir") "calle"

-}
buildAccess :: Exp -> [String] -> ObjExp
buildAccess exp [] = case exp of
                    EObj o -> o
                    _      -> error "Acceso invalido"
buildAccess exp (f:fs) = buildAccess' (Access exp f) fs

buildAccess' :: ObjExp -> [String] -> ObjExp
buildAccess' obj [] = obj
buildAccess' obj (f:fs) = buildAccess' (Access (EObj obj) f) fs




