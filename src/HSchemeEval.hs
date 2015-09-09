module HSchemeEval (eval) where

import HSchemeParse.LispVal

eval :: LispVal -> LispVal
eval val@(Integer _) = val
eval val@(Float _) = val
eval val@(String _) = val
eval val@(Bool _) = val
eval val@(Character _) = val
eval (List [Atom "quote", val]) = val

eval (List ((Atom func):args)) = apply func (map eval args)

apply :: String -> [LispVal] -> LispVal
apply func args =
  case (lookup func primitives) of
    Just fn -> fn args
    Nothing -> Bool False

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", closedBinaryNumeric (+) (+)),
              ("*", closedBinaryNumeric (*) (*)),
              ("-", closedBinaryNumeric (-) (-)),
              ("/", floatBinaryOp (/)),
              ("mod", integerBinaryOp mod),
              ("quotent", integerBinaryOp quot),
              ("remainder", integerBinaryOp rem),
              ("typeof", singleParamWrapper typeOf),
              ("symbol?", singleParamWrapper isAtom),
              ("float?", singleParamWrapper isFloat),
              ("integer?", singleParamWrapper isInteger),
              ("string?", singleParamWrapper isString),
              ("boolean?", singleParamWrapper isBool),
              ("char?", singleParamWrapper isChar),
              ("list?", singleParamWrapper isList),
              ("vector?", singleParamWrapper isVector),
              ("symbol->string", singleParamWrapper symbolToString),
              ("string->symbol", singleParamWrapper stringToSymbol)]


singleParamWrapper :: (LispVal -> LispVal) -> [LispVal] -> LispVal
singleParamWrapper fn xs =
  case (length xs) of
    1 -> fn (xs !! 0)
    -- otherwise -> failiur somehow?

symbolToString :: LispVal -> LispVal
symbolToString (Atom x) = String x

stringToSymbol :: LispVal -> LispVal
stringToSymbol (String x) = Atom x

typeOf :: LispVal -> LispVal
typeOf expr = case expr of
  Atom _ -> String "Symbol"
  Integer _ -> String "Integer"
  Float _ -> String "Float"
  String _ -> String "String"
  Bool _ -> String "Boolean"
  Character _ -> String "Character"
  List _ -> String "List"
  DottedList _ _ -> String "DottedList"
  Vector _ -> String "Vector"

isList :: LispVal -> LispVal
isList (List _) = Bool True
isList _ = Bool False

isVector :: LispVal -> LispVal
isVector (Vector _) = Bool True
isVector _ = Bool False

isFloat :: LispVal -> LispVal
isFloat (Float _) = Bool True
isFloat _ = Bool False

isInteger :: LispVal -> LispVal
isInteger (Integer _) = Bool True
isInteger _ = Bool False

isString :: LispVal -> LispVal
isString (String _) = Bool True
isString _ = Bool False

isAtom :: LispVal -> LispVal
isAtom (Atom _) = Bool True
isAtom _ = Bool False

isBool :: LispVal -> LispVal
isBool (Bool _) = Bool True
isBool _ = Bool False

isChar :: LispVal -> LispVal
isChar (Character _) = Bool True
isChar _ = Bool False

data LispNumType =  LTFloat | LTInteger -- Ordering is importan here
  deriving (Eq, Ord)

lispNumType :: LispVal -> Maybe LispNumType
lispNumType x = case x of
  (Float _) -> (Just LTFloat)
  (Integer _) -> (Just LTInteger)
  otherwise -> Nothing

allLispNumType :: [LispVal] -> Maybe LispNumType
allLispNumType = minimum . map lispNumType

type BinOp a = (a -> a -> a)
closedBinaryNumeric :: (BinOp Float) -> (BinOp Integer) -> [LispVal] -> LispVal
closedBinaryNumeric floatOp integerOp xs =
  case (allLispNumType xs) of
    (Just LTFloat) -> Float . foldl1 floatOp . map unpackFloat $ xs
    (Just LTInteger) -> Integer . foldl1 integerOp . map unpackInteger $ xs
    otherwise -> Integer 0 -- Should Error

integerBinaryOp :: (BinOp Integer) -> [LispVal] -> LispVal
integerBinaryOp op = Integer . foldl1 op . map unpackInteger

floatBinaryOp :: (BinOp Float) -> [LispVal] -> LispVal
floatBinaryOp op = Float . foldl1 op . map unpackFloat

unpackInteger :: LispVal -> Integer
unpackInteger (Integer x) = x

unpackFloat :: LispVal -> Float
unpackFloat (Integer x) = fromInteger x
unpackFloat (Float x) = x
