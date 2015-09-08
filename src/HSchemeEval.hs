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
              ("remainder", integerBinaryOp rem)]

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
