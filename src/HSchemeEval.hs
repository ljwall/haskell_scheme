module HSchemeEval (eval) where

import HSchemeParse.LispVal
import LispError
import Control.Monad.Except
import Control.Monad

eval :: LispVal -> ThrowsLispError LispVal
eval val@(Integer _) = return val
eval val@(Float _) = return val
eval val@(String _) = return val
eval val@(Bool _) = return val
eval val@(Character _) = return val
eval (List [Atom "quote", val]) = return val

eval (List ((Atom func):args)) = (mapM eval $ args) >>= apply func

apply :: String -> [LispVal] -> ThrowsLispError LispVal
apply func args =
  case (lookup func primitives) of
    Just fn -> fn args
    Nothing -> throwError $ NotFunction "Unrecognised function" func

primitives :: [(String, [LispVal] -> ThrowsLispError LispVal)]
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
              ("symbol->string", singleParamWithTypeCheckWrapper symbolToString),
              ("string->symbol", singleParamWithTypeCheckWrapper stringToSymbol)]

singleParamWithTypeCheckWrapper :: (LispVal -> ThrowsLispError LispVal) -> [LispVal]
                                        -> ThrowsLispError LispVal
singleParamWithTypeCheckWrapper fn xs =
  case (length xs) of
    1 -> fn (xs !! 0)
    otherwise -> throwError $ NumArgs 1 xs

singleParamWrapper :: (LispVal -> LispVal) -> [LispVal] -> ThrowsLispError LispVal
singleParamWrapper fn = singleParamWithTypeCheckWrapper (return . fn)

symbolToString :: LispVal -> ThrowsLispError LispVal
symbolToString (Atom x) = return $ String x
symbolToString x = throwError $ TypeMismatch "Symbol" x

stringToSymbol :: LispVal -> ThrowsLispError LispVal
stringToSymbol (String x) = return $ Atom x
stringToSymbol x = throwError $ TypeMismatch "String" x

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

-- data LispNumType =  LTFloat | LTInteger -- Ordering is importan here
--   deriving (Eq, Ord)
--
-- lispNumType :: LispVal -> Maybe LispNumType
-- lispNumType x = case x of
--   (Float _) -> (Just LTFloat)
--   (Integer _) -> (Just LTInteger)
--   otherwise -> Nothing
--
-- allLispNumType :: [LispVal] -> Maybe LispNumType
-- allLispNumType = minimum . map lispNumType

type BinOp a = (a -> a -> a)
closedBinaryNumeric :: (BinOp Float) -> (BinOp Integer) -> [LispVal] -> ThrowsLispError LispVal
closedBinaryNumeric floatOp integerOp xs =
  case (head xs) of
    x@(Integer _) -> do unpacked_xs <- (mapM unpackInteger xs)
                        let val = foldl1 integerOp unpacked_xs
                        return $ Integer val
    x@(Float _) -> do unpacked_xs <- (mapM unpackFloat xs)
                      let val = foldl1 floatOp unpacked_xs
                      return $ Float val
    x@otherwise -> throwError $ TypeMismatch "Numeric" x
-- closedBinaryNumeric :: (BinOp Float) -> (BinOp Integer) -> [LispVal] -> ThrowsLispError LispVal
-- closedBinaryNumeric floatOp integerOp xs =
--   case (allLispNumType xs) of
--     (Just LTFloat) -> return . Float . foldl1 floatOp . map (extractValue. unpackFloat) $ xs
--     (Just LTInteger) -> return . Integer . foldl1 integerOp . map (extractValue. unpackInteger) $ xs
--     otherwise -> throwError $ TypeMismatch "all numerics" "non-numerics"

integerBinaryOp :: (BinOp Integer) -> [LispVal] -> ThrowsLispError LispVal
integerBinaryOp op xs = case (foldl1 (liftM2 op) . map unpackInteger $ xs) of
  Left err -> throwError err
  Right result -> return $ Integer result

floatBinaryOp :: (BinOp Float) -> [LispVal] -> ThrowsLispError LispVal
floatBinaryOp op xs = case (foldl1 (liftM2 op) . map unpackFloat $ xs) of
  Left err -> throwError err
  Right result -> return $ Float result

unpackInteger :: LispVal -> ThrowsLispError Integer
unpackInteger (Integer x) =  return x
unpackInteger x = throwError $ TypeMismatch "Integer" x

unpackFloat :: LispVal -> ThrowsLispError Float
unpackFloat (Integer x) = return $ fromInteger x
unpackFloat (Float x) = return x
unpackFloat x = throwError $ TypeMismatch "Float or Integer" x
