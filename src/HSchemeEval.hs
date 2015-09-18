module HSchemeEval (eval, Env) where

import HSchemeParse.LispVal
import LispError
import Control.Monad.Except
import Control.Monad
import Data.IORef (IORef, readIORef, writeIORef, newIORef)

type Env = IORef [(String, IORef LispVal)]

eval :: Env -> LispVal -> IOThrowsLispError LispVal
eval _ val@(Integer _) = return val
eval _ val@(Float _) = return val
eval _ val@(String _) = return val
eval _ val@(Bool _) = return val
eval _ val@(Character _) = return val
eval _ (List [Atom "quote", val]) = return val

eval env (List [Atom "if", predicate, trueExpr, falseExpr]) =
  do b <- eval env predicate
     case b of
       Bool True -> eval env trueExpr
       Bool False -> eval env falseExpr
       x -> throwError $ TypeMismatch "Bool" x

eval env (Atom x) = getVar env x
eval env (List ((Atom "define"):(Atom varName):expr:[])) =
  eval env expr >>= defineVar env varName

eval env (List ((Atom "set!"):(Atom varName):expr:[])) =
  eval env expr >>= setVar env varName

eval env (List ((Atom func):args)) = (mapM (eval env) $ args) >>= apply env func

eval _ x = throwError $ Default $ "Much bafflement evaluating " ++ (show x)

apply :: Env -> String -> [LispVal] -> IOThrowsLispError LispVal
apply _ func args =
  case (lookup func primitives) of
    Just fn -> liftThrows $ fn args
    Nothing -> throwError $ NotFunction "Unrecognised function" func

getVar :: Env -> String -> IOThrowsLispError LispVal
getVar envRef varName = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Variable not bound" varName)
        (liftIO . readIORef)
        (lookup varName env)

isBound :: Env -> String -> IO Bool
isBound envRef varName = do
  env <- readIORef envRef
  return $ maybe False (\_ -> True) (lookup varName env)

setVar :: Env -> String -> LispVal -> IOThrowsLispError LispVal
setVar envRef varName val = do
   env <- liftIO $ readIORef envRef
   maybe (throwError $ UnboundVar "Cannot set! unbound variable" varName)
         (\varIORef -> liftIO (writeIORef varIORef val) >> return val)
         (lookup varName env)

defineVar :: Env -> String -> LispVal -> IOThrowsLispError LispVal
defineVar envRef varName val = do
  bound <- liftIO $ isBound envRef varName
  if (bound)
    then setVar envRef varName val
    else liftIO (do env <- readIORef envRef
                    newBind <- newIORef val
                    writeIORef envRef ((varName, newBind):env)
                    return val)

-- to be used later...
-- bindVars :: Env -> [(String, LispVal)] -> IO Env
-- bindVars = undefined

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
              -- ("vector?", singleParamWrapper isVector),
              ("symbol->string", singleParamWithTypeCheckWrapper symbolToString),
              ("string->symbol", singleParamWithTypeCheckWrapper stringToSymbol),
              ("&&", boolBoolBinOp (&&)),
              ("||", boolBoolBinOp (||)),
              ("string=?" , strBoolBinOp (==)),
              ("string>=?" , strBoolBinOp (>=)),
              ("string<=?" , strBoolBinOp (<=)),
              ("string>?" , strBoolBinOp (>)),
              ("string<?" , strBoolBinOp (<)),
              ("=", numBoolBinOp (==) (==)),
              ("<=", numBoolBinOp (<=) (<=)),
              (">=", numBoolBinOp (>=) (>=)),
              (">", numBoolBinOp (>) (>)),
              ("<", numBoolBinOp (<) (<)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eqv?", eqv),
              ("eq?", eqv),
              ("equal?", eqv),
              ("string", string),
              ("string-length", stringLength),
              ("string-ref", stringRef)]

string :: [LispVal] -> ThrowsLispError LispVal
string xs = foldM strAppend "" xs >>= (return . String)
    where strAppend :: String -> LispVal -> ThrowsLispError String
          strAppend acc next = case next of
                                (Character c) -> return $ acc++[c]
                                x -> throwError $ TypeMismatch "Char" x

stringLength :: [LispVal] -> ThrowsLispError LispVal
stringLength [String str] = return $ Integer (fromIntegral . length $ str)
stringLength [x] = throwError $ TypeMismatch "String" x
stringLength xs = throwError $ NumArgs 1 xs

stringRef :: [LispVal] ->  ThrowsLispError LispVal
stringRef [String _, Integer n] | n<0 = throwError $ Default "Negative index"
stringRef [String [], Integer n] = throwError $ Default "Index out of range"
stringRef [String str, Integer 0] = return $ Character (head str)
stringRef [String str, Integer n] = stringRef [String (tail str), Integer (n-1)]
stringRef vals@[_, _] = throwError $ TypeMismatch "(String, Integer)" (List vals)
stringRef xs = throwError $ NumArgs 2 xs



eqv :: [LispVal] -> ThrowsLispError LispVal
eqv [Atom x, Atom y] = return $ Bool (x==y)
eqv [Integer x, Integer y] = return $ Bool (x==y)
eqv [Float x, Float y] = return $ Bool (x==y)
eqv [String x, String y] = return $ Bool (x==y)
eqv [Bool x, Bool y] = return $ Bool (x==y)
eqv [Character x, Character y] = return $ Bool (x==y)
eqv [DottedList xs x, DottedList ys y] = eqv [List (x:xs), List (y:ys)]

eqv [List xs, List ys] = return $ Bool $ (length xs)==(length ys) && itemsEqv
  where itemsEqv = all pairEqv $ zip xs ys
                    where pairEqv (x, y) = case (eqv [x, y]) of
                                            Left _ -> False
                                            Right (Bool t) -> t
eqv [_, _] = return $ Bool False
eqv bad = throwError $ NumArgs 2 bad

car :: [LispVal] -> ThrowsLispError LispVal
car [List (x:_)] = return x
car [DottedList (x:_) _] = return x
car [x] = throwError $ TypeMismatch "pair" x
car x = throwError $ NumArgs 1 x

cdr :: [LispVal] -> ThrowsLispError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [x] = throwError $ TypeMismatch "pair" x
cdr x = throwError $ NumArgs 1 x

cons :: [LispVal] -> ThrowsLispError LispVal
cons [x, (List xs)] = return $ List (x:xs)
cons [x, (DottedList xs y)] = return $ DottedList (x:xs) y
cons [x, y] = return $ DottedList [x] y
cons xs = throwError $ NumArgs 2 xs


boolBinOp :: (LispVal -> ThrowsLispError a) -> (a -> a -> Bool)
                -> [LispVal] -> (ThrowsLispError LispVal)
boolBinOp unpack op xs =
  case (length xs) of
    2 ->  do b <- (liftM2 op (unpack $ xs !! 0) (unpack $ xs !! 1))
             return $ Bool b
    otherwise -> throwError $ NumArgs 2 xs

unpackBool :: LispVal -> ThrowsLispError Bool
unpackBool (Bool b) = return b
unpackBool x = throwError $ TypeMismatch "Boolean" x

boolBoolBinOp :: (Bool -> Bool -> Bool) -> [LispVal] -> (ThrowsLispError LispVal)
boolBoolBinOp = boolBinOp unpackBool

unpackString :: LispVal -> ThrowsLispError String
unpackString (String s) = return s
unpackString x = throwError $ TypeMismatch "String" x

strBoolBinOp :: (String -> String -> Bool) -> [LispVal] -> (ThrowsLispError LispVal)
strBoolBinOp = boolBinOp unpackString

numBoolBinOp :: (Float -> Float -> Bool)
                  -> (Integer -> Integer -> Bool)
                  -> [LispVal] -> (ThrowsLispError LispVal)
numBoolBinOp fop iop xs@((Integer x):rest) = boolBinOp unpackInteger iop xs
numBoolBinOp fop iop xs@((Float x):rest) = boolBinOp unpackFloat fop xs
numBoolBinOp _ _ (x:rest) = throwError $ TypeMismatch "Numeric" x

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
  -- Vector _ -> String "Vector"

isList :: LispVal -> LispVal
isList (List _) = Bool True
isList _ = Bool False

-- isVector :: LispVal -> LispVal
-- isVector (Vector _) = Bool True
-- isVector _ = Bool False

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

type BinOp a = (a -> a -> a)
closedBinaryNumeric :: (BinOp Float) -> (BinOp Integer) -> [LispVal] -> ThrowsLispError LispVal
closedBinaryNumeric _ _ [] = throwError $ Default "Expected at least one paramter"
closedBinaryNumeric floatOp integerOp xs =
  case (head xs) of
    x@(Integer _) -> do unpacked_xs <- (mapM unpackInteger xs)
                        let val = foldl1 integerOp unpacked_xs
                        return $ Integer val
    x@(Float _) -> do unpacked_xs <- (mapM unpackFloat xs)
                      let val = foldl1 floatOp unpacked_xs
                      return $ Float val
    x@otherwise -> throwError $ TypeMismatch "Numeric" x

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
