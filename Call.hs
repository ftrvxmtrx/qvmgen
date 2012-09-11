{-# LANGUAGE NamedFieldPuns #-}
module Call(callBuiltin) where

import Data.List

import TagParse

maxArgIndex = 8

arg :: Int -> String
arg i =
  "vm->args[" ++ show i ++ "]"

argB :: BaseType -> Int -> String
argB t i =
  case t of
    QBool   -> "!!" ++ s ++ "f"
    QInt    -> s ++ "f"
    QString -> "vm_string(vm, " ++ s ++ "s)"
    QFloat  -> s ++ "f"
    QVector -> s ++ "v"
    QEntity -> "vm_entity(vm, " ++ s ++ "e)"
    QFunc _ -> error "function as argument not supported yet"
  where
    s = arg i ++ "."

argT :: Type -> Int -> String
argT t i =
  case t of
    Value b  -> argB b i
    Field b  -> arg i ++ ".i"
    AnyField -> arg i ++ ".i"
    AnyValue -> arg i

sigToArgs :: Sig -> [String]
sigToArgs s =
  aux 0 s []
  where
    aux :: Int -> Sig -> [String] -> [String]
    aux i (Arg _ t s) accu =
      aux (succ i) s $ accu ++ [argT t i]
    aux i (OptArgs optArgs retType) accu =
      aux i (Return retType) $ auxOpt i optArgs accu
    aux i (Return Nothing) accu =
      accu
    aux i (Return (Just t)) accu =
      accu

    auxOpt :: Int -> OptArgs -> [String] -> [String]
    auxOpt i (OptArg _ t s) accu =
      accu
    auxOpt i (VarArg0 _ _ t) accu =
      accu ++ ["argc - " ++ show i] ++ map (argT t) [i..maxArgIndex]
    auxOpt i (VarArg1 _ _ t) accu =
      accu ++ [argT t i, "argc - " ++ show (succ i)] ++ map (argT t) [succ i..maxArgIndex]

callBuiltin :: TagData -> String
callBuiltin Builtin{ cFunc, sig } =
  cFunc ++ "(" ++ args ++ ")"
  where
    args = intercalate ", " $ ["vm"] ++ sigToArgs sig
callBuiltin _ =
  error "not a builtin"
