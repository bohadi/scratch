{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LispVal {
    LispVal(..)
  , showVal
} where

import Data.Text as T
import Data.Map as Map
import Data.Typeable
import Control.Monad.Except
import Control.Monad.Reader

data LispVal =
    Atom T.Text
  | Bool Bool
  | Number Integer
  | String T.Text
  | List [LispVal]
  | Fun IFunc
  | Lambda IFunc EnvCtx
  | Nil
  deriving (Typeable)

data IFunc = IFunc { fn :: [LispVal] -> Eval LispVal }
type EnvCtx = Map.Map T.Text LispVal

newtype Eval a = Eval { unEval :: ReaderT EnvCtx IO a }
  deriving ( Functor , Applicative
           , Monad   , MonadReader EnvCtx
           , MonadIO )

instance Show LispVal where
    show = T.unpack . showVal

showVal :: LispVal -> T.Text
showVal val = case val of
    (Atom atom)     -> atom
    (Bool True)     -> "#t"
    (Bool False)    -> "#f"
    (Number num)    -> T.pack $ show num
    (String str)    -> T.concat ["\"", str, "\""]
    (List contents) ->
        T.concat ["(", T.unwords $ showVal <$> contents, ")"]
    (Fun _)         -> "(fn)"
    (Lambda _ _)    -> "(λ)"
    Nil             -> "Nil"

