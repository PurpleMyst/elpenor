{-# LANGUAGE TypeApplications #-}
module Marshal(Marshal(..), MarshalWithType(..)) where

import Data.Binary
import Data.Int
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B

import AST
import Compiler

class Marshal a where
  marshal :: a -> ByteString

class Marshal a => MarshalWithType a where
  typeChar :: a -> Char

  marshalWithType :: a -> ByteString
  marshalWithType x = B.cons (typeChar x) (marshal x)

instance Marshal ByteString where
  marshal s =
    B.append
    (marshal @Int32 $ fromIntegral $ B.length s)
    s

instance MarshalWithType ByteString where
  typeChar = const 's'

instance Marshal Int32 where
  marshal = B.reverse . encode

instance MarshalWithType Int32 where
  typeChar = const 'i'

instance Marshal AST where
  marshal (String s)     = marshal $ B.drop 8 $ encode s
  marshal (Identifier s) = marshal $ String s
  marshal (Number n)     = marshal n
  marshal None           = B.empty
  marshal _              = undefined

instance MarshalWithType AST where
  typeChar (String _)     = 'u'
  typeChar (Identifier _) = 'u'
  typeChar (Number _)     = 'i'
  typeChar None           = 'N'
  typeChar x              = error $ "no typeChar for " ++ show x

instance MarshalWithType a => Marshal [a] where
  marshal xs = B.append (marshal @Int32 $ fromIntegral $ length xs) (B.concat $ marshalWithType <$> xs)

instance MarshalWithType a => MarshalWithType [a] where
  typeChar = const '('

instance Marshal CodeObject where
  marshal co = B.concat
    [ marshal         $            co_argcount co
    , marshal         $            co_kwonlyargcount co
    , marshal         $            co_nlocals co
    , marshal         $            co_stacksize co
    , marshal         $            co_flags co
    , marshalWithType $            co_code co
    , marshalWithType $            co_consts co
    , marshalWithType $ String <$> co_names co
    , marshalWithType $ String <$> co_varnames co
    , marshalWithType $ String <$> co_freevars co
    , marshalWithType $ String <$> co_cellvars co
    , marshalWithType $ String $   co_filename co
    , marshalWithType $ String $   co_name co
    , marshal         $            co_firstlineno co
    , marshalWithType $            co_lnotab co
    ]

instance MarshalWithType CodeObject where
  typeChar = const 'c'
