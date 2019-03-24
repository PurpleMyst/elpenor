{-# LANGUAGE TypeApplications #-}
module Marshal(Marshal(..), MarshalWithType(..)) where

import Data.Binary
import Data.Int
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B

import AST
import CodeObject

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

instance MarshalWithType a => Marshal [a] where
    marshal xs = B.append (marshal @Int32 $ fromIntegral $ length xs) (B.concat $ marshalWithType <$> xs)

instance MarshalWithType a => MarshalWithType [a] where
    typeChar = const '('

instance Marshal CodeObject where
    marshal co = B.concat
      [ marshal         $              co_argcount co
      , marshal         $              co_kwonlyargcount co
      , marshal         $              co_nlocals co
      , marshal         $              co_stacksize co
      , marshal         $              co_flags co
      , marshalWithType $              co_code co
      , marshalWithType $              co_consts co
      , marshalWithType $ PyString <$> co_names co
      , marshalWithType $ PyString <$> co_varnames co
      , marshalWithType $ PyString <$> co_freevars co
      , marshalWithType $ PyString <$> co_cellvars co
      , marshalWithType $ PyString  $  co_filename co
      , marshalWithType $ PyString  $  co_name co
      , marshal         $              co_firstlineno co
      , marshalWithType $              co_lnotab co
      ]

instance MarshalWithType CodeObject where
    typeChar = const 'c'

instance Marshal PyValue where
    marshal (PyString s) = marshal $ B.drop 8 $ encode s
    marshal (PyInt32 n)  = marshal n
    marshal PyNone       = B.empty

instance MarshalWithType PyValue where
    typeChar (PyString _) = 'u'
    typeChar (PyInt32 _)  = 'i'
    typeChar PyNone       = 'N'
