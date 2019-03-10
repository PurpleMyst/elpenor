{-# LANGUAGE ViewPatterns #-}
module Marshal(Marshal(..), MarshalWithType(..)) where

import Data.Binary (encode)
import Data.Bits
import Data.Char
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
    (marshal (toInteger $ B.length s))
    s

instance MarshalWithType ByteString where
  typeChar = const 's'

instance Marshal Integer where
  marshal n
      | n < 0xFFFFFFFF = B.pack [go 0, go 1, go 2, go 3]
      | otherwise      = error "can not marshal integrals larger than 32 bits"
    where
      go (fromInteger -> x) = chr $ (fromInteger n .&. (0xFF `shiftL` (8 * x))) `shiftR` (8 * x)

instance MarshalWithType Integer where
  typeChar n
    | n < 0xFFFFFFFF = 'i'
    | otherwise      = error "can not marshal integrals larger than 32 bits"

instance Marshal AST where
  marshal (String s)     = marshal (B.drop 8 $ encode s)
  marshal (Identifier s) = marshal (String s)
  marshal (Number n)     = marshal n
  marshal _              = undefined

instance MarshalWithType AST where
  typeChar (String _)     = 'u'
  typeChar (Identifier _) = 'u'
  typeChar (Number _)     = 'i'
  typeChar _              = undefined

instance MarshalWithType a => Marshal [a] where
  marshal xs = B.append (marshal $ toInteger $ length xs) (B.concat $ marshalWithType <$> xs)

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
