module CodeObject where

import AST

import Data.Int
import Data.ByteString.Lazy (ByteString)

data CodeObject = CodeObject
                { co_argcount       :: Int32
                , co_kwonlyargcount :: Int32
                , co_nlocals        :: Int32
                , co_stacksize      :: Int32
                , co_flags          :: Int32
                , co_code           :: ByteString
                , co_consts         :: [PyValue]
                , co_names          :: [String]
                , co_varnames       :: [String]
                , co_filename       :: String
                , co_name           :: String
                , co_firstlineno    :: Int32
                , co_lnotab         :: ByteString
                , co_freevars       :: [String]
                , co_cellvars       :: [String]
                } deriving (Eq, Show)

data PyValue = PyString String | PyInt32 Int32 | PyNone deriving (Eq, Show)

class ToPyValue a where
    toPyValue :: a -> PyValue

instance ToPyValue PyValue where
    toPyValue = id

instance ToPyValue AST where
    toPyValue (Number n) = PyInt32 n
    toPyValue (String n) = PyString n
    toPyValue None       = PyNone
    toPyValue _          = undefined
