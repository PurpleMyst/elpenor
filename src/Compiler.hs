{-# LANGUAGE ViewPatterns #-}
module Compiler where

import Control.Monad.State.Lazy
import Data.Int
import Data.List
import Data.Word
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B

import AST
import Parser

data CodeObject = CodeObject
                { co_argcount       :: Int32
                , co_kwonlyargcount :: Int32
                , co_nlocals        :: Int32
                , co_stacksize      :: Int32
                , co_flags          :: Int32
                , co_code           :: ByteString
                , co_consts         :: [AST]
                , co_names          :: [String]
                , co_varnames       :: [String]
                , co_filename       :: String
                , co_name           :: String
                , co_firstlineno    :: Int32
                , co_lnotab         :: ByteString
                , co_freevars       :: [String]
                , co_cellvars       :: [String]
                } deriving Show
--
-- opcode {{{
opcode :: String -> Word8
opcode "POP_TOP" = 1
opcode "ROT_TWO" = 2
opcode "ROT_THREE" = 3
opcode "DUP_TOP" = 4
opcode "DUP_TOP_TWO" = 5
opcode "NOP" = 9
opcode "UNARY_POSITIVE" = 10
opcode "UNARY_NEGATIVE" = 11
opcode "UNARY_NOT" = 12
opcode "UNARY_INVERT" = 15
opcode "BINARY_MATRIX_MULTIPLY" = 16
opcode "INPLACE_MATRIX_MULTIPLY" = 17
opcode "BINARY_POWER" = 19
opcode "BINARY_MULTIPLY" = 20
opcode "BINARY_MODULO" = 22
opcode "BINARY_ADD" = 23
opcode "BINARY_SUBTRACT" = 24
opcode "BINARY_SUBSCR" = 25
opcode "BINARY_FLOOR_DIVIDE" = 26
opcode "BINARY_TRUE_DIVIDE" = 27
opcode "INPLACE_FLOOR_DIVIDE" = 28
opcode "INPLACE_TRUE_DIVIDE" = 29
opcode "GET_AITER" = 50
opcode "GET_ANEXT" = 51
opcode "BEFORE_ASYNC_WITH" = 52
opcode "INPLACE_ADD" = 55
opcode "INPLACE_SUBTRACT" = 56
opcode "INPLACE_MULTIPLY" = 57
opcode "INPLACE_MODULO" = 59
opcode "STORE_SUBSCR" = 60
opcode "DELETE_SUBSCR" = 61
opcode "BINARY_LSHIFT" = 62
opcode "BINARY_RSHIFT" = 63
opcode "BINARY_AND" = 64
opcode "BINARY_XOR" = 65
opcode "BINARY_OR" = 66
opcode "INPLACE_POWER" = 67
opcode "GET_ITER" = 68
opcode "GET_YIELD_FROM_ITER" = 69
opcode "PRINT_EXPR" = 70
opcode "LOAD_BUILD_CLASS" = 71
opcode "YIELD_FROM" = 72
opcode "GET_AWAITABLE" = 73
opcode "INPLACE_LSHIFT" = 75
opcode "INPLACE_RSHIFT" = 76
opcode "INPLACE_AND" = 77
opcode "INPLACE_XOR" = 78
opcode "INPLACE_OR" = 79
opcode "BREAK_LOOP" = 80
opcode "WITH_CLEANUP_START" = 81
opcode "WITH_CLEANUP_FINISH" = 82
opcode "RETURN_VALUE" = 83
opcode "IMPORT_STAR" = 84
opcode "SETUP_ANNOTATIONS" = 85
opcode "YIELD_VALUE" = 86
opcode "POP_BLOCK" = 87
opcode "END_FINALLY" = 88
opcode "POP_EXCEPT" = 89
opcode "STORE_NAME" = 90
opcode "DELETE_NAME" = 91
opcode "UNPACK_SEQUENCE" = 92
opcode "FOR_ITER" = 93
opcode "UNPACK_EX" = 94
opcode "STORE_ATTR" = 95
opcode "DELETE_ATTR" = 96
opcode "STORE_GLOBAL" = 97
opcode "DELETE_GLOBAL" = 98
opcode "LOAD_CONST" = 100
opcode "LOAD_NAME" = 101
opcode "BUILD_TUPLE" = 102
opcode "BUILD_LIST" = 103
opcode "BUILD_SET" = 104
opcode "BUILD_MAP" = 105
opcode "LOAD_ATTR" = 106
opcode "COMPARE_OP" = 107
opcode "IMPORT_NAME" = 108
opcode "IMPORT_FROM" = 109
opcode "JUMP_FORWARD" = 110
opcode "JUMP_IF_FALSE_OR_POP" = 111
opcode "JUMP_IF_TRUE_OR_POP" = 112
opcode "JUMP_ABSOLUTE" = 113
opcode "POP_JUMP_IF_FALSE" = 114
opcode "POP_JUMP_IF_TRUE" = 115
opcode "LOAD_GLOBAL" = 116
opcode "CONTINUE_LOOP" = 119
opcode "SETUP_LOOP" = 120
opcode "SETUP_EXCEPT" = 121
opcode "SETUP_FINALLY" = 122
opcode "LOAD_FAST" = 124
opcode "STORE_FAST" = 125
opcode "DELETE_FAST" = 126
opcode "STORE_ANNOTATION" = 127
opcode "RAISE_VARARGS" = 130
opcode "CALL_FUNCTION" = 131
opcode "MAKE_FUNCTION" = 132
opcode "BUILD_SLICE" = 133
opcode "LOAD_CLOSURE" = 135
opcode "LOAD_DEREF" = 136
opcode "STORE_DEREF" = 137
opcode "DELETE_DEREF" = 138
opcode "CALL_FUNCTION_KW" = 141
opcode "CALL_FUNCTION_EX" = 142
opcode "SETUP_WITH" = 143
opcode "EXTENDED_ARG" = 144
opcode "LIST_APPEND" = 145
opcode "SET_ADD" = 146
opcode "MAP_ADD" = 147
opcode "LOAD_CLASSDEREF" = 148
opcode "BUILD_LIST_UNPACK" = 149
opcode "BUILD_MAP_UNPACK" = 150
opcode "BUILD_MAP_UNPACK_WITH_CALL" = 151
opcode "BUILD_TUPLE_UNPACK" = 152
opcode "BUILD_SET_UNPACK" = 153
opcode "SETUP_ASYNC_WITH" = 154
opcode "FORMAT_VALUE" = 155
opcode "BUILD_CONST_KEY_MAP" = 156
opcode "BUILD_STRING" = 157
opcode "BUILD_TUPLE_UNPACK_WITH_CALL" = 158
opcode _ = error "unknown opcode"
-- }}}


-- NB: Opcode/arg must be reversed so that they are correct later.
addOpcode :: Integral a => String -> a -> State CodeObject ()
addOpcode (opcode -> o) (fromIntegral -> arg) =
  modify (\co -> co { co_code = B.cons arg $ B.cons o $ co_code co })

minimumStackSize :: (Integral a) => a -> State CodeObject ()
minimumStackSize (fromIntegral -> n) = modify (\co -> co { co_stacksize = if co_stacksize co < n then n else co_stacksize co })

addName :: String -> State CodeObject Int
addName name = do
    co <- get

    if name `elem` co_names co
      then do
        let Just idx = name `elemIndex` co_names co
        return $ fromIntegral $ length (co_names co) - (idx + 1)
      else do
        put $ co { co_names = name : co_names co }
        return $ fromIntegral $ length (co_names co)

addConst :: AST -> State CodeObject Int
addConst ast = do
    co <- get
    if ast `elem` co_consts co
      then do
        let Just idx = ast `elemIndex` co_consts co
        return $ length (co_consts co) - (idx + 1)
      else do
        put $ co { co_consts = ast : co_consts co }
        return $ length (co_consts co)


compile' :: AST -> State CodeObject ()

compile' (Program xs) = mapM_ compile' xs >> addConst None >>= addOpcode "RETURN_VALUE"

compile' (Assignment name rhs) = compile' rhs >> addName name >>= addOpcode "STORE_NAME"

compile' (Identifier name) = addName name >>= addOpcode "LOAD_NAME"

compile' ast@(Number _) = addConst ast >>= addOpcode "LOAD_CONST"
compile' ast@(String _) = addConst ast >>= addOpcode "LOAD_CONST"
compile' ast@None       = addConst ast >>= addOpcode "LOAD_CONST"

compile' (FunctionCall name args) = do
  compile' name
  mapM_ compile' args
  minimumStackSize (length args + 1)
  addOpcode "CALL_FUNCTION" (fromIntegral $ length args :: Integer)

compile :: String -> CodeObject
compile = cleanUp . flip execState co . compile' . parse
  where
    cleanUp co' = co' { co_code     = B.reverse (co_code co')
                      , co_names    = reverse   (co_names co')
                      , co_consts   = reverse   (co_consts co')
                      , co_varnames = reverse   (co_varnames co')
                      , co_lnotab   = B.reverse (co_lnotab co')
                      , co_freevars = reverse   (co_freevars co')
                      , co_cellvars = reverse   (co_cellvars co')
                      }

    co = CodeObject
                { co_argcount       = 0
                , co_cellvars       = []
                , co_code           = B.empty
                , co_consts         = []
                , co_filename       = ""
                , co_firstlineno    = 0
                , co_flags          = 0
                , co_freevars       = []
                , co_kwonlyargcount = 0
                , co_lnotab         = B.empty
                , co_name           = ""
                , co_names          = []
                , co_nlocals        = 0
                , co_stacksize      = 0
                , co_varnames       = []
                }

-- vim: foldmethod=marker
