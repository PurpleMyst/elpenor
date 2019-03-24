module Opcode(Opname(..), encode) where

import Data.Word (Word8)

import qualified Data.ByteString.Lazy as B

unusedArg = 0

data Opname = POP_TOP
            | ROT_TWO
            | ROT_THREE
            | DUP_TOP
            | DUP_TOP_TWO
            | NOP
            | UNARY_POSITIVE
            | UNARY_NEGATIVE
            | UNARY_NOT
            | UNARY_INVERT
            | BINARY_MATRIX_MULTIPLY
            | INPLACE_MATRIX_MULTIPLY
            | BINARY_POWER
            | BINARY_MULTIPLY
            | BINARY_MODULO
            | BINARY_ADD
            | BINARY_SUBTRACT
            | BINARY_SUBSCR
            | BINARY_FLOOR_DIVIDE
            | BINARY_TRUE_DIVIDE
            | INPLACE_FLOOR_DIVIDE
            | INPLACE_TRUE_DIVIDE
            | GET_AITER
            | GET_ANEXT
            | BEFORE_ASYNC_WITH
            | INPLACE_ADD
            | INPLACE_SUBTRACT
            | INPLACE_MULTIPLY
            | INPLACE_MODULO
            | STORE_SUBSCR
            | DELETE_SUBSCR
            | BINARY_LSHIFT
            | BINARY_RSHIFT
            | BINARY_AND
            | BINARY_XOR
            | BINARY_OR
            | INPLACE_POWER
            | GET_ITER
            | GET_YIELD_FROM_ITER
            | PRINT_EXPR
            | LOAD_BUILD_CLASS
            | YIELD_FROM
            | GET_AWAITABLE
            | INPLACE_LSHIFT
            | INPLACE_RSHIFT
            | INPLACE_AND
            | INPLACE_XOR
            | INPLACE_OR
            | BREAK_LOOP
            | WITH_CLEANUP_START
            | WITH_CLEANUP_FINISH
            | RETURN_VALUE
            | IMPORT_STAR
            | SETUP_ANNOTATIONS
            | YIELD_VALUE
            | POP_BLOCK
            | END_FINALLY
            | POP_EXCEPT
            | STORE_NAME Word8
            | DELETE_NAME
            | UNPACK_SEQUENCE
            | FOR_ITER
            | UNPACK_EX
            | STORE_ATTR
            | DELETE_ATTR
            | STORE_GLOBAL
            | DELETE_GLOBAL
            | LOAD_CONST Word8
            | LOAD_NAME Word8
            | BUILD_TUPLE
            | BUILD_LIST
            | BUILD_SET
            | BUILD_MAP
            | LOAD_ATTR
            | COMPARE_OP
            | IMPORT_NAME
            | IMPORT_FROM
            | JUMP_FORWARD
            | JUMP_IF_FALSE_OR_POP
            | JUMP_IF_TRUE_OR_POP
            | JUMP_ABSOLUTE
            | POP_JUMP_IF_FALSE
            | POP_JUMP_IF_TRUE
            | LOAD_GLOBAL
            | CONTINUE_LOOP
            | SETUP_LOOP
            | SETUP_EXCEPT
            | SETUP_FINALLY
            | LOAD_FAST
            | STORE_FAST
            | DELETE_FAST
            | STORE_ANNOTATION
            | RAISE_VARARGS
            | CALL_FUNCTION Word8
            | MAKE_FUNCTION
            | BUILD_SLICE
            | LOAD_CLOSURE
            | LOAD_DEREF
            | STORE_DEREF
            | DELETE_DEREF
            | CALL_FUNCTION_KW
            | CALL_FUNCTION_EX
            | SETUP_WITH
            | EXTENDED_ARG
            | LIST_APPEND
            | SET_ADD
            | MAP_ADD
            | LOAD_CLASSDEREF
            | BUILD_LIST_UNPACK
            | BUILD_MAP_UNPACK
            | BUILD_MAP_UNPACK_WITH_CALL
            | BUILD_TUPLE_UNPACK
            | BUILD_SET_UNPACK
            | SETUP_ASYNC_WITH
            | FORMAT_VALUE
            | BUILD_CONST_KEY_MAP
            | BUILD_STRING
            | BUILD_TUPLE_UNPACK_WITH_CALL

encode POP_TOP                      = B.cons 1 (B.singleton unusedArg)
encode ROT_TWO                      = B.cons 2 (B.singleton unusedArg)
encode ROT_THREE                    = B.cons 3 (B.singleton unusedArg)
encode DUP_TOP                      = B.cons 4 (B.singleton unusedArg)
encode DUP_TOP_TWO                  = B.cons 5 (B.singleton unusedArg)
encode NOP                          = B.cons 9 (B.singleton unusedArg)
encode UNARY_POSITIVE               = B.cons 10 (B.singleton unusedArg)
encode UNARY_NEGATIVE               = B.cons 11 (B.singleton unusedArg)
encode UNARY_NOT                    = B.cons 12 (B.singleton unusedArg)
encode UNARY_INVERT                 = B.cons 15 (B.singleton unusedArg)
encode BINARY_MATRIX_MULTIPLY       = B.cons 16 (B.singleton unusedArg)
encode INPLACE_MATRIX_MULTIPLY      = B.cons 17 (B.singleton unusedArg)
encode BINARY_POWER                 = B.cons 19 (B.singleton unusedArg)
encode BINARY_MULTIPLY              = B.cons 20 (B.singleton unusedArg)
encode BINARY_MODULO                = B.cons 22 (B.singleton unusedArg)
encode BINARY_ADD                   = B.cons 23 (B.singleton unusedArg)
encode BINARY_SUBTRACT              = B.cons 24 (B.singleton unusedArg)
encode BINARY_SUBSCR                = B.cons 25 (B.singleton unusedArg)
encode BINARY_FLOOR_DIVIDE          = B.cons 26 (B.singleton unusedArg)
encode BINARY_TRUE_DIVIDE           = B.cons 27 (B.singleton unusedArg)
encode INPLACE_FLOOR_DIVIDE         = B.cons 28 (B.singleton unusedArg)
encode INPLACE_TRUE_DIVIDE          = B.cons 29 (B.singleton unusedArg)
encode GET_AITER                    = B.cons 50 (B.singleton unusedArg)
encode GET_ANEXT                    = B.cons 51 (B.singleton unusedArg)
encode BEFORE_ASYNC_WITH            = B.cons 52 (B.singleton unusedArg)
encode INPLACE_ADD                  = B.cons 55 (B.singleton unusedArg)
encode INPLACE_SUBTRACT             = B.cons 56 (B.singleton unusedArg)
encode INPLACE_MULTIPLY             = B.cons 57 (B.singleton unusedArg)
encode INPLACE_MODULO               = B.cons 59 (B.singleton unusedArg)
encode STORE_SUBSCR                 = B.cons 60 (B.singleton unusedArg)
encode DELETE_SUBSCR                = B.cons 61 (B.singleton unusedArg)
encode BINARY_LSHIFT                = B.cons 62 (B.singleton unusedArg)
encode BINARY_RSHIFT                = B.cons 63 (B.singleton unusedArg)
encode BINARY_AND                   = B.cons 64 (B.singleton unusedArg)
encode BINARY_XOR                   = B.cons 65 (B.singleton unusedArg)
encode BINARY_OR                    = B.cons 66 (B.singleton unusedArg)
encode INPLACE_POWER                = B.cons 67 (B.singleton unusedArg)
encode GET_ITER                     = B.cons 68 (B.singleton unusedArg)
encode GET_YIELD_FROM_ITER          = B.cons 69 (B.singleton unusedArg)
encode PRINT_EXPR                   = B.cons 70 (B.singleton unusedArg)
encode LOAD_BUILD_CLASS             = B.cons 71 (B.singleton unusedArg)
encode YIELD_FROM                   = B.cons 72 (B.singleton unusedArg)
encode GET_AWAITABLE                = B.cons 73 (B.singleton unusedArg)
encode INPLACE_LSHIFT               = B.cons 75 (B.singleton unusedArg)
encode INPLACE_RSHIFT               = B.cons 76 (B.singleton unusedArg)
encode INPLACE_AND                  = B.cons 77 (B.singleton unusedArg)
encode INPLACE_XOR                  = B.cons 78 (B.singleton unusedArg)
encode INPLACE_OR                   = B.cons 79 (B.singleton unusedArg)
encode BREAK_LOOP                   = B.cons 80 (B.singleton unusedArg)
encode WITH_CLEANUP_START           = B.cons 81 (B.singleton unusedArg)
encode WITH_CLEANUP_FINISH          = B.cons 82 (B.singleton unusedArg)
encode RETURN_VALUE                 = B.cons 83 (B.singleton unusedArg)
encode IMPORT_STAR                  = B.cons 84 (B.singleton unusedArg)
encode SETUP_ANNOTATIONS            = B.cons 85 (B.singleton unusedArg)
encode YIELD_VALUE                  = B.cons 86 (B.singleton unusedArg)
encode POP_BLOCK                    = B.cons 87 (B.singleton unusedArg)
encode END_FINALLY                  = B.cons 88 (B.singleton unusedArg)
encode POP_EXCEPT                   = B.cons 89 (B.singleton unusedArg)
encode (STORE_NAME x)               = B.cons 90 (B.singleton x)
encode DELETE_NAME                  = B.cons 91 (B.singleton unusedArg)
encode UNPACK_SEQUENCE              = B.cons 92 (B.singleton unusedArg)
encode FOR_ITER                     = B.cons 93 (B.singleton unusedArg)
encode UNPACK_EX                    = B.cons 94 (B.singleton unusedArg)
encode STORE_ATTR                   = B.cons 95 (B.singleton unusedArg)
encode DELETE_ATTR                  = B.cons 96 (B.singleton unusedArg)
encode STORE_GLOBAL                 = B.cons 97 (B.singleton unusedArg)
encode DELETE_GLOBAL                = B.cons 98 (B.singleton unusedArg)
encode (LOAD_CONST x)               = B.cons 100 (B.singleton x)
encode (LOAD_NAME x)                = B.cons 101 (B.singleton x)
encode BUILD_TUPLE                  = B.cons 102 (B.singleton unusedArg)
encode BUILD_LIST                   = B.cons 103 (B.singleton unusedArg)
encode BUILD_SET                    = B.cons 104 (B.singleton unusedArg)
encode BUILD_MAP                    = B.cons 105 (B.singleton unusedArg)
encode LOAD_ATTR                    = B.cons 106 (B.singleton unusedArg)
encode COMPARE_OP                   = B.cons 107 (B.singleton unusedArg)
encode IMPORT_NAME                  = B.cons 108 (B.singleton unusedArg)
encode IMPORT_FROM                  = B.cons 109 (B.singleton unusedArg)
encode JUMP_FORWARD                 = B.cons 110 (B.singleton unusedArg)
encode JUMP_IF_FALSE_OR_POP         = B.cons 111 (B.singleton unusedArg)
encode JUMP_IF_TRUE_OR_POP          = B.cons 112 (B.singleton unusedArg)
encode JUMP_ABSOLUTE                = B.cons 113 (B.singleton unusedArg)
encode POP_JUMP_IF_FALSE            = B.cons 114 (B.singleton unusedArg)
encode POP_JUMP_IF_TRUE             = B.cons 115 (B.singleton unusedArg)
encode LOAD_GLOBAL                  = B.cons 116 (B.singleton unusedArg)
encode CONTINUE_LOOP                = B.cons 119 (B.singleton unusedArg)
encode SETUP_LOOP                   = B.cons 120 (B.singleton unusedArg)
encode SETUP_EXCEPT                 = B.cons 121 (B.singleton unusedArg)
encode SETUP_FINALLY                = B.cons 122 (B.singleton unusedArg)
encode LOAD_FAST                    = B.cons 124 (B.singleton unusedArg)
encode STORE_FAST                   = B.cons 125 (B.singleton unusedArg)
encode DELETE_FAST                  = B.cons 126 (B.singleton unusedArg)
encode STORE_ANNOTATION             = B.cons 127 (B.singleton unusedArg)
encode RAISE_VARARGS                = B.cons 130 (B.singleton unusedArg)
encode (CALL_FUNCTION x)            = B.cons 131 (B.singleton x)
encode MAKE_FUNCTION                = B.cons 132 (B.singleton unusedArg)
encode BUILD_SLICE                  = B.cons 133 (B.singleton unusedArg)
encode LOAD_CLOSURE                 = B.cons 135 (B.singleton unusedArg)
encode LOAD_DEREF                   = B.cons 136 (B.singleton unusedArg)
encode STORE_DEREF                  = B.cons 137 (B.singleton unusedArg)
encode DELETE_DEREF                 = B.cons 138 (B.singleton unusedArg)
encode CALL_FUNCTION_KW             = B.cons 141 (B.singleton unusedArg)
encode CALL_FUNCTION_EX             = B.cons 142 (B.singleton unusedArg)
encode SETUP_WITH                   = B.cons 143 (B.singleton unusedArg)
encode EXTENDED_ARG                 = B.cons 144 (B.singleton unusedArg)
encode LIST_APPEND                  = B.cons 145 (B.singleton unusedArg)
encode SET_ADD                      = B.cons 146 (B.singleton unusedArg)
encode MAP_ADD                      = B.cons 147 (B.singleton unusedArg)
encode LOAD_CLASSDEREF              = B.cons 148 (B.singleton unusedArg)
encode BUILD_LIST_UNPACK            = B.cons 149 (B.singleton unusedArg)
encode BUILD_MAP_UNPACK             = B.cons 150 (B.singleton unusedArg)
encode BUILD_MAP_UNPACK_WITH_CALL   = B.cons 151 (B.singleton unusedArg)
encode BUILD_TUPLE_UNPACK           = B.cons 152 (B.singleton unusedArg)
encode BUILD_SET_UNPACK             = B.cons 153 (B.singleton unusedArg)
encode SETUP_ASYNC_WITH             = B.cons 154 (B.singleton unusedArg)
encode FORMAT_VALUE                 = B.cons 155 (B.singleton unusedArg)
encode BUILD_CONST_KEY_MAP          = B.cons 156 (B.singleton unusedArg)
encode BUILD_STRING                 = B.cons 157 (B.singleton unusedArg)
encode BUILD_TUPLE_UNPACK_WITH_CALL = B.cons 158 (B.singleton unusedArg)
