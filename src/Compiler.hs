module Compiler where

import Control.Monad.State.Lazy
import Data.Int
import Data.List
import Data.Word

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B

import AST
import Parser
import CodeObject
import Opcode

type Compiler a b = a -> State CodeObject b

opname :: Compiler Opname ()
opname opname =
  modify (\co -> co { co_code = B.append (B.reverse $ encode opname) (co_code co) })

insertIn :: (Eq a, Integral b) => a -> [a] -> (b, [a])
insertIn x xs = case x `elemIndex` xs of
    Just idx -> (fromIntegral $ length xs - (idx + 1), xs)
    Nothing  -> (fromIntegral $ length xs, x : xs)

-- TODO: Smart stack size calculation
addStackSize :: Compiler Int32 ()
addStackSize n = modify (\co -> co { co_stacksize = n + co_stacksize co } )

-- TODO: Support LOAD_FAST
loadName :: Compiler String ()
loadName name = do
    co <- get
    let (idx, names) = name `insertIn` co_names co
    put $ co { co_names = names }
    addStackSize 1
    opname (LOAD_NAME idx)

storeName :: Compiler String ()
storeName name = do
    co <- get
    let (idx, names) = name `insertIn` co_names co
    put $ co { co_names = names }
    opname (STORE_NAME idx)

loadConst :: ToPyValue a => Compiler a ()
loadConst val = do
    co <- get
    let (idx, consts) = toPyValue val `insertIn` co_consts co
    put $ co { co_consts = consts }
    opname (LOAD_CONST idx)


compileAST :: Compiler AST ()

compileAST (Program xs) = mapM_ compileAST xs >> loadConst None >> opname RETURN_VALUE

compileAST (Assignment name rhs) = compileAST rhs >> storeName name

compileAST (Identifier name) = loadName name

compileAST ast@(Number _) = loadConst ast
compileAST ast@(String _) = loadConst ast
compileAST ast@None       = loadConst ast

compileAST (FunctionCall name args) = do
    compileAST name
    mapM_ compileAST args
    addStackSize (genericLength args)
    opname (CALL_FUNCTION (genericLength args))

compile :: String -> CodeObject
compile = cleanUp . flip execState co . compileAST . parse
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
                , co_filename       = "<module>"
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
