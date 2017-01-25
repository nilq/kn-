{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import Data.Word
import Data.String
import Data.List
import Data.Function
import qualified Data.Map as Map

import Control.Monad.State
import Control.Applicative

import LLVM.General.AST
import LLVM.General.AST.Global
import qualified LLVM.General.AST as AST

import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.FloatingPointPredicate as FP

newtype LLVM a =
  LLVM { unLLVM :: State AST.Module a
       } deriving (Functor, Applicative, Monad, MonadState AST.Module)

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM =
  flip (execState . unLLVM)

emptyModule :: String -> AST.Module
emptyModule label =
  defaultModule { moduleName = label
                }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d]
                   }

define :: Type -> String -> [(Type, AST.Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults { name        = Name label
                                      , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
                                      , returnType  = retty
                                      , basicBlocks = []
                                      }

external :: Type -> String -> [(Type, Name)] -> LLVM ()
external retty label argtys =
  addDefn $ GlobalDefinition $ functionDefaults
    { name = Name label
    , parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
    , returnType = retty
    , basicBlocks = []
    }

double :: Type
double =
  FloatingPointType 64 IEEE

type Names =
  Map.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm, Map.insert nm 1 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix + 1) ns)

instance IsString Name where
  fromString =
    Name . fromString

type SymbolTable =
  [ ( String
    , Operand
    )
  ]

data CodegenState = CodegenState { currentBlock :: Name
                                 , blocks       :: Map.Map Name BlockState
                                 , symtab       :: SymbolTable
                                 , blockCount   :: Int
                                 , count        :: Word
                                 , names        :: Names
                                 } deriving Show

data BlockState = BlockState { idx   :: Int
                             , stack :: [Named Instruction]
                             , term  :: Maybe (Named Terminator)
                             } deriving Show

-- Codegen operations

newtype Codegen a = Codegen { runCodegen :: State CodegenState a
                            } deriving (Functor, Applicative, Monad, MonadState CodegenState)

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks =
  sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m =
  map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, (BlockState _ s t)) =
  BasicBlock l s (maketerm t)
    where
      maketerm (Just x) =
        x
      maketerm Nothing =
        error $ "Block is fucking up: " ++ (show l)

entryBlockName :: String
entryBlockName =
  "entry"

emptyBlock :: Int -> BlockState
emptyBlock i =
  BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen =
  CodegenState (Name entryBlockName) Map.empty [] 1 0 Map.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m =
  execState (runCodegen m) emptyCodegen

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i
                   }
  return $ i + 1

instr :: Instruction -> Codegen (Operand)
instr ins = do
  n <- fresh
  let ref = (UnName n)
  blk <- current
  let i = stack blk
  modifyBlock ( blk { stack = i ++ [ref := ins]
                    }
              )
  return $ local ref

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock ( blk { term = Just trm
                     }
               )
  return trm

-- Block state

entry :: Codegen Name
entry =
  gets currentBlock

addBlock :: String -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix <- gets blockCount
  nms <- gets names
  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms
  modify $ \s -> s { blocks = Map.insert (Name qname) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (Name qname)

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname
                   }
  return bname

getBlock :: Codegen Name
getBlock =
  gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s)
                   }

current :: Codegen BlockState
current = do
  c    <- gets currentBlock
  blks <- gets blocks

  case Map.lookup c blks of
    Just x  -> return x
    Nothing -> error $ "Fucked up non-existing block: " ++ show c

assign :: String -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = [(var, x)] ++ lcls
                   }

getvar :: String -> Codegen Operand
getvar var = do
  syms <- gets symtab

  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable is nowhere to be found: " ++ show var

-- References

local :: Name -> Operand
local =
  LocalReference double

global :: Name -> C.Constant
global =
  C.GlobalReference double

externf :: Name -> Operand
externf =
  ConstantOperand . C.GlobalReference double

fadd :: Operand -> Operand -> Codegen Operand
fadd a b =
  instr $ FAdd NoFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b =
  instr $ FSub NoFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b =
  instr $ FMul NoFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b =
  instr $ FDiv NoFastMathFlags a b []

fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp cond a b =
  instr $ FCmp cond a b []

cons :: C.Constant -> Operand
cons =
  ConstantOperand

uitofp :: Type -> Operand -> Codegen Operand
uitofp ty a =
  instr $ UIToFP a ty []

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs =
  map (\x -> (x, []))

call :: Operand -> [Operand] -> Codegen Operand
call fn args =
  instr $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: Type -> Codegen Operand
alloca ty =
  instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store ptr val =
  instr $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr =
  instr $ Load False ptr Nothing 0 []

br :: Name -> Codegen (Named Terminator)
br val =
  terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl =
  terminator $ Do $ CondBr cond tr fl []

ret :: Operand -> Codegen (Named Terminator)
ret val =
  terminator $ Do $ Ret (Just val) []
