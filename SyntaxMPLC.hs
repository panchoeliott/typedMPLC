--from: http://dev.stephendiehl.com/fun/005_evaluation.html
--Syntax:
--Based on de bruijn notation version of Lambda calculus.
module SyntaxMPLC where

-- import Test
import Data.List
import Data.Maybe

type Name = String


data Expr = Var Name 
    | Lam Name Expr
    | App Expr Expr
    | LitN Int
    | LitB Bool
    | PrimUni UniOp Expr
    | PrimBin BinOp Expr Expr
    | If Expr Expr Expr 
    | Let Name Expr Expr
    | LetRec Name Expr Expr
    --AST expression:
    | AST [Expr]
    | VarRep Name
    | GenSym
    -- | IntRep Int    --these are not used yet
    -- | BoolRep Bool  --these are not used yet
    --up and down arrow (meta-level):
    | DownA Expr
    | UpA Expr
    --eval:
    | Eval Expr
    | EvalA Type Expr
    --lift:
    -- | Lift Expr
    --LetDA x = M in N:
    | LetDA Name Expr Expr
    --Tag:
    | TagExpr Tag
    --error:
    | Error Expr
    | ErrorLoop Expr
    | ErrorType Expr Type
    deriving (Show, Eq) 
    

data Type = TyVar String
    | TyVarRep String
    | TyInt 
    | TyBool
    | TyFunc Type Type
    | TyCodeUnP
    | TyCode Type 
    -- | TyCodePower Int Type 
    | TyTag
    | TyGenSym --or TyVarRep ...?
    | TyErrorEq
    | TyError Expr Type Type -- expression1 has type1 but should have type2
    | TyErrorLoop
    | TyErrorUnify Type Type
    deriving (Show, Eq) 
    
type TypeExpr = (Expr, Type)    

type Env = Maybe [(Name, Type)]

-- type Env1 = Maybe [(Expr, Type)]

-- type Constraint = [(Expr, Type)]

-- type State = (Expr, Env, Constraint, Int) 

type State = (Expr, Env, Int) 

type Sub = Maybe [(Type, Name)]

type SubType = Maybe (Sub, Type, Int)
    
data Tag = TVarRep
    -- | TIntRep
    -- | TBoolRep
    | TVar 
    | TLam 
    | TApp 
    | TLitN 
    | TLitB 
    | TPrimUni UniOp 
    | TPrimBin BinOp 
    | TIf 
    | TLet 
    | TLetRec 
    | TGenSym
    | TPromote
    -- | TUpA (should not exist)
    -- | TDownA (should not exist)
    | TEval
    | TEvalA Type
    -- | TLift
    -- | TLetDA (should not exist)
    deriving (Show, Eq)    
    
data Eval = RTEval | CTEval | DownMLEval | UpMLEval
    deriving (Show, Eq)

data BinOp = Add | Mul | Min | Div | Eq | Lt | Gt | GtEq | Or | And
    deriving (Show, Eq)

    
binOpAll = binOpBool ++ binOpInt
binOpInt = binOpIntInt ++ binOpIntBool
binOpIntInt = [Add, Mul, Min, Div]
binOpIntBool = [Eq, Lt, Gt, GtEq]
binOpBool = [Or, And] --implies?
    
    
data UniOp = Not
    deriving (Show, Eq)
uniOpAll = uniOpBool ++ uniOpInt
uniOpBool = [Not]
uniOpInt = []
    
tagLength0 :: [Tag]
tagLength0 = [TGenSym]

tagLength1 :: [Tag]
tagLength1 = [TVarRep, TVar, TLitB, TLitN, TEval] ++ map TPrimUni uniOpAll

tagLength2 :: [Tag]
tagLength2 = [TLam, TApp] ++ map TPrimBin binOpAll

tagLength3 :: [Tag]
tagLength3 = [TIf, TLet, TLetRec]

tagLengthx :: [Tag]
tagLengthx = [TPromote]

    

type FreshCounter = Int

type TestTriple = (Expr, Expr, String)    
    
type TestList = (Expr, Expr, Expr, Expr, Expr, Expr, Expr, String)


-- Free Variables:
fv :: (Expr)-> [Name]
fv (VarRep string1) = []
fv (Var n) = [n]
fv (Lam n exp) = (delete n (fv exp))
fv (App a b) = union (fv a) (fv b)
fv (LitN n) = []
fv (LitB b) = []
fv (PrimUni uni a) = fv a
fv (PrimBin bin a b) = union (fv a) (fv b)
fv (If bool1 exp1 exp2) = union (fv bool1) (union (fv exp1) (fv exp2))
fv (Let name1 exp1 exp2) = union (fv exp1) (delete name1 (fv exp2))
fv (LetRec name1 exp1 exp2) = (delete name1 (union (fv exp1)  (fv exp2)))
fv (AST xs) = nub $ concatMap fv xs 
fv (GenSym) = []
fv (DownA exp1)= fv (exp1)
fv (UpA exp1) = fv (exp1)
fv (Eval exp1) = fv (exp1)
-- fv (Lift exp1) = fv (exp1)
fv (LetDA name1 exp1 exp2) = union (fv (exp1)) (delete name1 (fv (exp2)))

fv (TagExpr tag1) = []
fv (Error exp1)= []
fv (ErrorLoop exp1)= []


-- Length Of Expressions
lengthExpr :: (Expr)-> Int
lengthExpr (VarRep string1) = 1
lengthExpr (Var n) = 1
lengthExpr (Lam n exp) = 1 + (lengthExpr exp)
lengthExpr (App a b) =1 + (lengthExpr a) + (lengthExpr b)
lengthExpr (LitN n) = 1
lengthExpr (LitB b) = 1
lengthExpr (PrimUni uni a) = 1 + (lengthExpr a)
lengthExpr (PrimBin bin a b) = 1 + (lengthExpr a) + (lengthExpr b)
lengthExpr (If bool1 exp1 exp2) = (lengthExpr bool1)+ (lengthExpr exp1) +(lengthExpr exp2)
lengthExpr (Let name1 exp1 exp2) = 1+ (lengthExpr exp1) + (lengthExpr exp2)
lengthExpr (LetRec name1 exp1 exp2) = 1+  (lengthExpr exp1) + (lengthExpr exp2)
lengthExpr (AST xs) = 1 + (sum $ map lengthExpr xs)
lengthExpr GenSym = 1
lengthExpr (DownA exp1)= 1 + lengthExpr (exp1)
lengthExpr (UpA exp1) = 1 + lengthExpr (exp1)
lengthExpr (Eval exp1) = 1 + lengthExpr (exp1)
-- lengthExpr (Lift exp1) = lengthExpr (exp1)
lengthExpr (LetDA name1 exp1 exp2) = 1 +  (lengthExpr (exp1)) + (lengthExpr (exp2))
lengthExpr (TagExpr tag1) = 1
lengthExpr (Error exp1)= 1 + lengthExpr exp1
lengthExpr (ErrorLoop exp1)= 1 + lengthExpr exp1

 