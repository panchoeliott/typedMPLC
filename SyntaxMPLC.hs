--from: http://dev.stephendiehl.com/fun/005_evaluation.html
--Syntax:
--Based on de bruijn notation version of Lambda calculus.
module SyntaxMPLC where

-- import Test
import Data.List
import Data.Maybe
import qualified Data.Map as M

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
    -- | Eval Expr
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
    | TyVarRep
    | TyInt 
    | TyBool
    | TyFunc Type Type
    | TyCodeUnP
    -- | TyCode Type 
    -- | TyCodePower Int Type 
    | TyTag
    | TyTBC
    | TyErrorEq
    | TyErrorExpr Expr
    | TyError Expr Type Type -- expression1 has type1 but should have type2
    | TyErrorLoop
    | TyErrorUnify Type Type
    deriving (Show, Eq) 
    
type TypeExpr = (Expr, Type)    

-- type Env = Maybe [(Name, Type)]


type Env = M.Map Name Type

-- type Env1 = Maybe [(Expr, Type)]

type Constraint = [(Type, Type)]

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
    -- | TEval
    | TEvalA Type
    -- | TLift
    -- | TLetDA (should not exist)
    deriving (Show, Eq)    
    
-- data Eval = RTEval | CTEval | DownMLEval | UpMLEval
    -- deriving (Show, Eq)

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
tagLength1 = [TVarRep, TVar, TLitB, TLitN] ++ map TPrimUni uniOpAll

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
fv (EvalA t exp1) = fv (exp1)
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
lengthExpr (EvalA t exp1) = 1 + lengthExpr (exp1)
-- lengthExpr (Lift exp1) = lengthExpr (exp1)
lengthExpr (LetDA name1 exp1 exp2) = 1 +  (lengthExpr (exp1)) + (lengthExpr (exp2))
lengthExpr (TagExpr tag1) = 1
lengthExpr (Error exp1)= 1 + lengthExpr exp1
lengthExpr (ErrorLoop exp1)= 1 + lengthExpr exp1


hasError :: (Expr)-> Bool
hasError (VarRep string1)   = False
hasError (Var n)            = False
hasError (Lam n exp)        = hasError exp
hasError (App a b)          = (hasError a) || (hasError b)
hasError (LitN n)           = False
hasError (LitB b)           = False
hasError (PrimUni uni a)    = (hasError a)
hasError (PrimBin bin a b)  = (hasError a) || (hasError b)
hasError (If bool1 exp1 exp2)       = (hasError bool1) || (hasError exp1) || (hasError exp2)
hasError (Let name1 exp1 exp2)      = (hasError exp1) || (hasError exp2)
hasError (LetRec name1 exp1 exp2)   = (hasError exp1) || (hasError exp2)
hasError (AST xs)           = and $ map hasError xs
hasError GenSym             = False
hasError (DownA exp1)       = hasError (exp1)
hasError (UpA exp1)         = hasError (exp1)
-- hasError (Eval exp1)        = hasError (exp1)
hasError (EvalA type1 exp1)  = hasError (exp1) || (containsError type1)
-- hasError (Lift exp1) = hasError (exp1)
hasError (LetDA name1 exp1 exp2)    = (hasError exp1) || (hasError exp2)
hasError (TagExpr tag1)     = False
hasError (Error exp1)       = True
hasError (ErrorLoop exp1)   = True
hasError (ErrorType exp1 t1)   = True

    
containsError :: Type -> Bool
containsError (TyVar st1)       = False
containsError (TyVarRep)        = False
containsError TyInt             = False
containsError TyBool            = False
containsError (TyFunc t1 t2)    = containsError t1 || containsError t2
containsError (TyCodeUnP)       = False
containsError TyTag             = False
containsError (TyErrorExpr e1)  = True
containsError TyErrorEq         = True
containsError (TyError e1 t1 t2)= True
containsError TyErrorLoop       = True
containsError (TyErrorUnify t1 t2) = True

areError :: [Type] -> Bool
areError [] = False
areError (x:xs) = (containsError x) || areError xs

compiled :: Expr -> Bool
compiled exp1 = case exp1 of
    VarRep n1               -> True
    Var n1                  -> True
    Lam n1 e1               -> compiled e1
    App e1 e2               -> compiled e1 || compiled e2
    LitN int1               -> True
    LitB bool1              -> True
    PrimUni uniOp1 e1       -> compiled e1
    PrimBin binOp1 e1 e2    -> compiled e1 || compiled e2
    If e1 e2 e3             -> compiled e1 || compiled e2 || compiled e3
    Let n1 e1 e2            -> compiled e1 || compiled e2
    LetRec n1 e1 e2         -> compiled e1 || compiled e2
    AST exprL               -> and $ map compiled  exprL
    DownA e1                -> False
    UpA e1                  -> False
    EvalA t1 e1             -> False
    LetDA name1 e1 e2       -> False
    TagExpr tag1            -> True
    Error e1                -> False
 
 
 --Substitute type1 for type2 in type3 = type3 [type1/type2]          
subType :: Sub -> Type -> Type
subType Nothing a                                   = a
subType (Just []) a                                 = a
subType (Just [(ts1, name1)]) TyInt                 = TyInt
subType (Just [(ts1, name1)]) TyBool                = TyBool
subType (Just [(ts1, name1)]) (TyVar var1)
    | (name1 == var1)                               = ts1
    | otherwise                                     = (TyVar var1)
subType (Just [(ts1, name1)]) TyVarRep              = TyVarRep
subType (Just [(ts1, name1)]) (TyFunc type1 type2)  = TyFunc (subType (Just [(ts1, name1)]) type1) (subType (Just [(ts1, name1)]) type2)
subType (Just [(ts1, name1)]) (TyCodeUnP)           = TyCodeUnP
subType (Just [(ts1, name1)]) (TyTag)               = TyTag
subType sub1 (TyErrorEq)                            = TyErrorEq
subType sub1 (TyError a b c)                        = TyError a b c
subType sub1 (TyErrorLoop)                          = TyErrorLoop

subType (Just (x:xs)) type1                         = subType (Just [x]) (subType (Just xs) type1)

subCon :: Sub -> Constraint -> Constraint
subCon s [] = []
subCon s ((t1,t2):xs) = ((subType s t1, subType s t2): subCon s xs)