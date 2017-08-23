module TestAlgW where

import SyntaxMPLC
import TestALL as ALL
import TypeChecker as TC
import TypeCheckerUnParam as TCUP
import EvalMPLC 
import Data.Maybe

eqTy :: Type -> Type -> Bool
eqTy (TyVar n1) (TyVar n2) 
    | n1 == n2          = True
    | otherwise         = False
eqTy (TyVarRep n1) (TyVarRep n2) 
    | n1 == n2          = True
    | otherwise         = False
eqTy TyInt TyInt        = True
eqTy TyBool TyBool      = True
eqTy (TyFunc t11 t12) (TyFunc t21 t22) = (eqTy t11 t21) && (eqTy t12 t22)
eqTy (TyCode t1) t2     = case t2 of
    TyCodeUnP           -> True
    TyCode t3           -> eqTy t1 t3
    otherwise           -> False
eqTy TyCodeUnP t2       = case t2 of
    TyCodeUnP           -> True
    TyCode t3           -> True
    otherwise           -> False
eqTy TyTag TyTag        = True
eqTy TyGenSym TyGenSym  = True
eqTy TyErrorEq t1       = case t1 of
    TyErrorEq           -> True
    TyError e1 t21 t22  -> True
    TyErrorLoop         -> True
    TyErrorUnify t21 t22  -> True
    otherwise           -> False
eqTy (TyError e1 t21 t22) t1 = case t1 of
    TyErrorEq           -> True
    TyError e1 t21 t22  -> True
    TyErrorLoop         -> True
    TyErrorUnify t21 t22  -> True
    otherwise           -> False    
eqTy (TyErrorLoop) t1   = case t1 of
    TyErrorEq           -> True
    TyError e1 t21 t22  -> True
    TyErrorLoop         -> True
    TyErrorUnify t21 t22  -> True
    otherwise           -> False    
eqTy (TyErrorUnify t11 t12) t1 = case t1 of
    TyErrorEq           -> True
    TyError e1 t21 t22  -> True
    TyErrorLoop         -> True
    TyErrorUnify t21 t22  -> True
    otherwise           -> False    
eqTy a1 a2              = False


tyEvalP :: Expr -> Expr
tyEvalP (Var n1)            = Var n1
tyEvalP (Lam n1 e1)         = (Lam n1 (tyEvalP e1))
tyEvalP (App e1 e2)         = (App (tyEvalP e1) (tyEvalP e2))
tyEvalP (LitN n)            = (LitN n)
tyEvalP (LitB n)            = (LitB n)
tyEvalP (PrimUni uOp e1)    = (PrimUni uOp (tyEvalP e1))
tyEvalP (PrimBin bOp e1 e2) = (PrimBin bOp (tyEvalP e1) (tyEvalP e2))
tyEvalP (If e1 e2 e3)       = (If (tyEvalP e1) (tyEvalP e2) (tyEvalP e3))
tyEvalP (Let n1 e2 e3)      = (Let n1 (tyEvalP e2) (tyEvalP e3))
tyEvalP (LetRec n1 e2 e3)   = (LetRec n1 (tyEvalP e2) (tyEvalP e3))
tyEvalP (AST xlist)         = (AST (map tyEvalP xlist))
tyEvalP (VarRep n1)         = (VarRep n1)
tyEvalP (GenSym)            = (GenSym)
tyEvalP (DownA e1)          = (DownA (tyEvalP e1))
tyEvalP (UpA e1)            = (UpA (tyEvalP e1))
tyEvalP (Eval exp1) 
    | (isJust (TC.algWExp exp1))= (EvalA t1 exp1)
    | otherwise                 = (EvalA TyErrorEq exp1)
    where Just (s1, t1, n1)     = TC.algWExp exp1
tyEvalP (EvalA t1 e1)          = (EvalA t1 (tyEvalP e1))    
tyEvalP (LetDA n1 e2 e3)    = (LetDA n1 (tyEvalP e2) (tyEvalP e3))
tyEvalP (TagExpr t1)        = (TagExpr t1)

tyEvalUP :: Expr -> Expr
tyEvalUP (Var n1)            = Var n1
tyEvalUP (Lam n1 e1)         = (Lam n1 (tyEvalUP e1))
tyEvalUP (App e1 e2)         = (App (tyEvalUP e1) (tyEvalUP e2))
tyEvalUP (LitN n)            = (LitN n)
tyEvalUP (LitB n)            = (LitB n)
tyEvalUP (PrimUni uOp e1)    = (PrimUni uOp (tyEvalUP e1))
tyEvalUP (PrimBin bOp e1 e2) = (PrimBin bOp (tyEvalUP e1) (tyEvalUP e2))
tyEvalUP (If e1 e2 e3)       = (If (tyEvalUP e1) (tyEvalUP e2) (tyEvalUP e3))
tyEvalUP (Let n1 e2 e3)      = (Let n1 (tyEvalUP e2) (tyEvalUP e3))
tyEvalUP (LetRec n1 e2 e3)   = (LetRec n1 (tyEvalUP e2) (tyEvalUP e3))
tyEvalUP (AST xlist)         = (AST (map tyEvalUP xlist))
tyEvalUP (VarRep n1)         = (VarRep n1)
tyEvalUP (GenSym)            = (GenSym)
tyEvalUP (DownA e1)          = (DownA (tyEvalUP e1))
tyEvalUP (UpA e1)            = (UpA (tyEvalUP e1))
tyEvalUP (Eval exp1) 
    | (isJust (TC.algWExp exp1))= (EvalA t1 exp1)
    | otherwise                 = (EvalA TyErrorEq exp1)
    where Just (s1, t1, n1)     = TCUP.algWExp exp1
tyEvalUP (EvalA t1 e1)          = (EvalA t1 (tyEvalUP e1))    
tyEvalUP (LetDA n1 e2 e3)    = (LetDA n1 (tyEvalUP e2) (tyEvalUP e3))
tyEvalUP (TagExpr t1)        = (TagExpr t1)



filterNotEq :: [((String, Expr, Type), (String, Expr, Type))]
filterNotEq = filter (\((nm1, ex1, t1), (nm2, ex2, t2)) -> not $ eqTy t1 t2) (zip testExprP'' testExprUP'')




testExprP :: [(String, Expr, SubType)]
testExprP = map (\(a1, a2, a3, a4, a5, a6, a7, a8)-> (a8, a1, (TC.algWExp (evalCT a1)))) ALL.tAll

testExprUP :: [(String, Expr, SubType)]
testExprUP = map (\(a1, a2, a3, a4, a5, a6, a7, a8)-> (a8, a1, (TCUP.algWExp a1))) ALL.tAll



testExprP' :: [(String, Expr, Type)]
testExprP' = map (\(a1, a2, a3, a4, a5, a6, a7, a8)-> (a8, a1, typefromSubType (TC.algWExp (evalCT a1)))) ALL.tAll

testExprUP' :: [(String, Expr, Type)]
testExprUP' = map (\(a1, a2, a3, a4, a5, a6, a7, a8)-> (a8, a1, typefromSubType (TCUP.algWExp a1))) ALL.tAll



testExprP'' :: [(String, Expr, Type)]
testExprP'' = map (\(a1, a2, a3, a4, a5, a6, a7, a8)-> (a8, tyEvalP a1, typefromSubType (TC.algWExp (evalCT (tyEvalP a1))))) ALL.tAll

testExprUP'' :: [(String, Expr, Type)]
testExprUP'' = map (\(a1, a2, a3, a4, a5, a6, a7, a8)-> (a8, tyEvalUP a1, typefromSubType (TCUP.algWExp (tyEvalUP a1)))) ALL.tAll



checkExprP ::[(String, (String, Bool), (String, Bool), (String, Bool), (String, Bool))]
checkExprP = map (\(a1, a2, a3, a4, a5, a6, a7, a8) ->                           (a8, ("CT", ((typefromSubType (TC.algWExp a1)) == (typefromSubType (TC.algWExp a2)))),  ("RT", ((typefromSubType (TC.algWExp a1)) == (typefromSubType (TC.algWExp a3)))),  ("CTRT", ((typefromSubType (TC.algWExp a1)) == (typefromSubType (TC.algWExp a4)))), ("UL", ((TyCode (typefromSubType (TC.algWExp a1))) == (typefromSubType (TC.algWExp a6)))))) ALL.tAll

checkExprUP ::[(String, (String, Bool), (String, Bool), (String, Bool), (String, Bool))]
checkExprUP = map (\(a1, a2, a3, a4, a5, a6, a7, a8) ->                           (a8, ("CT", ((typefromSubType (TCUP.algWExp a1)) == (typefromSubType (TCUP.algWExp a2)))),  ("RT", ((typefromSubType (TCUP.algWExp a1)) == (typefromSubType (TCUP.algWExp a3)))),  ("CTRT", ((typefromSubType (TCUP.algWExp a1)) == (typefromSubType (TCUP.algWExp a4)))), ("UL", ((TyCodeUnP) == (typefromSubType (TCUP.algWExp a6)))))) ALL.tAll


 -- (AST [TagExpr (TPrimBin Add),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]],Just [("x",TyInt)],2)
 
 -- (AST [TagExpr (TPrimBin Add),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]],Just [("x",TyVarRep "Type1{x}")],2)

 
typefromSubType :: SubType -> Type
typefromSubType Nothing            = TyErrorEq
typefromSubType (Just (s, t, n))   = t
 