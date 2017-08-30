module TestAlgW where

import SyntaxMPLC
import TestALL as ALL
-- import TypeChecker as TC
import TypeCheckerUnParam
import EvalMPLC 
import Data.Maybe




checkTy :: (TestList, Type) ->  Bool
checkTy ((a1, a2, a3, a4, a5, a6, a7, a8), t1) = case typefromSubType $ algW (genEnv $ evalCT a2) of 
    ty1 | ty1 == t1 -> False
    otherwise       -> True


filterAllTy :: [(TestList, Type)] -> [(String, Expr, Type, Type)]
filterAllTy (x:xs)  = map (\((a1, a2, a3, a4, a5, a6, a7, a8), t1) -> (a8, a1, (typefromSubType $ algW (genEnv $ evalCT a2)), t1)) $ filter checkTy (x:xs)
    
filtTyAll = filterAllTy ALL.typedAll
    



-- testExprP :: [(String, Expr, SubType)]
-- testExprP = map (\(a1, a2, a3, a4, a5, a6, a7, a8)-> (a8, a1, (TC.algWExp (evalCT a1)))) ALL.tAll

-- testExprUP :: [(String, Expr, SubType)]
-- testExprUP = map (\(a1, a2, a3, a4, a5, a6, a7, a8)-> (a8, a1, (TCUP.algWExp a1))) ALL.tAll



-- testExprP' :: [(String, Expr, Type)]
-- testExprP' = map (\(a1, a2, a3, a4, a5, a6, a7, a8)-> (a8, a1, typefromSubType (TC.algWExp (evalCT a1)))) ALL.tAll

-- testExprUP' :: [(String, Expr, Type)]
-- testExprUP' = map (\(a1, a2, a3, a4, a5, a6, a7, a8)-> (a8, a1, typefromSubType (TCUP.algWExp a1))) ALL.tAll



-- testExprP'' :: [(String, Expr, Type)]
-- testExprP'' = map (\(a1, a2, a3, a4, a5, a6, a7, a8)-> (a8, tyEvalP a1, typefromSubType (TC.algWExp (evalCT (tyEvalP a1))))) ALL.tAll

-- testExprUP'' :: [(String, Expr, Type)]
-- testExprUP'' = map (\(a1, a2, a3, a4, a5, a6, a7, a8)-> (a8, tyEvalUP a1, typefromSubType (TCUP.algWExp (tyEvalUP a1)))) ALL.tAll


-- testExprCTUP'' :: [(String, Expr, Type)]
-- testExprCTUP'' = map (\(a1, a2, a3, a4, a5, a6, a7, a8)-> (a8, tyEvalUP (evalCT a1), typefromSubType (TCUP.algWExp (tyEvalUP (evalCT a1))))) ALL.tAll


-- checkExprP ::[(String, (String, Bool), (String, Bool), (String, Bool), (String, Bool))]
-- checkExprP = map (\(a1, a2, a3, a4, a5, a6, a7, a8) ->                           (a8, ("CT", ((typefromSubType (TC.algWExp a1)) == (typefromSubType (TC.algWExp a2)))),  ("RT", ((typefromSubType (TC.algWExp a1)) == (typefromSubType (TC.algWExp a3)))),  ("CTRT", ((typefromSubType (TC.algWExp a1)) == (typefromSubType (TC.algWExp a4)))), ("UL", ((TyCode (typefromSubType (TC.algWExp a1))) == (typefromSubType (TC.algWExp a6)))))) ALL.tAll

-- checkExprUP ::[(String, (String, Bool), (String, Bool), (String, Bool), (String, Bool))]
-- checkExprUP = map (\(a1, a2, a3, a4, a5, a6, a7, a8) ->                           (a8, ("CT", ((typefromSubType (TCUP.algWExp a1)) == (typefromSubType (TCUP.algWExp a2)))),  ("RT", ((typefromSubType (TCUP.algWExp a1)) == (typefromSubType (TCUP.algWExp a3)))),  ("CTRT", ((typefromSubType (TCUP.algWExp a1)) == (typefromSubType (TCUP.algWExp a4)))), ("UL", ((TyCodeUnP) == (typefromSubType (TCUP.algWExp a6)))))) ALL.tAll


 -- (AST [TagExpr (TPrimBin Add),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]],Just [("x",TyInt)],2)
 
 -- (AST [TagExpr (TPrimBin Add),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]],Just [("x",TyVarRep "Type1{x}")],2)

 
typefromSubType :: SubType -> Type
typefromSubType Nothing            = TyErrorEq
typefromSubType (Just (s, t, n))   = t
 