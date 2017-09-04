module TypeCheckerUnParam where

-- import EvalMPLC
import SyntaxMPLC
import Data.List
import Data.Maybe
import System.IO 
import qualified Data.Map as M
-- Check the type of an expression is valid, i.e the program does not have type errors

-- Takes in expression and 

typeExp :: Expr -> Maybe Type 
typeExp e = applyUnifyCon' $ genCon  e




genEnv :: Expr -> State
genEnv exp1 = genEnv' exp1 (exp1, M.empty, 1)
  
  

genEnv' :: Expr -> State -> State
genEnv' exp (st@(exp00, env1,n)) = case exp of
    (Var nm1)           -> 
        let newnm1      = TyVar ("TyVar" ++ show n ++ "{" ++ nm1 ++ "}") in 
        case M.lookup nm1 env1 of
            Nothing                 -> (exp00, M.insert nm1 newnm1 env1 , n + 1)
            Just (t1)               -> st
    (Lam nm1 exp1)              -> foldr genEnv' st [exp1, (Var nm1)]
    (App exp1 exp2)             -> foldr genEnv' st [exp2, exp1]
    (LitB bool1)                -> st
    (LitN numb1)                -> st
    (PrimUni uOp1 exp1)         -> genEnv' exp1 st
    (PrimBin bOp1 exp1 exp2)    -> foldr genEnv' st [exp2, exp1]
    (If bool1 exp1 exp2)        -> foldr genEnv' st [exp2, exp1, bool1]
    (Let nm1 exp1 exp2)         -> foldr genEnv' st [exp2, exp1, (Var nm1)]
    (LetRec nm1 exp1 exp2)      -> foldr genEnv' st [exp2, exp1, (Var nm1)]
    (AST list1)                 -> foldr genEnv' st list1
    (VarRep nm1)                -> st
    GenSym                      -> st
    (DownA exp1)                -> genEnv' exp1 st
    (UpA exp1)                  -> genEnv' exp1 st
    (EvalA t1 exp1)             -> genEnv' exp1 st
    (LetDA name1 exp1 exp2)     -> foldr genEnv' st [exp2, exp1, (Var name1)]
    (TagExpr tag1)              -> st
    (Error exp1)                -> error ("error in term: " ++ show exp)
    (ErrorLoop exp1)            -> error ("error in term: " ++ show exp)

genCon :: Expr -> (Constraint, Type, FreshCounter)
genCon e1 = genCon' e1 (genEnv e1)
    
genCon' :: Expr -> State -> (Constraint, Type, FreshCounter)
genCon' exp (st@(exp00, env, n)) = case exp of
    (Var nm1)                   -> ([], env M.! nm1, n)
    (Lam nm1 e1) -> 
        let t = freshType n 
            (ce1, te1, n') = genCon' e1 (exp00, (M.insert nm1 t env), n+1)
        in (ce1, TyFunc t te1, n')
    (App e1 e2) -> 
        let ([(ce1, te1), (ce2, te2)], n') = genConL [e1, e2] st
            t = freshType n' 
        in                      (ce1 ++ ce2 ++ [(te1, TyFunc te2 t)], t, n' +1)
    (LitB bl1)                  -> ([], TyBool, n)
    (LitN nmb1)                 -> ([], TyInt, n)
    (PrimUni uOp1 e1) -> 
        let (ce1, te1, n') = genCon' e1 st
        in case elem uOp1 uniOpBool of 
            True                -> (ce1 ++ [(te1, TyBool)], TyBool, n')
            False               -> error ("no such unary operation" ++ show uOp1)
    (PrimBin bOp1 e1 e2) -> 
        let ([(ce1, te1), (ce2, te2)], n') = genConL [e1, e2] st
        in case elem bOp1 binOpBool of 
            True                            -> (ce1 ++ ce2 ++ [(te1, TyBool), (te2, TyBool)], TyBool, n')
            False | elem bOp1 binOpIntBool  -> (ce1 ++ ce2 ++ [(te1, TyInt), (te2, TyInt)], TyBool, n')
            False | elem bOp1 binOpIntInt   -> (ce1 ++ ce2 ++ [(te1, TyInt), (te2, TyInt)], TyInt, n')
            False               -> error ("no such binary operation" ++ show bOp1)
    (If b1 e1 e2) -> 
        let ([(cb1, tb1), (ce1, te1), (ce2, te2)], n') = genConL [b1, e1, e2] st
        in  (cb1 ++ ce1 ++ ce2 ++ [(tb1, TyBool), (te1, te2)], te1, n')
        
    (Let nm1 e1 e2) ->
        let ([(ce1, te1), (ce2, te2)], n') = genConL [e1, e2] st
        in  (ce1 ++ ce2 ++ [(env M.! nm1, te1)], te2, n')
        
    (LetRec nm1 e1 e2)      ->  
        let ([(ce1, te1), (ce2, te2)], n') = genConL [e1, e2] st
        in  (ce1 ++ ce2 ++ [(env M.! nm1, te1)], te2, n')    
    (AST list1)                 ->
        let (list1', n') = genConL list1 st
            clist1' = concat $ fst $ unzip list1' ------ ????????????????
        in  (clist1', TyCode, n')    
    (VarRep nm1)                -> ([], TyVarRep, n)
    GenSym                      -> ([], TyVarRep, n)
    (DownA e1) -> 
        let (ce1, te1, n') = genCon' e1 st 
        in                      (ce1 ++ [(te1, TyCode)], TyErrorExpr exp, n')    
    (UpA e1) -> 
        let (ce1, te1, n') = genCon' e1 st 
        in                      (ce1, TyErrorExpr exp, n')
    (EvalA t1 e1) -> 
        let (ce1, te1, n') = genCon' e1 st 
        in                      (ce1 ++ [(te1, TyCode)], t1, n')
    (LetDA nm1 e1 e2)     -> 
        let ([(ce1, te1), (ce2, te2)], n') = genConL [e1, e2] st
        in  (ce1 ++ ce2 ++ [(env M.! nm1, te1)], TyError exp te2 te2, n')   
    (TagExpr tag1)              -> ([], TyTag, n)
    (Error e1)                -> ([], TyErrorExpr exp, n)
    (ErrorLoop e1)            -> ([], TyErrorExpr exp, n)
    
genConL :: [Expr] -> State -> ([(Constraint, Type)], FreshCounter)
genConL [] st@(exp00, env, n) = ([], n)
genConL (e1:xs) st@(exp00, env, n) =
    let (ce1, te1, n')  = genCon' e1 st
        (ctL, n'')      = genConL xs (exp00, env, n')
    in  ([(ce1, te1)] ++ ctL, n'') 

    
applyUnifyCon :: (Constraint, Type, FreshCounter) -> Maybe (Type, FreshCounter)
applyUnifyCon (c, t, n) = case unifyCon c of
    Just s          -> Just (subType (Just s) t, n)
    Nothing         -> Nothing
    
applyUnifyCon' :: (Constraint, Type, FreshCounter) -> Maybe (Type)
applyUnifyCon' (c, t, n) = case unifyCon c of
    Just s          -> Just (subType (Just s) t)
    Nothing         -> Nothing
    
unifyCon :: Constraint -> Maybe [(Type, Name)] --Sub
unifyCon constr = case constr of
    []                      -> Just []
    ((t1, t2):rest) -> case (t1, t2) of
        (TyInt, TyInt)              -> unifyCon rest
        (TyBool, TyBool)            -> unifyCon rest
        (TyVarRep, TyVarRep)        -> unifyCon rest
        (TyFunc t11 t12, TyFunc t21 t22) -> unifyCon (rest ++ [(t11, t21), (t12, t22)])
        (TyVar x, t)                -> case t of 
            TyVar x' | x == x'      -> unifyCon rest
            otherwise               -> case occursIn x t of
                True                    -> Nothing
                False                   -> case unifyCon (subCon (Just [(t, x)]) rest) of 
                    Nothing         -> Nothing
                    (Just subRest)  -> Just (subRest ++ [(t, x)])
        (t, TyVar x)                -> case t of 
            TyVar x' | x == x'      -> unifyCon rest
            otherwise               -> case occursIn x t of
                True                    -> Nothing
                False                   -> case unifyCon (subCon (Just [(t, x)]) rest) of 
                    Nothing         -> Nothing
                    (Just subRest)  -> Just (subRest ++ [(t, x)])             
        (TyCode, TyCode)      -> unifyCon rest
        (TyTag, TyTag)              -> unifyCon rest
        -- (t, TyVar x)                -> case occursIn x t of
            -- True                    -> Nothing
            -- False                   -> case unifyCon (subCon (Just [(t, x)]) rest) of 
                -- Nothing         -> Nothing
                -- (Just subRest)  -> Just (subRest ++ [(t, x)])
        other | areError [t1, t2]   -> Nothing
        otherwise                   -> Nothing
        
occursIn :: Name -> Type -> Bool
occursIn x t = case t of
    (TyVar nm1) | x == nm1  -> True
    (TyVar nm1) | x /= nm1  -> False
    TyFunc t1 t2            -> occursIn x t1 || occursIn x t2
    otherwise               -> False
    
    
    
freshType :: Int -> Type
freshType n = (TyVar ("TyVar" ++ show n++"{fresh}"))    

freshVarRepName :: Int -> Name
freshVarRepName n = ("freshGenSym"++ "{" ++ show n ++ "}")

    
    