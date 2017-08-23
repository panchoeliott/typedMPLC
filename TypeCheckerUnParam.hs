module TypeCheckerUnParam where

import EvalMPLC
import SyntaxMPLC
import Data.List
import Data.Maybe
import System.IO 

-- Check the type of an expression is valid, i.e the program does not have type errors

-- Takes in expression and 


genEnv :: Expr -> (State)
genEnv exp1 = genEnv' exp1' st0
    where st0@(exp1', env, n) = replaceGenSym (exp1, Just [], 1)
  

genEnv' :: Expr -> State -> State
-- genEnv' exp (exp00, Nothing, [],n) = (exp00, Nothing, [],n)
genEnv' (Var name1) (exp00, env1, n) = case env1 of
    Nothing     -> (exp00, Nothing, n)
    Just []     -> (exp00, Just [(name1, TyVar ("Type" ++ show n ++ "{" ++ name1 ++ "}"))], n+1)
    otherwise   -> case name1 `isNameInEnv` env1 of
        True    -> (exp00, env1,n)
        False   -> (exp00, (++) <$> (Just [(name1, TyVar ("Type" ++ show n ++ "{" ++ name1 ++ "}"))]) <*>  env1, n+1)
genEnv' (Lam name1 exp1) (exp00, env1,n)          =  foldr genEnv' (exp00, env1, n) [exp1, (Var name1)]
genEnv' (App exp1 exp2)  (exp00, env1, n)          =  foldr genEnv' (exp00, env1, n) [exp2, exp1]
genEnv' (LitB bool1) (exp00, env1, n)              = (exp00, env1, n)
genEnv' (LitN numb1) (exp00, env1,n)              = (exp00, env1, n)
genEnv' (PrimUni uOp1 exp1) (exp00, env1,n)       = genEnv' exp1 (exp00, env1, n)
genEnv' (PrimBin bOp1 exp1 exp2) (exp00, env1, n)  =  foldr genEnv' (exp00, env1, n) [exp2, exp1]
genEnv' (If bool1 exp1 exp2) (exp00, env1, n)      =  foldr genEnv' (exp00, env1,n) [exp2, exp1, bool1]
genEnv' (Let name1 exp1 exp2) (exp00, env1, n)     = foldr genEnv' (exp00, env1, n) [exp2, exp1, (Var name1)]
genEnv' (LetRec name1 exp1 exp2) (exp00, env1, n)  = foldr genEnv' (exp00, env1, n) [exp2, exp1, (Var name1)]
genEnv' (AST list1) (exp00, env1, n)               = foldr genEnv' (exp00, env1, n) list1
genEnv' (VarRep name1) (exp00, env1, n)            = case env1 of
    Nothing     -> (exp00, Nothing, n)
    Just []     -> (exp00, Just [(name1, TyVarRep ("Type" ++ show n ++ "{" ++ name1 ++ "}"))], n+1)
    otherwise   -> case name1 `isNameInEnv` env1 of
        True    -> (exp00, env1,n)
        False   -> (exp00, (++) <$> (Just [(name1, TyVarRep ("Type" ++ show n ++ "{" ++ name1 ++ "}"))]) <*>  env1, n+1)
genEnv' GenSym (exp00, env1, n)                    = (exp00, (++) <$> (Just [(name1, TyVarRep ("Type" ++ show n ++ "{" ++ name1 ++ "}"))]) <*> env1, n+1)
    where name1 = freshVarRepName n
genEnv' (DownA exp1) (exp00, env1, n)              =  genEnv' exp1 (exp00, env1, n)
genEnv' (UpA exp1) (exp00, env1, n)                =  genEnv' exp1 (exp00, env1, n)
genEnv' (Eval exp1) (exp00, env1, n)               =  genEnv' exp1 (exp00, env1, n)
genEnv' (EvalA t1 exp1) (exp00, env1, n)           =  genEnv' exp1 (exp00, env1, n)
genEnv' (LetDA name1 exp1 exp2) (exp00, env1, n)   =  foldr genEnv' (exp00, env1, n) [exp2, exp1, (Var name1)]
genEnv' (TagExpr tag1) (exp00, env1, n)            = (exp00, env1, n)
genEnv' (Error exp1) (exp00, env1, n)              = (exp00, Nothing, n)
genEnv' (ErrorLoop exp1) (exp00, env1, n)          = (exp00, Nothing, n)



replaceGenSym :: State -> State
replaceGenSym st0@(Var name1, env, n) =st0
replaceGenSym st0@(Lam name1 exp1, env, n) = (Lam name1 nexp1, nenv, nn)
    where (nexp1, nenv, nn) = replaceGenSym (exp1, env, n)
replaceGenSym st0@(App exp1 exp2, env, n) = (App nexp1 nexp2, nnenv, nnn)
    where (nexp1, nenv, nn)     = replaceGenSym (exp1, env, n)
          (nexp2, nnenv, nnn)   = replaceGenSym (exp2, nenv, nn)
replaceGenSym st0@(LitN numb1, env, n) = st0
replaceGenSym st0@(LitB bool1, env, n) = st0
replaceGenSym st0@(PrimUni uniOp1 exp1, env, n) = (PrimUni uniOp1 nexp1, nenv, nn)
    where (nexp1, nenv, nn) = replaceGenSym (exp1, env, n)
replaceGenSym st0@(PrimBin binOp1 exp1 exp2, env, n) = (PrimBin binOp1 nexp1 nexp2, nnenv, nnn)
    where (nexp1, nenv, nn)     = replaceGenSym (exp1, env, n)
          (nexp2, nnenv, nnn)   = replaceGenSym (exp2, nenv, nn)
replaceGenSym st0@(If bool1 exp1 exp2, env, n) = (If nbool1 nexp1 nexp2, nnnenv, nnnn)
    where (nbool1, nenv, nn)    = replaceGenSym (bool1, env, n)
          (nexp1, nnenv, nnn)   = replaceGenSym (exp1, nenv, nn)
          (nexp2, nnnenv, nnnn) = replaceGenSym (exp2, nnenv, nnn)
replaceGenSym st0@(Let name1 exp1 exp2, env, n) = (Let name1 nexp1 nexp2, nnenv, nnn)
    where (nexp1, nenv, nn)     = replaceGenSym (exp1, env, n)
          (nexp2, nnenv, nnn)   = replaceGenSym (exp2, nenv, nn)
replaceGenSym st0@(LetRec name1 exp1 exp2, env, n) = (LetRec name1 nexp1 nexp2, nnenv, nnn)
    where (nexp1, nenv, nn)     = replaceGenSym (exp1, env, n)
          (nexp2, nnenv, nnn)   = replaceGenSym (exp2, nenv, nn)
replaceGenSym st0@(AST (exprList), env, n) = (AST nexprList, nenv, nn)
    where (nexprList, nenv, nn)     = replaceGenSymList (exprList, env, n) 
replaceGenSym st0@(VarRep name1, env, n) =st0
replaceGenSym st0@(GenSym, env, n) = (VarRep name1, env, n+1)
    where name1 = freshVarRepName n
replaceGenSym st0@(UpA exp1, env, n) = (UpA nexp1, nenv, nn)
    where (nexp1, nenv, nn) = replaceGenSym (exp1, env, n)
replaceGenSym st0@(DownA exp1, env, n) = (DownA nexp1, nenv, nn)
    where (nexp1, nenv, nn) = replaceGenSym (exp1, env, n)
replaceGenSym st0@(Eval exp1, env, n) = (Eval nexp1, nenv, nn)
    where (nexp1, nenv, nn) = replaceGenSym (exp1, env, n)
replaceGenSym st0@(EvalA t1 exp1, env, n) = (EvalA t1 nexp1, nenv, nn)
    where (nexp1, nenv, nn) = replaceGenSym (exp1, env, n)
replaceGenSym st0@(LetDA name1 exp1 exp2, env, n) = (LetDA name1 nexp1 nexp2, nnenv, nnn)
    where (nexp1, nenv, nn)     = replaceGenSym (exp1, env, n)
          (nexp2, nnenv, nnn)   = replaceGenSym (exp2, nenv, nn)
replaceGenSym st0@(TagExpr tag1, env, n) =st0    
replaceGenSym st0@(Error exp1, env, n) =st0
replaceGenSym st0@(ErrorLoop exp1, env, n) =st0
replaceGenSym st0@(ErrorType exp1 type1, env, n) =st0
    
replaceGenSymList :: ([Expr], Env, Int) -> ([Expr], Env, Int)
replaceGenSymList ([], env, n) = ([], env, n)
replaceGenSymList ((x:xs), env, n) = ((nx:nnxs), nnenv, nnn)
    where (nx, nenv, nn)        = replaceGenSym (x, env, n)
          (nnxs, nnenv, nnn)    = replaceGenSymList (xs, nenv, nn)

 
isNameInEnv :: Name -> Env -> Bool
isNameInEnv name1 Nothing = False
isNameInEnv name1 (Just []) = False
isNameInEnv name1 (Just (x:xs))
    | (fst x) == name1 = True
    | otherwise = name1 `isNameInEnv` (Just xs)

 
-- isTinEnv :: Name -> Env1 -> Bool
-- isTinEnv name1 Nothing = False
-- isTinEnv name1 (Just []) = False
-- isTinEnv name1 (Just (x:xs))
    -- | (fst x) == name1 = True
    -- | otherwise = name1 `isTinEnv` (Just xs)

algWExp :: Expr -> SubType
algWExp exp1 = algW (genEnv exp1)



algW :: State -> SubType
algW (exp, Nothing, n) = Nothing
algW (Var x, env, n)      
    | (Nothing == (readTypeEnv x env)) = Nothing
    | otherwise         = Just ((Just []), fromJust $ readTypeEnv x env, n)
algW ((App exp1 exp2), env, n) 
    | areJust           = (Just (s, t, n2 +1))
    | otherwise         = Nothing
    where areJust = (isJust (algW (exp1, env, n))) && (isJust (algW (exp2, (applySubEnv s1 env), n1))) && isJust s
          (Just (s1,t1, n1)) = algW (exp1, env, n)
          (Just (s2,t2, n2)) = algW (exp2, (applySubEnv s1 env), n1)
          subV = unifier(s2t1, TyFunc t2 bnew)
          bnew = freshType n2 
          s = (++) <$> subV <*> s2s1
          s2s1 = (++) <$> s2 <*> s1
          s2t1 = subType s2 t1
          t = subType subV bnew
algW ((Lam name1 exp1), env, n) 
    | areJust           = (Just (s1, t, n1 + 1))
    | otherwise         = Nothing
    where areJust = isJust (algW (exp1, envNewx, n+1))
          (Just (s1, t1, n1)) = algW (exp1, envNewx, n+1)
          envNewx = addTExpEnv (name1, bnew) envRx
          envRx = (removeNameEnv name1 env)
          bnew = freshType n
          subs1bnew = subType s1 bnew
          t = TyFunc (subs1bnew) t1
          --(Just (s2,t2)) = algW (exp2, (applySubEnv s1 env), n?)
algW (LitN numb1, env, n) = Just (Just [], TyInt,  n)
algW (LitB bool1, env, n) = Just (Just [], TyBool, n)
algW (PrimUni uniOp1 exp1, env, n) 
    | areJust           = case elem uniOp1 uniOpBool of
        True            -> case isJust s of
            True        -> Just (s, TyBool, n1)
            otherwise   -> Just (s, TyError exp1 t1 TyBool, n1)
        False           -> Nothing
    | otherwise         = Nothing
    where areJust = isJust (algW (exp1, env, n))
          (Just (s1, t1, n1))   = algW (exp1, env, n)
          s                     = (++) <$> subV <*> s1
          subV                  = unifier (TyBool, subType s1 t1)
algW (PrimBin binOp1 exp1 exp2, env, n) 
    | areJust               = case elem binOp1 binOpBool of
        True                -> case isJust subVt1Bool of
            True            -> case isJust subVt2Bool of
                True        -> Just (sBool, TyBool, n2)
                False       -> Just (sBool, TyError exp2 t2 TyBool, n2)
            False           -> Just (sBool, TyError exp1 t1 TyBool, n2)
        False | elem binOp1 binOpIntInt -> case isJust subVt1Int of
            True            -> case isJust subVt2Int of
                True        -> Just (sInt, TyInt, n2)
                False       -> Just (sInt, TyError exp2 t2 TyInt, n2)
            False           -> Just (sInt, TyError exp2 t1 TyInt, n2)
        False | elem binOp1 binOpIntBool -> case isJust subVt1Int of
            True            -> case isJust subVt2Int of
                True        -> Just (sInt, TyBool, n2)
                False       -> Just (sInt, TyError exp2 t2 TyInt, n2)
            False           -> Just (sInt, TyError exp2 t1 TyInt, n2)
        False               -> Nothing
    | otherwise     = Nothing
    where areJust               = isJust ( algW (exp1, env, n)) && isJust ( algW (exp2, (applySubEnv s1 env), n1))  
          (Just (s1, t1, n1))   = algW (exp1, env, n)
          (Just (s2, t2, n2))   = algW (exp2, (applySubEnv s1 env), n1)
          s2s1                  = (++) <$> s2 <*> s1
          s2s1t1                = subType s2s1 t1
          s2s1t2                = subType s2s1 t2
          subVt1Int             = unifier (s2s1t1, TyInt)
          subVt2Int             = unifier (subType subVt1Int  s2s1t2, TyInt )
          subVt1Bool            = unifier (s2s1t1, TyBool)
          subVt2Bool            = unifier (subType subVt1Bool s2s1t2, TyBool)
          sInt                  = (++) <$> subVt2Int <*> subVt1Int
          sBool                 = (++) <$> subVt2Bool <*> subVt1Bool
algW (If expb exp1 exp2, env, n) 
    | areJust           = case isJust sb of
        True            -> case isJust subVt1t2 of 
            True        -> Just (s, t, n3)
            False       -> Just (s, TyError exp1 (TyError exp2 TyErrorEq (subType subVbool t2)) (subType subVbool t1), n3)
        False           -> Just (s, TyError expb tb TyBool, n3)
    | otherwise         = Nothing
    where areJust               = isJust ( algW (exp1, env, n)) && isJust ( algW (exp2, env1, n1)) && isJust (algW (exp2, env2, n2))
          (Just (sb, tb, n1))   = algW (expb, env,  n)
          (Just (s1, t1, n2))   = algW (exp1, env1, n1)
          (Just (s2, t2, n3))   = algW (exp2, env2, n2)
          env1                  = applySubEnv sb env
          env2                  = applySubEnv s1 env1
          subVbool              = unifier (tb, TyBool)
          subVt1t2              = unifier (subType subVbool t1, subType subVbool t2) 
          s                     = concatSub [subVt1t2, subVbool]
          t                     = subType subVt1t2 (subType subVbool t1)
algW ((Let name1 exp1 exp2), env, n) 
    | areJust                   = (Just (s, t, n2))
    | otherwise                 = Nothing
    where areJust               = (isJust (algW (exp1, env, n))) && (isJust (algW (exp2, s1AxUnewx, n1)) )
          (Just (s1, t1, n1))   = algW (exp1, env, n)
          tyname1               = t1
          newx                  = (name1, subType s1 tyname1)
          envAx                 = removeNameEnv name1 env
          s1Ax                  = applySubEnv s1 envAx
          s1AxUnewx             = addTExpEnv newx s1Ax
          (Just (s2, t2, n2))   = algW (exp2, s1AxUnewx, n1)
          -- tyname1               = (fromJust $ readTypeEnv name1 env)
          s                     = concatSub [s2, s1]
          t                     = t2

algW ((LetRec name1 exp1 exp2), env, n) 
    | areJust                   = (Just (s, t, n2))
    | otherwise                 = Nothing
    where areJust               = (isJust (algW (exp1, env, n))) && (isJust (algW (exp2, s1AxUnewx, n1)) )
          (Just (s1, t1, n1))   = algW (exp1, env, n)
          tyname1               = (t1)
          newx                  = (name1, subType s1 tyname1)
          envAx                 = removeNameEnv name1 env
          s1Ax                  = applySubEnv s1 envAx
          s1AxUnewx             = addTExpEnv newx s1Ax
          (Just (s2, t2, n2))   = algW (exp2, s1AxUnewx, n1)
          -- tyname1               = (fromJust $ readTypeEnv name1 env)
          s                     = concatSub [s2, s1]
          t                     = t2

algW (AST [], env, n)           = Just (Nothing, TyError (AST []) (TyCodeUnP) TyTag, n)
algW (AST (x:xs), env, n)       = Just (sl, TyCodeUnP, nl)
    where (st1@(Just (s1, t1, n1)):stL) = algWList xs (x, env, n)      
          Just (sl, tl, nl)             = last stL 
          
algW (UpA exp1, env, n) = case algW (exp1, env, n) of
    Nothing             -> Just (Nothing, TyError exp1 TyErrorEq TyErrorEq, n)
    Just (s1, t1, n1)   -> Just (s1, TyCodeUnP, n1)
algW (DownA exp1, env, n) = case algW (exp1, env, n) of
    Nothing             -> Just (Nothing, TyError exp1 TyErrorEq TyErrorEq, n)
    Just (s1, TyCodeUnP, n1)    -> algW ((\(a1,a3) -> (a1, env, a3)) (evalFCRT (evalFCCT (DownA exp1, n))))
    Just (s1, t1, n1)           -> Just (s1, TyError exp1 t1 (TyCodeUnP), n1)
        
        
algW (VarRep x, env, n)      
    | (Nothing == (readTypeEnv x env))  = Nothing
    | otherwise                         = Just ((Just []), fromJust $ readTypeEnv x env, n)
algW (GenSym, env, n)                   = Just (Just [], TyGenSym, n)   
algW (TagExpr tag1, env, n)             = Just (Just [], TyTag, n)          
algW (EvalA t1 exp0, env, n)            = case algW (exp0, env, n) of 
    Nothing                             -> Nothing
    Just (s1, TyCodeUnP, n1)            -> Just (s1, t1, n1)
    otherwise                           -> Nothing
algW (Eval exp0, env, n)                = Nothing
algW (LetDA name1 exp1 exp2, env, n) = case algW (exp1, env, n) of
    Nothing                             -> Nothing
    Just (s1, t1, n1)                   -> case algW (Var name1, applySubEnv s1 env, n1) of
        Nothing                         -> Nothing
        Just (sn, tn, nn)               -> case unifier (tn, t1) of
            Nothing                     -> Just (Nothing, TyErrorUnify tn t1, nn)
            subnexp1                    -> case algW (exp2, applySubEnv subnexp1 (applySubEnv sn (applySubEnv s1 env)), nn) of
                Nothing                 -> Nothing
                Just (s2, t2, n2)       -> Just (news, newt, n2)
                    where news = concatSub [s2, subnexp1, sn, s1]
                          newt = subType news t2
                      
            

areTyCode :: [SubType] -> Bool
areTyCode []        = True
areTyCode (Nothing:xs) = False
areTyCode ((Just (s1, t1, n)):xs) = case t1 of
    TyCode type1    -> True && (areTyCode xs)
    TyCodeUnP       -> True && (areTyCode xs)
    otherwise       -> False
    
fromTyCode :: [Type] -> Maybe [Type]
fromTyCode [] = Just []
fromTyCode (x:xs) = case x of
    TyCode type1    -> case fromTyCode xs of
        Nothing     -> Nothing
        Just xs'    -> Just (type1:xs')
    otherwise       -> Nothing
    
-- algW (AST  (x:xs), env, n)      = case tyTag == TyTag of
    -- True                        -> case x of
        -- TyTag                   -> Just (sTag, x, n1)
        -- otherwise               -> case areJust 
  
    -- False                       -> Just (Nothing, TyError x tyTag TyTag, n1)
        -- (Just (s, t, n2))
    -- | otherwise                 = Nothing
    -- where conTag                = isJust (algW (x, env, n))
          -- Just (sTag, tyTag, n1)= algW (x, env, n) 
          -- areJust               = conTag && (isJust (algWList xs (x, env, n) ))
          
          
          -- (Just (sx, tx, n1))   = algW (x, env, n)
          -- s1AxUnewx             = addTExpEnv newx s1Ax
          -- (Just (s2, t2, n2))   = algW (exp2, s1AxUnewx, n1)
          -- newx                  = (name1, subType s1 tyname1)
          -- tyname1               = (fromJust $ readTypeEnv name1 env)
          -- s1A                   = applySubEnv s1 envAx
          -- envAx                 = removeNameEnv name1 env
          -- s1Ax                  = s1A
          -- s                     = (++) <$> s2 <*> s1
          -- t                     = t2


algWAST :: (Expr, Env, Int) -> SubType
algWAST (AST [], env, n)            = Just (Nothing, TyError (AST []) TyErrorEq TyTag, n)
algWAST (AST (x:xs) , env, n)       = Just (sl, TyCodeUnP, nl)
    where (st1@(Just (s1, t1, n1)):stL) = algWList xs (x, env, n)      
          Just (sl, tl, nl) = last stL 
          
algWAST (other , env, n) = Nothing


areJustList [] = True
areJustList (x:xs) = case x of
    Nothing     -> False
    (Just x1)   -> areJustList xs




  

algWList :: [Expr] -> State -> [SubType]
algWList [] st1 = [algW st1]
algWList (x:xs) st1@(exp1, env1, n1) 
    | areJust = sFinal
    | otherwise = [Nothing]
    where areJust = isJust (algW st1) && (and $ map isJust (algWList xs (x, applySubEnv s2 env1, n2)))
          Just (s2, t2, n2)     = algW st1
          stList                = algWList  xs (x, applySubEnv s2 env1, n2)
          sFinal                = ((Just (s2, t2, n2)):stList)
    
removeNameEnv :: Name -> Env -> Env
removeNameEnv name1 Nothing = Nothing
removeNameEnv name1 (Just tNameL) = Just (filter (\x -> name1 /= fst x) tNameL)
    
    
removeNameState :: Name -> State -> State
removeNameState name1 (exp1, Nothing, n) = (exp1, Nothing, n)
removeNameState name1 (exp1, (Just tNameL), n) = (exp1, Just (filter (\x -> name1 /= fst x) tNameL), n)
    
    
    
applySubEnv :: Sub -> Env -> Env
applySubEnv Nothing env = Nothing
applySubEnv (Just []) env = env
applySubEnv (Just sub) (Just []) = (Just [])
applySubEnv (Just sub) (Just (x:xs)) = (++) <$> (Just [(applySubEnv1 (Just sub) (x))]) <*> (applySubEnv (Just sub) (Just xs))



applySubEnv1 :: Sub -> (Name, Type) -> (Name, Type)
applySubEnv1 Nothing texp = error "how is this possibe"
applySubEnv1 (Just [(t1, name1)]) (name2, t2)
    | name1 == name2 = (name2, t1)
    | name1 /= name2 = (name2, t2)
    | otherwise = error "this should not be possible"
applySubEnv1 (Just (x:xs)) texp1 = applySubEnv1 (Just [x]) (applySubEnv1 (Just xs) texp1)

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
--Provides a substitution sub1 which when applied to type pair (t1,t2)  gives (t3,t3) where t3 is the substitution of sub1 t1 (and sub1 t2)
unifier :: (Type, Type) -> Sub
unifier (t1,t2) = iterateF (Just []) (t1,t2)


--does the hard work of the unifier function. Provides a substitution for each node where the subtypes do not match, uses depth first traversal.
iterateF :: Sub -> (Type, Type) -> Sub
iterateF subV (t1,t2)  
    | subVt1 == subVt2 = subV
    | isViableSub (Just [(b, a)]) = iterateF (basubV) (t1,t2)
    | isViableSub (Just [(a, b)]) = iterateF (absubV) (t1,t2)
    | otherwise = Nothing
      where subVt1 = subType subV t1
            subVt2 = subType subV t2
            Just (a, b) = (dft (Just (subVt1, subVt2)))
            subab = Just (a, (\(TyVar x) -> x) b)
            subba = Just (b, (\(TyVar x) -> x) a)
            absubV = ((:) <$> subab <*> subV) 
            basubV = ((:) <$> subba <*> subV) 
           
isViableSub :: Maybe [(Type, Type)] -> Bool
isViableSub Nothing = False
isViableSub (Just [(t1, (TyVar name1))]) = name1 `notElem` (allTypeVar t1) 
isViableSub (Just [(t1,t2)]) = False
isViableSub (Just (x:xs)) = isViableSub (Just [x]) && isViableSub (Just xs)

--Gives list of all type Variable names in a type.
allTypeVar :: Type -> [Name]
allTypeVar (TyVar name1)                = [name1]
allTypeVar (TyVarRep name1)             = [name1]
allTypeVar TyInt                        = []
allTypeVar TyBool                       = []
allTypeVar (TyFunc type1 type2)         = union (allTypeVar type1) (allTypeVar type2)
allTypeVar (TyCodeUnP)                  = []
allTypeVar (TyTag)                      = []
allTypeVar (TyGenSym)                   = []
allTypeVar (TyErrorEq)                  = []
allTypeVar (TyError exp1 type1 type2)   = []
allTypeVar (TyErrorLoop)                = []

--Depth first traversal (DFT) finds the first pair at which the two types do not match.
dft :: Maybe (Type, Type) -> Maybe (Type, Type)
dft Nothing                                 = Nothing
dft (Just ((TyVar a), (TyVar b)))
    | (a==b)                                = Nothing
    | otherwise                             = Just (TyVar a, TyVar b)
dft (Just ((TyVarRep a), (TyVarRep b)))
    | (a==b)                                = Nothing
    | otherwise                             = Just (TyVarRep a, TyVarRep b)
dft (Just (TyVar a, TyFunc ty1 ty2))        = Just (TyVar a, TyFunc ty1 ty2)
dft (Just (TyFunc ty1 ty2, TyVar a))        = Just (TyFunc ty1 ty2, TyVar a)
dft (Just (TyVarRep a, TyFunc ty1 ty2))     = Just (TyVarRep a, TyFunc ty1 ty2)
dft (Just (TyFunc ty1 ty2, TyVarRep a))     = Just (TyFunc ty1 ty2, TyVarRep a)
dft (Just (TyFunc ty1 ty2, TyFunc ty3 ty4)) 
    | (ty1 == ty3)                          = dft (Just (ty2, ty4))
    | otherwise                             = dft (Just (ty1, ty3))
dft (Just (TyCodeUnP, t1))                  = Just (TyCodeUnP, t1)
dft (Just (t1, TyCodeUnP))                  = Just (t1, TyCodeUnP)
dft (Just (TyErrorEq, _))                   = Nothing
dft (Just (a, TyErrorEq))                   = Nothing
dft (Just (TyError _ _ _, _))               = Nothing
dft (Just (_, TyError _ _ _))               = Nothing
dft (Just (TyErrorLoop, _))                 = Nothing
dft (Just (_, TyErrorLoop))                 = Nothing
dft (Just (TyGenSym, a))                    = Just (TyGenSym, a)
dft (Just (a, TyGenSym))                    = Just (a, TyGenSym)
dft (Just (type1, type2))
    | (type1 == type2)                      = Nothing
    | otherwise                             = Just (type1, type2)
    
-- dft (Just (a,b)) = Just (a,b)

    
    
containsError :: Type -> Bool
containsError (TyVar st1)       = False
containsError (TyVarRep st1)    = False
containsError TyInt             = False
containsError TyBool            = False
containsError (TyFunc t1 t2)    = containsError t1 && containsError t2
containsError (TyCodeUnP)       = False
containsError TyTag             = False
containsError TyGenSym          = False
containsError TyErrorEq         = True
containsError (TyError e1 t1 t2)= True
containsError TyErrorLoop       = True
containsError (TyErrorUnify t1 t2) = True
    
    
    
    
concatSub :: [Sub] -> Sub
concatSub []                = Just []    
concatSub [Nothing]         = Nothing
concatSub [Just s1]         = Just s1
concatSub (Nothing:xs)      = Nothing
concatSub ((Just s1):xs)    = case concatSub xs of
    Nothing                 -> Nothing
    Just s2                 -> Just (s1 ++ s2)

    
    
-- genCon :: State -> State

--creates the constraints from an expression given a state
-- createCon1 :: Expr -> State -> State
-- createCon1 (Var name1) st0 = st0 
-- createCon1 expLam@(Lam name1 exp1) st0@(exp0, env1, conL ,n) = st3
    -- where [texpLam, tname1, texp1] = findTypeL  [expLam, (Var namex), exp1] st0
          -- st1 = unionMK [(Var namex, texpvx)] $ removeExp (Var namex) st0
          -- st2 = createCon1  exp1 st1
          -- st3 = addCon [(texpLam, TyFunc tname1 texp1)] st2
-- createCon1 (expApp@(App expe expe')) st0 = st3
    -- where [texpee', texpe, texpe'] = readTypeL  [expApp, expe, expe'] st0
          -- st1 = createCon1 expe  st0
          -- st2 = createCon1 expe' st1
          -- st3 = addCon [(texpe, TyFunc texpe' texpee')] st2
-- createCon1 (LitN numb1) st0 = st0
-- createCon1 (LitB bool1) st0 = st0
-- createCon1 expPrimUni@(PrimUni uniOp1 exp1) st0 = case uniOp1 of
    -- Not         -> addCon [(exp1, TyBool), (expPrimUni, TyBool)] st1
    -- otherwise   -> st0
    -- where st1 = createCon1 exp1 st0
 
-- createCon1 expPrimBin@(PrimBin binOp1 exp1 exp2) st0 = case elem binOp1 binOpBool of
    -- True        -> addCon [(exp1, TyBool), (expPrimBin, TyBool)] st1
    -- False       -> case elem binOp1 binOpInt of
        -- True    -> addCon [(exp1, TyInt), (expPrimBin, TyInt)] st1
        -- False   -> error "no such binary operation in PrimBin"
    -- where st1 = createCon1 exp1 st0

-- createCon1 expIf@(If exp1 exp2 exp3) st0 = addCon [(exp1, TyBool), (exp2, TyVar "x"), (exp3, TyVar "x")] st0

-- readTypeL :: [Name] -> State -> [Maybe Type]
-- readTypeL [] (exp2, xs, xCon, n) = []
-- readTypeL [exp1] (exp2, Just xs, xCon, n) = [lookup exp1 xs]
-- readTypeL (exp1:expL) (exp2, Just xs, xCon, n) = ((lookup exp1 xs):(readTypeL expL (exp2, Just xs, xCon, n)))
-- readTypeL [exp1] (exp2, Nothing, xCon, n) = [Nothing]
-- readTypeL (exp1:expL) (exp2, Nothing, xCon, n) = (Nothing:(readTypeL expL (exp2, Nothing, xCon, n)))


readTypeL :: [Name] -> State -> [Maybe Type]
readTypeL [] (exp2, xs, n) = []
readTypeL [exp1] (exp2, Just xs, n) = [lookup exp1 xs]
readTypeL (exp1:expL) (exp2, Just xs, n) = ((lookup exp1 xs):(readTypeL expL (exp2, Just xs, n)))
readTypeL [exp1] (exp2, Nothing, n) = [Nothing]
readTypeL (exp1:expL) (exp2, Nothing, n) = (Nothing:(readTypeL expL (exp2, Nothing, n)))


readTypeEnv :: Name -> Env -> Maybe Type
readTypeEnv name1 (Just []) = Nothing
readTypeEnv name1 (Just [(name2, type2)]) 
    | name1 == name2 = Just type2
    | otherwise = Nothing
readTypeEnv name1 (Just ((x@(name2, type2)):xs))  
    | name1 == name2 = Just type2
    | otherwise = readTypeEnv name1 (Just xs)

addTExpEnv :: (Name, Type) -> Env -> Env
addTExpEnv texp1 Nothing = Nothing
addTExpEnv texp1 (Just xs) = Just (texp1:xs)
    
        
--Substitute type1 for type2 in type3 = type3 [type1/type2]          
subType :: Sub -> Type -> Type
subType Nothing a                                   = a
subType (Just []) a                                 = a
subType (Just [(ts1, name1)]) TyInt                 = TyInt
subType (Just [(ts1, name1)]) TyBool                = TyBool
subType (Just [(ts1, name1)]) (TyVar var1)
    | (name1 == var1)                               = ts1
    | otherwise                                     = (TyVar var1)
subType (Just [(ts1, name1)]) (TyVarRep var1)       = (TyVarRep var1)
subType (Just [(ts1, name1)]) (TyFunc type1 type2)  = TyFunc (subType (Just [(ts1, name1)]) type1) (subType (Just [(ts1, name1)]) type2)
subType (Just [(ts1, name1)]) (TyCodeUnP)           = TyCodeUnP
subType (Just [(ts1, name1)]) (TyTag)               = TyTag
subType (Just [(ts1, name1)]) (TyGenSym)            = TyGenSym
subType sub1 (TyErrorEq)                            = TyErrorEq
subType sub1 (TyError a b c)                        = TyError a b c
subType sub1 (TyErrorLoop)                          = TyErrorLoop

subType (Just (x:xs)) type1                         = subType (Just [x]) (subType (Just xs) type1)
    
    
    
    
    
freshType :: Int -> Type
freshType n = (TyVar ("Type" ++ show n++"{fresh}"))    

freshVarRepName :: Int -> Name
freshVarRepName n = ("freshGenSym"++ "{" ++ show n ++ "}")

    
    