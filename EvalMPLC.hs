
    
module EvalMPLC where
  -- runEval
-- ) where

--syntax could be written here
import SyntaxMPLC
import Data.List
import Data.Maybe
import System.IO  
import TestALL as ALL
-- import TypeCheckerUnParam

--evaluation at compile time then run-time (lambda)
evalCTRT :: Expr -> Expr
evalCTRT exp1 = fst (evalFCRT $ evalFCCT (exp1, 1))

--evaluation at run-time (lambda)
evalRT :: Expr -> Expr
evalRT expr1 = fst (evalFCRT (expr1, 1))

--evaluation at compile time 
evalCT :: Expr -> Expr
evalCT expr1 = fst (evalFCCT (expr1, 1))

--evaluation down a meta level of an evaluation up a meta-level
evalULDL :: Expr -> Expr
evalULDL expr1 = fst (evalFCDL $ evalFCUL (expr1, 1))

--evaluation down a meta level of an evaluation up a meta-level of a compiled term
evalCTULDL :: Expr -> Expr
evalCTULDL expr1 = fst (evalFCDL $ evalFCUL $ evalFCCT (expr1, 1))

--evaluation down a meta level of an evaluation up a meta-level
evalULDL8 :: Expr -> Expr
evalULDL8 expr1 = fst (evalFCDL $ evalFCDL $ evalFCDL $ evalFCDL $ evalFCDL $ evalFCDL $ evalFCDL $ evalFCDL $ evalFCDL $ evalFCDL $ evalFCUL$ evalFCUL$ evalFCUL$ evalFCUL$ evalFCUL $ evalFCUL $ evalFCUL $ evalFCUL $ evalFCUL $ evalFCUL (expr1, 1))

--evaluation down a meta-level
evalDL ::Expr -> Expr
evalDL expr1 = fst (evalFCDL (expr1, 1))

--evaluation up a meta-level
evalUL ::Expr -> Expr
evalUL expr1 = fst (evalFCUL (expr1, 1))

--the charcter length limit of an expression used for loops.
limitx = 800













--COMPILE-TIME EVALUATION:

evalFCCT :: (Expr, FreshCounter) -> (Expr, FreshCounter)
evalFCCT (VarRep string1, n)            = (VarRep string1, n)
evalFCCT (Var name1, n)                 = (Var name1, n)
evalFCCT (Lam name1 exp1, n) 
    | isNotError newexp1                = (newExp, newn)
    | otherwise                         = (Error newExp, newn)
    where (newexp1, newn)               = evalFCCT (exp1, n)
          newExp                        = Lam name1 newexp1
evalFCCT (App exp1 exp2, n) 
    | areNotError [newexp1, newexp2]    = (newExp, newn)
    | otherwise                         = (Error newExp, newn)
    where ([newexp1, newexp2], newn)    = evalFCCTList ([exp1, exp2], n) 
          newExp                        = App newexp1 newexp2
evalFCCT (LitN int1, n)                 = (LitN int1, n)
evalFCCT (LitB bool1, n)                = (LitB bool1, n)
evalFCCT (PrimUni uniOp1 exp1, n)
    | isNotError newexp1                = (newExp, newn)
    | otherwise                         = (Error newExp, newn)
    where (newexp1, newn)               = evalFCCT (exp1, n)
          newExp                       = (PrimUni uniOp1 newexp1)
evalFCCT (PrimBin binOp1 exp1 exp2, n)
    | areNotError [newexp1, newexp2]    = (newExp, newn)
    | otherwise                         = (Error newExp, newn)
    where ([newexp1, newexp2], newn)    = evalFCCTList ([exp1, exp2], n) 
          newExp                        = PrimBin binOp1 newexp1 newexp2
evalFCCT (If exp1 exp2 exp3, n) 
    | areNotError [newexp1, newexp2, exp3]      = (newExp, newn)
    | otherwise                                 = (Error newExp, newn)
    where ([newexp1, newexp2, newexp3], newn)   = evalFCCTList ([exp1, exp2, exp3], n) 
          newExp                                = If newexp1 newexp2 newexp3
evalFCCT (Let name1 exp1 exp2, n) 
    | areNotError [newexp1, newexp2]       = (newExp, newn)
    | otherwise                         = (Error newExp, newn)
    where ([newexp1, newexp2], newn)    = evalFCCTList ([exp1, exp2], n) 
          newExp                        = Let name1  newexp1 newexp2
evalFCCT (LetRec name1 exp1 exp2, n) 
    | areNotError [newexp1, newexp2]    = (newExp, n)
    | otherwise                         = (Error newExp, n)
    where ([newexp1, newexp2], newn)    = evalFCCTList ([exp1, exp2], n)
          newExp                        = LetRec name1 newexp1 newexp2
evalFCCT (AST xs, n) 
    | areNotError newxs                 = (newExp, newn)
    | otherwise                         = (Error newExp, newn)
    where (newxs, newn)                 = evalFCCTList (xs, n)
          newExp                        = AST newxs
evalFCCT (GenSym, n)                    = (GenSym, n)       
evalFCCT (DownA exp1, n) 
    | areNotError [newExp]              = (newExp, newn)
    | otherwise                         = (Error (DownA newExp), newn)
    where (newExp, newn)                = evalFCDL $ evalFCRT $ evalFCCT (exp1, n)        
          
evalFCCT (UpA exp1, n) 
    | isNotError exp1                   = evalFCUL (exp1, n)
    | otherwise                         = (Error (UpA exp1), n)
evalFCCT (Eval exp1, n)
    | isNotError newexp1                = (Eval newexp1, newn)
    | otherwise                         = (Error (Eval newexp1), newn)
    where (newexp1, newn)               = evalFCCT (exp1,n)    
evalFCCT (EvalA t1 exp1, n)
    | isNotError newexp1                = (EvalA t1 newexp1, newn)
    | otherwise                         = (Error (EvalA t1 newexp1), newn)
    where (newexp1, newn)               = evalFCCT (exp1,n)
-- --LIFT CT
-- evalFCCT (Lift exp1, n) 
    -- | isNotError newexp1             = (Lift newexp1, newn)
    -- | otherwise                      = (Error (Lift newexp1), newn)
    -- where (newexp1, newn)            = evalFCCT (exp1, n)
evalFCCT (LetDA name1 exp1 exp2, n)
    | areNotError [newSubCTexp2]        = (newSubCTexp2, newn)
    | otherwise                         = (Error newSubCTexp2, newn)
    where (newCTRTexp1, newn')            = evalFCRT $ evalFCCT (exp1, n)
          (newSubCTexp2, newn)          = evalFCCT $ sub (newCTRTexp1, name1) (exp2, newn')
evalFCCT (TagExpr tag1, n)              = (TagExpr tag1, n)
evalFCCT (Error exp0, n)                = (Error (Error exp0), n)
evalFCCT (ErrorLoop exp0, n)            = (Error (ErrorLoop exp0), n)
--just in case other -> error

--evaluates a list of expressions in compile time consecutively carrying the fresh counter
evalFCCTList :: ([Expr], FreshCounter) -> ([Expr], FreshCounter)
evalFCCTList ([], n)        = ([], n)
evalFCCTList ((x:xs), n)    = ((newx : newxs), newn)
    where (newx, n')        = evalFCCT (x, n)
          (newxs, newn)     = evalFCCTList (xs, n')











--RUN-TIME EVALUATION:

evalFCRT :: (Expr, FreshCounter) -> (Expr, FreshCounter)
evalFCRT (VarRep name1, n)      = (VarRep name1, n)
evalFCRT (Var x, n)             = (Var x, n)
--LAM RT (neta reduction)
evalFCRT (Lam x exp1, n) 
    | isNotError newexp1        = (Lam x newexp1, newn)
    | otherwise                 = (Error (Lam x (Error exp1)), newn)
    where (newexp1, newn)       = (evalFCRT (exp1, n)) 
--APP RT (beta reduction)
-- evalFCRT (App e1 e2, n )        = case (e1, e2) of    
    -- ((Lam x eM), (eN))          -> do evalFCRT (sub (eN, x) (eM, n''))
    -- other                       -> case (e1', e2') of
        -- ((Lam x eM), eN)        -> do evalFCRT (sub (eN, x) (eM, n''))
        -- otherwise               -> ((App e1' e2'), n'')
    -- where (e1', n')             = evalFCRT (e1, n)
          -- (e2', n'')            = evalFCRT (e2, n')
evalFCRT (App e1 e2, n )        = case (e1', e2') of    
    ((Lam x eM), eN )           -> evalFCRT (sub (eN, x) (eM, n''))
    (Error exp1, e2')           -> case (e1, e2) of
        ((Lam x eM), eN)        -> case evalFCRT (sub (eN, x) (eM, n'')) of
            (Error eM', n''')   -> evalFCRT (sub (eN, x) (eM, n'')) 
            (exp, m) | not (hasError exp)    -> (exp, m)
            (other, m)           -> (Error other, m)
        otherwise               -> (Error (App e1 e2), n'')
    (e1' , Error exp2)          -> case (e1, e2) of
        ((Lam x eM), eN)        -> evalFCRT (sub (eN, x) (eM, n''))
        otherwise               -> (Error (App e1 e2), n'')
    ((eM), (eN))                -> (App eM eN, n'')
    where (e1', n')             = evalFCRT (e1, n)
          (e2', n'')            = evalFCRT (e2, n')
evalFCRT (LitN numb, n)         = (LitN numb, n)
evalFCRT (LitB bool, n)         = (LitB bool, n)
evalFCRT (PrimUni Not exp1, n)  = case newexp1 of
    -- Var name1                   -> (PrimUni Not newexp1, n)
    LitB False                  -> (LitB True, newn)
    LitB True                   -> (LitB False, newn)
    LitN numb                   -> (Error (PrimUni Not newexp1), n)
    -- App exp2 exp3               -> (PrimUni Not newexp1, n)
    -- PrimUni uniOp1 exp2         -> (PrimUni Not newexp1, n)
    -- PrimBin binOp1 exp2 exp3    -> (PrimUni Not newexp1, n)
    -- If exp2 exp3 exp4           -> (PrimUni Not newexp1, n)
    -- Let name1 exp2 exp3         -> (PrimUni Not newexp1, n)
    -- LetRec name1 exp2 exp3      -> (PrimUni Not newexp1, n)
    -- LetDA name1 exp2 exp3       -> (PrimUni Not newexp1, n)
    -- Eval exp1                   -> (PrimUni Not newexp1, n)
    -- EvalA ty1 exp1              -> (PrimUni Not newexp1, n)
    other                       -> ((PrimUni Not newexp1), newn)
    where (newexp1, newn)   = evalFCRT (exp1, n)
evalFCRT (PrimBin binOp exp1 exp2, n)
    | areNotError [newexp1, newexp2]    = evalPrimBin (PrimBin binOp newexp1 newexp2, newn)
    | otherwise                         = (Error (PrimBin binOp newexp1 newexp2), newn)
    where ([newexp1, newexp2], newn)    = evalFCRTList ([exp1,exp2], n)
evalFCRT (If bool1 exp1 exp2, n) 
    | trueNotError                  = evalFCRT (exp1, newn) --only checks if exp1 is not error
    | falseNotError                 = evalFCRT (exp2, newn) --only checks if exp2 is not error
    | (isNotBool newbool1)          = (Error (If newbool1 exp1 exp2), n)
    | otherwise                     = (If newbool1 exp1 exp2, newn) 
    where (newbool1, newn)          = evalFCRT (bool1, n)  
          trueNotError              = (isBoolT newbool1) && (areNotError [newbool1, exp1])
          falseNotError             = (isBoolF newbool1) && (areNotError [newbool1, exp2])
evalFCRT (Let name1 exp1 exp2, n) 
    | areNotError [exp1, exp2]      = evalFCRT (sub (exp1, name1) (exp2, n))--uses sub
    | otherwise                     = (Error (Let name1 exp1 exp2), n)
-- evalFCRT (exp0@(LetRec name1 exp1 exp2), n)
    -- | loopsIndefiniedtly            = (ErrorLoop exp0, n)
    -- | notError && noSub             = (exp2, n) 
    -- | notError                      = evalFCRT (sub (exp1, name1) (newexp2,n'))
    -- | otherwise                     = (Error exp0, n)
    -- where loopsIndefiniedtly        = (lengthExpr exp0) >= limitx 
          -- (newexp2, n')             = evalFCRT (exp2, n)
          -- noSub                     = exp0 == (fst $ evalFCRT (sub (exp1, name1) (newexp2,n')))
          -- notError                  = areNotError [exp1, newexp2]
-- evalFCRT (exp0@(LetRec name1 exp1 exp2), n)
    -- | loopsIndefiniedtly            = (ErrorLoop exp0, n)
    -- | otherwise                     = evalFCRTLetRec (exp0, n)
    -- where (newLetRec, newn)         = (evalFCRTLetRec (exp0, n))
          -- loopsIndefiniedtly        = areNotError [exp1, exp2] && lengthExpr newLetRec >= limitx 
evalFCRT (exp0@(LetRec name1 exp1 exp2), n)
    | growingLoopLimit              = case newLetRec of 
        ErrorLoop expn              -> (ErrorLoop exp0, newn) --bound on length of infinite growth
        other                       -> (ErrorLoop other, newn)
    | stationaryLoop                = case grows name1 exp2 of
        True                        -> (exp0, newn) -- equal but keep growing, leave LetRec 
        False                       -> (exp2, n)    -- equal but does not grow, remove LetRec
    | otherLoop                     = evalFCRT (newLetRec, newn) 
    | otherwise                     = (Error exp0, n)
    where (newLetRec, newn) = evalFCRTLetRec (exp0, n)
          growingLoopLimit          = areNotError [exp1, exp2] && lengthExpr newLetRec > limitx 
          stationaryLoop            = areNotError [exp1, exp2] && newLetRec == exp0
          otherLoop                 = areNotError [exp1, exp2]
    --maybe require alpha equivalence rather than just equivalence
evalFCRT (AST [], n)                = (Error (AST []), n)
evalFCRT (AST (tag1 : xs), n) 
    | validASTList                  = (AST (newtag1:newxs), newn)
    | otherwise                     = (Error (AST (newtag1:newxs)), newn)
    where ((newtag1:newxs), newn)   = evalFCRTList ((tag1:xs), n)
          validASTList              = areNotError (newtag1:newxs) && isTagLengthValid (newtag1:newxs) 
evalFCRT (GenSym, n)                = (VarRep ("freshZ" ++ show n), n+1)            
evalFCRT (DownA exp1, n)            = (Error (DownA exp1), n)        
evalFCRT (UpA exp1, n)              = (Error (UpA exp1), n)
evalFCRT (Eval exp1, n) 
    | isNotError newRTDLRTexp1      = (newRTDLRTexp1, newn)
    | otherwise                     = (Error newRTDLRTexp1, newn)
    where (newRTDLRTexp1, newn)     = evalFCRT $ evalFCDL $ evalFCRT (exp1, n)
evalFCRT (EvalA t1 exp1, n) 
    | isNotError newRTDLRTexp1      = (newRTDLRTexp1, newn)
    | otherwise                     = (Error newRTDLRTexp1, newn)
    where (newRTDLRTexp1, newn)     = evalFCRT $ evalFCDL $ evalFCRT (exp1, n)
-- --LIFT RT
-- evalFCRT (Lift exp1, n) = case exp1 of
    -- VarRep name1             -> (AST [(TagExpr TVarRep), VarRep name1], n)
    -- Var name1                -> (AST [(TagExpr TVar), VarRep name1], n)
    -- Lam name1 exp1           -> (AST [TagExpr TLam, newname1, newexp1], newn)
    -- App exp1 exp2            -> (AST [TagExpr TApp, newexp1, newexp2], newn)
    -- LitN numb1               -> (AST [TagExpr TLitN, LitN numb1], n)
    -- LitB bool1               -> (AST [TagExpr TLitB, LitB bool1], n)
    -- PrimUni uniOp1 exp1      -> (AST [TagExpr (TPrimUni uniOp1), newexp1], newn)
    -- PrimBin binOp1 exp1 exp2 -> (AST [TagExpr (TPrimBin binOp1), newexp1 newexp2], newn)
    -- If bool1 exp1 exp2       -> (AST [TagExpr TIf, newbool1, newexp1, newexp2], newn)
    -- Let name1 exp1 exp2      -> (AST [TagExpr TLet, newname1, newexp1, newexp2], newn)
    -- LetRec name1 exp1 exp2   -> (AST [TagExpr TLetRec, newname1, newexp1, newexp2], newn)
    -- AST exp1                 -> (AST ((TagExpr TPromote): newexp1), newn)
    -- DownA exp1               -> (Error (Lift (DownA exp1)), n)
    -- DownA exp1               -> (AST [TagExpr TDownA, newexp1], newn)
    -- --UpA exp1               -> (Error (Lift (UpA exp1)), n)
    -- UpA exp1                 -> (AST [TagExpr TUpA, newexp1], newn)
    -- Eval exp1                -> (AST [TagExpr TEval, newexp1], newn)
    -- -- Lift exp1             -> (AST [TagExpr TLift, newexp1], newn)
    -- LetDA exp1               -> (Error exp1, n)
    -- TagExpr tag1             -> (TagExpr tag1, n)
    -- Error exp1               -> (Error (Lift (Error exp1)), n)
evalFCRT (LetDA name1 exp1 exp2, n) = (Error (LetDA  name1 exp1 exp2), n)
evalFCRT (TagExpr tag1, n)          = (TagExpr tag1, n) 
evalFCRT (Error exp1, n)            = (Error exp1, n)
evalFCRT (ErrorLoop exp1, n)        = (ErrorLoop exp1, n)


--evaluates a list of expressions in compile time consecutively carrying the fresh counter
evalFCRTList :: ([Expr], FreshCounter) -> ([Expr], FreshCounter)
evalFCRTList ([], n)        = ([], n)
evalFCRTList ((x:xs), n)    = ((newx : newxs), newn)
    where (newx, n')        = evalFCRT (x, n)
          (newxs, newn)     = evalFCRTList (xs, n')

          
evalFCRTLetRec  ::  (Expr, FreshCounter) -> (Expr, FreshCounter)
evalFCRTLetRec (exp0@(LetRec name1 exp1 exp2), n) 
    | largeLoop                     = (ErrorLoop newLetRec, newn) --bound on length of infinite growth
    | stationaryLoop                = case grows name1 exp2 of
        True                        -> (newLetRec, newn) -- equal but keep growing, leave LetRec 
        False                       -> (exp2, n)    -- equal but does not grow, remove LetRec
    | otherLoop                     = evalFCRTLetRec (newLetRec, newn) 
    | otherwise                     = (Error exp0, n)
    where (newLetRec, newn)         = evalFCRTLetRec1 (exp0, n)
          largeLoop                 = areNotError [exp1, exp2] && ((lengthExpr newLetRec) >= limitx)
          stationaryLoop            = areNotError [exp1, exp2] && newLetRec == exp0
          otherLoop                 = areNotError [exp1, exp2]
    -- -- --maybe require alpha equivalence rather than just equivalence         
 -- evalFCRTLetRec (exp0@(LetRec name1 exp1 exp2), n) 
    -- | loopsIndefiniedtly            = (ErrorLoop exp0, n)
    -- | notError && noSub             = (exp2, n) 
    -- | notError                      = evalFCRT (sub (exp0, name1) (newexp2,n'))
    -- | otherwise                     = (Error exp0, n)
    -- where loopsIndefiniedtly        = areNotError [exp1, exp2] && (lengthExpr exp0) >= limitx 
          -- (newexp2, n')             = evalFCRT (exp2, n)
          -- noSub                     = exp0 == (fst $ evalFCRT (sub (exp0, name1) (newexp2,n')))
          -- notError                  = areNotError [exp1, newexp2]
evalFCRTLetRec (other, n) = (other, n)       

 
--eval one step (substitution) of LetRec:
evalFCRTLetRec1 :: (Expr, FreshCounter) -> (Expr, FreshCounter)
evalFCRTLetRec1 (exp0@(LetRec name1 exp1 exp2), n)
    | areNotError [newexp1, newexp2]    = (newLetRec, newn)
    | otherwise                         = (Error newLetRec, newn)
    where ([newexp1,newexp2], newn1)    = evalFCRTList ([exp1, exp2], n)
          -- (newLetexp2, newn)            = evalFCRT (Let name1 newexp1 newexp2, newn1)
          (newLetexp2, newn)            = (sub (newexp1, name1)  (newexp2, newn1))
          newLetRec                     = LetRec name1 newexp1 newLetexp2
    
evalFCRTLetRec1 (exp, n) = (Error exp, n)
-- (LetRec f M N) ---> (LetRec f M (N[M/f]))

--essentially says whether variable is contained (free) in another expression.
grows :: Name -> Expr -> Bool
grows name1 (VarRep varRp1) = False
grows name1 (Var name2)
    | name1 == name2        = True
    | otherwise             = False
grows name1 (Lam name2 exp2)
    | name1 == name2        = False
    | otherwise             = grows name1 exp2
grows name1 (App (Lam name2 exp1) exp2)
    | name1 == name2        = grows name1 exp2
    | otherwise             = (grows name1 (Lam name2 exp1)) || (grows name1 exp2)
grows name1 (App exp1 exp2) = (grows name1 exp1) || (grows name1 exp2)
grows name1 (LitN n)        = False
grows name1 (LitB b)        = False
grows name1 (PrimUni uniOp1 exp1)       = (grows name1 exp1)
grows name1 (PrimBin binOp1 exp1 exp2)  = (grows name1 exp1) || (grows name1 exp2)
grows name1 (If exp1 exp2 exp3)         = (grows name1 exp1) || (grows name1 exp2) || (grows name1 exp3)
grows name1 (Let name2 exp1 exp2)
    | name1 == name2        = (grows name1 exp1)
    | otherwise             = (grows name1 exp1) || (grows name1 exp2)
grows name1 (LetRec name2 exp1 exp2) 
    | name1 == name2        = grows name1 exp1
    | otherwise             = (grows name1 exp1) || (grows name1 exp2)
grows name1 (AST (expL))    = or $ map (\x -> grows name1 x) expL
grows name1 (GenSym)        = False
grows name1 (DownA exp1)    = grows name1 exp1
grows name1 (UpA exp1)      = grows name1 exp1
grows name1 (Eval exp1)     = grows name1 exp1
grows name1 (EvalA t1 exp1) = grows name1 exp1
-- grows name1 (Lift exp1)  = grows name1 exp1
grows name1 (LetDA name2 exp1 exp2)
    | name1 == name2        = (grows name1 exp1)
    | otherwise             = (grows name1 exp1) || (grows name1 exp2)
grows name1 (TagExpr tag1)  = False
grows name1 (Error exp1)    = error " Error grow?"
grows name1 (ErrorLoop exp1)    = error " Error Loop grow?"

          
          
          

evalPrimBin (PrimBin binOp1 exp1 exp2, n)
    | (elem binOp1 binOpBool) && (isNotBool exp1 || isNotBool exp2 ) = (Error (PrimBin binOp1 exp1 exp2), n)
    | (elem binOp1 binOpBool)   = case (exp1, exp2) of
        (LitB bool1, LitB bool2)    -> case binOp1 of
            And                         -> (LitB (bool1 && bool2), n)
            Or                          -> (LitB (bool1 || bool2), n)
            other                       -> (Error (PrimBin binOp1 exp1 exp2), n)
        (a, b)                      -> case (isNotBool a && isNotBool b) of
            True                        -> (Error (PrimBin binOp1 exp1 exp2), n)
            False                       -> (PrimBin binOp1 exp1 exp2, n)
    | (elem binOp1 binOpInt)    = case (exp1, exp2) of
        (LitN numb1, LitN numb2)    -> case binOp1 of
            Add                         -> (LitN (numb1 + numb2), n)
            Min                         -> (LitN (numb1 - numb2), n)
            Mul                         -> (LitN (numb1 * numb2), n)
            Div                     -> case (numb2 == 0) of
                True                    -> (Error (PrimBin binOp1 (LitN numb1) (Error (LitN numb2))), n)
                False                   -> (LitN (numb1 `div` numb2), n)
            Eq                          -> (LitB (numb1 == numb2), n)
            Lt                          -> (LitB (numb1 < numb2), n)
            Gt                          -> (LitB (numb1 > numb2), n)
            GtEq                        -> (LitB (numb1 >= numb2), n)
            other                       -> (Error (PrimBin binOp1 exp1 exp2), n)
        (a, b)                      -> case (isNotInt a || isNotInt b) of
            True                        -> (Error (PrimBin binOp1 exp1 exp2), n)
            False                       -> (PrimBin binOp1 exp1 exp2, n)
    | otherwise                         = (Error (Error(PrimBin binOp1 exp1 exp2)), n)
    










--DOWN META-LEVEL EVALUATION:

evalFCDL :: (Expr, FreshCounter) -> (Expr, FreshCounter)

evalFCDL ((VarRep string1), n)              = (Error (DownA (VarRep string1)), n)
evalFCDL (Var name1, n)                     = (Error (DownA (Var name1)), n)
evalFCDL (Lam name1 exp1, n)                = (Error (DownA (Lam name1 exp1)), n)
evalFCDL (App exp1 exp2, n)                 = (Error (DownA (App exp1 exp2)), n)
evalFCDL (LitN int1, n)                     = (Error (DownA (LitN int1)), n)
evalFCDL (LitB bool1, n)                    = (Error (DownA (LitB bool1)), n)
evalFCDL (PrimUni uniOp1 exp1, n)           = (Error (DownA (PrimUni uniOp1 exp1)), n)
evalFCDL (PrimBin binOp1 exp1 exp2, n)      = (Error (DownA (PrimBin binOp1 exp1 exp2)), n)
evalFCDL (If bool1 exp1 exp2, n)            = (Error (DownA (If bool1 exp1 exp2)), n)
evalFCDL (Let name1 exp1 exp2, n)           = (Error (DownA (Let name1 exp1 exp2)), n)
evalFCDL (LetRec name1 exp1 exp2, n)        = (Error (DownA (LetRec name1 exp1 exp2)), n)
evalFCDL (AST [], n)                        = (Error (DownA (AST [])), n)
evalFCDL (AST [TagExpr TVarRep, ulexp1], n) = case ulexp1 of
    VarRep string1                          -> (VarRep string1, newn)
    otherwise                               -> (Error (DownA (AST [TagExpr TVarRep, ulexp1])), newn)
    where (exp1, newn)                      = evalFCDL (ulexp1, n)
evalFCDL (AST [TagExpr TVar, ulname1], n) 
    | areNotError [ulname1]                   = case ulname1 of
        AST [TagExpr TVarRep, VarRep name2] -> (Var name2, n)
        otherwise                           -> case name1 of
            VarRep n2                       -> (Var n2, newn)
            otherwise                       -> (Error (DownA (AST [TagExpr TVar, Error name1])), newn)
    | otherwise                             = (Error (DownA (AST [TagExpr TVar, ulname1])), n)
    where (name1, newn)                     = evalFCDL (ulname1, n)
evalFCDL (ast@(AST [TagExpr TLam, ulname1, ulexp1]), n) 
    | areNotError [name1, exp1]             = case name1 of
        Var name1'                          -> (Lam name1' exp1, newn)
        otherwise                           -> (Error (DownA (AST [TagExpr TLam, Error ulname1, ulexp1])), newn)
    | otherwise                             = (Error (DownA ast), newn)     
    where ([name1, exp1], newn)             = evalFCDLList ([ulname1, ulexp1], n)
evalFCDL (ast@(AST [TagExpr TApp, ulexp1, ulexp2]), n) 
    | areNotError [exp1, exp2]              = (App exp1 exp2 , newn)
    | otherwise                             = (Error (DownA ast), newn)
    where ([exp1, exp2], newn)              = evalFCDLList ([ulexp1, ulexp2], n)
evalFCDL (AST [TagExpr TLitN, ulexp1], n)   = case ulexp1 of
    LitN numb1                              -> (LitN numb1, n)
    otherwise                               -> (Error (DownA (AST [TagExpr TLitN, Error ulexp1])), n)
evalFCDL (AST [TagExpr TLitB, ulexp1], n)   = case ulexp1 of
    LitB bool1                              -> (LitB bool1, n)
    otherwise                               -> (Error (DownA (AST [TagExpr TLitB, Error ulexp1])), n)
evalFCDL (AST [TagExpr (TPrimUni uniOp1), ulexp1], n) 
    | areNotError [exp1]                    = ((PrimUni uniOp1 exp1), newn)
    | otherwise                             = (Error (DownA (AST [TagExpr (TPrimUni uniOp1), exp1])), n)
    where ([exp1], newn)                    = evalFCDLList ([ulexp1], n)
evalFCDL (AST [TagExpr (TPrimBin binOp1), ulexp1, ulexp2], n) 
    | areNotError [ exp1, exp2]             = (PrimBin binOp1 exp1 exp2, newn)
    | otherwise                             = (Error (DownA (AST [TagExpr (TPrimBin binOp1), exp1, exp2])), newn) 
    where ([exp1, exp2], newn)              = evalFCDLList ([ulexp1, ulexp2], n)
evalFCDL (AST [TagExpr TIf, ulbool1, ulexp1, ulexp2], n) 
    | areNotError [bool1, exp1, exp2]       = (If bool1 exp1 exp2, newn)
    | otherwise                             = (Error (DownA (AST [TagExpr TIf, bool1, exp1, exp2])), newn) 
    where ([bool1, exp1, exp2], newn)       = evalFCDLList ([ulbool1, ulexp1, ulexp2], n)
evalFCDL (AST [TagExpr TLet, ulname1, ulexp1, ulexp2], n) 
    | areNotError [name1, exp1, exp2]       = case name1 of
        (Var name1')                        -> (Let name1' exp1 exp2, newn)
        otherwise                           -> (Error (DownA (AST [TagExpr TLet, Error name1, exp1, exp2])), newn) 
    | otherwise                             = (Error (DownA (AST [TagExpr TLet, name1, exp1, exp2])), newn) 
    where ([name1, exp1, exp2], newn)       = evalFCDLList ([ulname1, ulexp1, ulexp2], n)
evalFCDL (AST [TagExpr TLetRec, ulname1, ulexp1, ulexp2], n) 
    | areNotError [name1, exp1, exp2]       = case name1 of
        (Var name1')                        -> (LetRec name1' exp1 exp2, newn)
        otherwise                           -> (Error (DownA (AST [TagExpr TLetRec, Error name1, exp1, exp2])), newn) 
    | otherwise                             = (Error (DownA (AST [TagExpr TLetRec, name1, exp1, exp2])), newn) 
    where ([name1, exp1, exp2], newn)       = evalFCDLList ([ulname1, ulexp1, ulexp2], n)
evalFCDL (AST [(TagExpr TGenSym)], n)         = (GenSym, n)
evalFCDL (AST ((TagExpr TPromote):ulxs), n) 
    | areNotError xs                        = (AST xs, newn)
    | otherwise                             = (Error (DownA (AST ((TagExpr TPromote):xs))), newn)
    where (xs, newn)                        = evalFCDLList    (ulxs, n)
-- evalFCDL (AST [TagExpr TUpA, ulexp1], n) 
    -- | areNotError [exp1]                 = (UpA exp1, newn)
    -- | otherwise                          = (Error (DownA (AST [TagExpr TUpA, exp1])), newn)
    -- where ([exp1], newn)                 = evalFCDLList    ([ulexp1], n)
-- evalFCDL (AST [TagExpr TDownA, ulexp1], n) 
    -- | areNotError [exp1]                 = (DownA exp1, newn)
    -- | otherwise                          = (Error (DownA (AST [TagExpr TDownA, exp1])), newn)
    -- where ([exp1], newn)                 = evalFCDLList    ([ulexp1], n)
evalFCDL (AST [TagExpr TEval, ulexp1], n)
    | areNotError [exp1]                    = (Eval exp1, newn)
    | otherwise                             = (Error (DownA (AST [TagExpr TEval, exp1])), newn)
    where (exp1, newn)                      = evalFCDL (ulexp1, n)
evalFCDL (AST [TagExpr (TEvalA t1), ulexp1], n)
    | areNotError [exp1]                    = (EvalA t1 exp1, newn)
    | otherwise                             = (Error (DownA (AST [TagExpr (TEvalA t1), exp1])), newn)
    where (exp1, newn)                      = evalFCDL (ulexp1, n)
-- evalFCDL (AST [TagExpr TLift, ulexp1], n)
    -- | areNotError [exp1]                 = (Lift exp1, newn)
    -- | otherwise                          = (Error (AST [TagExpr TLift, exp1]), newn)
    -- where (exp1, newn)                   = evalFCDL (ulexp1, n)
-- evalFCDL (AST [TagExpr TLetDA, ulname1, ulexp1, ulexp2], n)
    -- | areNotError [name1', exp1, exp2]   = case name1' of
        -- (Var name1)                      -> (LetDA name1 exp1 exp2, newn)
        -- otherwise                        -> (Error (AST [TagExpr TLetDA, Error name1', exp1, exp2]), newn)
    -- | otherwise                          = (Error (AST [TagExpr TLetDA, name1', exp1, exp2]), newn)
    -- where (exp1, newn)                   = evalFCDL (ulexp1, n)        
evalFCDL (GenSym, n) = (Error GenSym, n)
evalFCDL (DownA exp1, n) 
    | areNotError [newDLexp1, newDLDLexp1]  = (newDLDLexp1, newn)
    | areNotError [newDLexp1]               = (Error (DownA newDLexp1), newn)
    | otherwise                             = (Error (DownA newDLDLexp1), newn)
    where (newDLexp1, newn1)                = evalFCDL (exp1, n)
          (newDLDLexp1, newn)               = evalFCDL (newDLexp1, newn1)
evalFCDL (UpA exp1, n) 
    | isNotError newexp1                    = (newexp1, newn)
    | otherwise                             = (Error newexp1, newn)
    where (ulexp1, newn')                   = evalFCUL (exp1, n)
          (newexp1, newn)                   = evalFCDL (ulexp1, newn')
evalFCDL (Eval exp1, n)                     = (Error (DownA (Eval exp1)), n)
-- evalFCDL (Lift exp1, n)                  = (Error (DownA (Lift exp1)), n)
evalFCDL (LetDA name1 exp1 exp2, n)         = (Error (DownA (LetDA name1 exp1 exp2)), n)
evalFCDL (TagExpr tag1, n)                  = (TagExpr tag1, n)
evalFCDL (Error exp1, n)                    = (Error (Error (exp1)), n)
evalFCDL (ErrorLoop exp1, n)                = (ErrorLoop (Error (exp1)), n)
    
evalFCDLList :: ([Expr], FreshCounter) -> ([Expr], FreshCounter)
evalFCDLList ([], n)        = ([], n)
evalFCDLList ((x:xs), n)    = ((newx:newxs), newn)
    where (newx, n')        = evalFCDL (x, n)
          (newxs, newn)     = evalFCDLList (xs, n')


          
          
          
          
          
          
          
          

--UP META-LEVEL EVALUATION:
evalFCUL :: (Expr, FreshCounter) -> (Expr, FreshCounter)
evalFCUL (VarRep string1, n)    = (AST [(TagExpr TVarRep), (VarRep string1)], n)
evalFCUL (Var name1, n)         = (AST [(TagExpr TVar), AST [TagExpr TVarRep, VarRep name1] ], n)
evalFCUL (Lam name1 exp1, n) 
    | areNotError ulList        = (AST ((TagExpr TLam):ulList), newn)
    | otherwise                 = (Error (UpA (Lam name1 exp1)), n)
    where (ulList, newn) = evalFCULList ([Var name1, exp1], n)
evalFCUL (App exp1 exp2, n) 
    | areNotError ulList        = (AST ((TagExpr TApp):ulList), newn)
    | otherwise                 = (Error (UpA (App exp1 exp2)), n)
    where (ulList, newn)        = evalFCULList ([exp1, exp2], n)
evalFCUL (LitN int1, n)         = (AST [TagExpr TLitN, LitN int1], n)
evalFCUL (LitB bool1, n)        = (AST [TagExpr TLitB, LitB bool1], n)
evalFCUL (PrimUni uniOp1 exp1, n) 
    | areNotError [ulexp1]      = (AST [TagExpr (TPrimUni uniOp1), ulexp1], newn)
    | otherwise                 = (Error (UpA (PrimUni uniOp1 exp1)), n) 
    where ([ulexp1], newn)      = evalFCULList ([exp1], n)
evalFCUL (PrimBin binOp1 exp1 exp2, n) 
    | areNotError ulList        = (AST ((TagExpr (TPrimBin binOp1)):ulList), newn)
    | otherwise                 = (Error (UpA (PrimBin binOp1 exp1 exp2)), n) 
    where (ulList, newn)        = evalFCULList ([exp1, exp2], n)
evalFCUL (If bool1 exp1 exp2, n) 
    | areNotError ulList        = (AST ((TagExpr TIf):ulList), newn)
    | otherwise                 = (Error (UpA (If bool1 exp1 exp2)), n) 
    where (ulList, newn)        = evalFCULList ([bool1, exp1, exp2], n)
evalFCUL (Let name1 exp1 exp2, n) 
    | areNotError ulList        = (AST ((TagExpr TLet):ulList), newn)
    | otherwise                 = (Error (UpA (Let name1 exp1 exp2)), n) 
    where (ulList, newn) = evalFCULList ([Var name1, exp1, exp2], n)
evalFCUL (LetRec name1 exp1 exp2, n) 
    | areNotError ulList        = (AST ((TagExpr TLetRec): ulList), newn)
    | otherwise                 = (Error (UpA (LetRec name1 exp1 exp2)), n)
    where (ulList, newn)        = evalFCULList ([Var name1, exp1, exp2], n)
evalFCUL (AST [], n)            = (Error (UpA (AST [])), n)
evalFCUL (AST expL, n) 
    | areNotError ulexpL        = (AST ((TagExpr TPromote) :  ulexpL), newn)
    | otherwise                 = (Error (UpA (AST expL)), n) 
    where (ulexpL, newn)        = evalFCULList (expL, n)
evalFCUL (GenSym, n)            = (AST [TagExpr TGenSym], n)
evalFCUL (DownA  exp1, n) 
    | areNotError [uldlexp1]    = (uldlexp1, newn)
    | otherwise                 = (Error (UpA (DownA  exp1)), n) 
    where (dlexp1, newn')       = evalFCDL (exp1, n)
          (uldlexp1, newn)      = evalFCUL (dlexp1, newn')
evalFCUL (UpA exp1, n) 
    | areNotError [exp1]        = evalFCUL $ evalFCUL (exp1, n)
    | otherwise                 = (Error (UpA (UpA exp1)), n) 
evalFCUL (Eval exp1, n)
    | areNotError [exp1]        = (AST [TagExpr TEval, ulexp1], newn)
    | otherwise                 = (Error (UpA (Eval exp1)), n)
    where (ulexp1, newn)        = evalFCUL (exp1, n)
evalFCUL (EvalA t1 exp1, n)
    | areNotError [exp1]        = (AST [TagExpr (TEvalA t1), ulexp1], newn)
    | otherwise                 = (Error (UpA (Eval exp1)), n)
    where (ulexp1, newn)        = evalFCUL (exp1, n)
-- evalFCUL (Lift exp1, n) = (AST [TagExpr TLift, ulexp1], newn)
    -- where (ulexp1, newn) = evalFCUL (exp1, n) 
evalFCUL (LetDA name1 exp1 exp2, n) 
    | areNotError [exp1, exp2]  = (Error (UpA (LetDA  name1 exp1 exp2)), n)
    | otherwise                 = (Error (UpA (LetDA name1 exp1 exp2)), n) 
evalFCUL (TagExpr tag, n)       = (TagExpr tag, n)
evalFCUL (Error exp1, n)        = (Error (UpA exp1), n)
evalFCUL (ErrorLoop exp1, n)    = (ErrorLoop (UpA exp1), n)
    
    
evalFCULList :: ([Expr], FreshCounter) -> ([Expr], FreshCounter)
evalFCULList ([], n) = ([], n)
evalFCULList ((x:xs), n) = ((newx:newxs), newn)
    where (newx, n')    = evalFCUL (x, n)
          (newxs, newn) = evalFCULList (xs, n')




    

--Substitution function:
sub :: (Expr, Name)  -> (Expr, FreshCounter) -> (Expr, FreshCounter)

--Variable Representaiton:
sub (expRep, nameRep) (VarRep varRepName, n)    = (VarRep varRepName, n) 
--Variable :
sub (expRep, nameRep) (Var nameInit, n) 
    | (nameRep == nameInit)                     = (expRep, n)
    | (nameRep /= nameInit)                     = (Var nameInit, n)    
--Lambda :
sub (eN, x) (Lam y eM, n) 
    | (x == y)                                  = (Lam y eM, n)
    | noInterference                            = (Lam y neweM, newn)
    | interference                              = (sub (eN, x) (Lam freshz neweM', newn' +1))
    where freshz                = "freshz"++ show n
          (neweM, newn)         = sub (eN, x) (eM, n)
          (neweM', newn')       = (sub (Var freshz, y) (eM, n))
          noInterference        = (x /= y) && ((x `notElem` (fv eM)) ||  (y  `notElem` (fv eN)))
          interference          = (x /= y) && (x `elem` (fv eM)) &&  (y  `elem` (fv eN))
--Application :          
sub (eN, x) (App exp1 exp2, n)                  = (App newexp1 newexp2, newn)
    where ([newexp1, newexp2], newn)            = subListExp (eN, x) ([exp1, exp2], n)
--Literal Numbers :
sub (eN, x) (LitN numb, n)                      = (LitN numb, n)
--Literal Booleans :
sub (eN, x) (LitB bool, n)                      = (LitB bool, n)
--PrimUni:
sub (eN, x) (PrimUni Not bool, n)               = (PrimUni Not newbool, newn) 
    where (newbool, newn)                       = sub (eN, x) (bool, n)
--PrimBin :
sub (eN, x) (PrimBin binOp1 exp1 exp2, n)       = (PrimBin binOp1 newexp1 newexp2, newn)
    where ([newexp1, newexp2], newn)            = subListExp (eN, x) ([exp1, exp2], n)
--If then else :
sub (eN, x) (If bool1 exp1 exp2, n)             = (If newbool1 newexp1 newexp2, newn)
    where ([newbool1, newexp1, newexp2], newn)  = subListExp (eN, x) ([bool1, exp1, exp2], n)
--Let :
sub (eN, x) (Let name1 exp1 exp2, n)            = (Let name1 newexp1 newexp2, newn)
    where ([newexp1, newexp2], newn)            = subListExp (eN, x) ([exp1, exp2], n)
--LetRec :
sub (eN, x) (LetRec name1 exp1 exp2, n)         = (LetRec name1 newexp1 newexp2, newn)
    where ([newexp1, newexp2], newn)            = subListExp (eN, x) ([exp1, exp2], n)
--AST :
sub (eN, x) (AST exp1L, n)                      = (AST newexp1L, newn)
    where (newexp1L, newn)                      = subListExp (eN, x) (exp1L, n)
--GenSym :
sub (eN, x) (GenSym, n)                         = (GenSym, n)
--DownA :
sub (eN, x) (DownA exp1, n)                     = (DownA newexp1, newn)
    where (newexp1, newn)                       = sub (eN, x) (exp1, n)
-- UpA :
sub (eN, x) (UpA exp1, n)                       = (UpA newexp1, newn)
    where (newexp1, newn)                       = sub (eN, x) (exp1, n)
--Eval :
sub (eN, x) (Eval exp1, n)                      = (Eval newexp1, newn)
    where (newexp1, newn)                       = sub (eN, x) (exp1, n)
sub (eN, x) (EvalA  t1 exp1, n)                 = (EvalA t1 newexp1, newn)
    where (newexp1, newn)                       = sub (eN, x) (exp1, n)
    -- -- Lift :
-- sub (eN, x) (Lift exp1, n)                   = (Lift newexp1, newn)
    -- where (newexp1, newn)                    = sub (eN, x) (exp1, n)
-- LetDA :
sub (eN, x) (LetDA name1 exp1 exp2, n)          = (LetDA name1 newexp1 newexp2, newn)
    where ([newexp1, newexp2], newn)                      = subListExp (eN, x) ([exp1, exp2], n)
          
--TagExpr :
sub (eN, x) (TagExpr tag1, n)                   = (TagExpr tag1, n)          
--Error :          
sub (eN, x) (Error exp1, n)                     = (Error exp1, n)
sub (eN, x) (ErrorLoop exp1, n)                 = (ErrorLoop exp1, n)
sub (eN, x) (other, n)                          = error "error in substitution"





subListSub :: [(Expr, Name)] -> (Expr, FreshCounter) -> (Expr, FreshCounter)
subListSub [] expFC     = expFC
subListSub (x:xs) expFC = sub x (subListSub xs expFC)
          
subListExp :: (Expr, Name) -> ([Expr], FreshCounter) -> ([Expr], FreshCounter)
subListExp sub1 ([], n)     =  ([], n)
subListExp sub1 ((x:xs), n) = ((newx:newxs), newn)
    where (newx, newn')     = (sub sub1 (x, n))
          (newxs, newn)     = (subListExp sub1 (xs, newn'))
    
    --test:
    --sub (Var "r", "m") (Lam "r" (Var "m"), 1)
    -- sub (Var "p", "m") (sub (Var "r", "m") (Lam "r" (Lam "p" (Var "m")), 1))





isTagLengthValid :: [Expr] -> Bool 
isTagLengthValid [TagExpr TGenSym]                  = True
isTagLengthValid ((TagExpr TPromote):newxs)         = True
isTagLengthValid [(TagExpr tag1), exp1]             = elem tag1 tagLength1
isTagLengthValid [(TagExpr tag2), exp1, exp2]       = elem tag2 tagLength2
isTagLengthValid [(TagExpr tag3), exp1, exp2, exp3] = elem tag3 tagLength3
isTagLengthValid _                                  = False

    
--These are used to determine if an expression is a boolean particularly in If ...

--is a boolean after evaluation    
isBooleval :: (Expr, FreshCounter) -> Bool
isBooleval (e, n) = case fst (evalFCRT (e, n)) of
    (LitB bool) -> True
    _           -> False
    
--is a boolean
isBool :: Expr -> Bool
isBool (LitB bool1) = True
isBool _            = False
--is a boolean and is True
isBoolT :: Expr -> Bool
isBoolT (LitB True) = True
isBoolT _           = False
--is a boolean and is False
isBoolF :: Expr -> Bool
isBoolF (LitB False)    = True
isBoolF _               = False

--is an Integer after evaluation
isInteval :: (Expr, FreshCounter) -> Bool
isInteval (e, n) = case fst (evalFCRT (e, n)) of
    (LitN int)  -> True
    _           -> False
--is an Integer    
isInt :: Expr -> Bool
isInt (LitN a)  = True
isInt _         = False


isError :: Expr -> Bool
isError (Error exp1)    = True
isError (ErrorLoop exp1)= True
isError _               = False

isNotError :: Expr -> Bool
isNotError (Error exp1)     = False
isNotError (ErrorLoop exp1) = False
isNotError _                = True

areNotError :: [Expr] -> Bool
areNotError []      = True
areNotError (x:xs)  = isNotError x && areNotError xs

isNotBool :: Expr -> Bool
isNotBool exp1 = case exp1 of
    VarRep name1            -> True
    Var name1               -> False
    Lam name1 exp1          -> True
    App exp1 exp2           -> False
    LitB bool               -> False
    LitN numb               -> True
    PrimUni Not exp1        -> False
    -- PrimUni a exp1       -> True
    PrimBin binOp1 exp1 exp2 -> notElem binOp1 (binOpBool ++ binOpIntBool)
    If exp1 exp2 exp3       -> False
    Let name1 exp1 exp2     -> False
    LetRec name1 exp1 exp2  -> False
    AST xs                  -> True
    GenSym                  -> False
    DownA exp1              -> False
    UpA exp1                -> True
    Eval exp1               -> False
    EvalA t1 exp1           -> False
    TagExpr tag1            -> True
    Error exp1              -> True
    ErrorLoop exp1          -> True
    -- other                -> False
    

isNotInt :: Expr -> Bool
isNotInt exp1 = case exp1 of
    VarRep name1                -> True
    Var name1                   -> False
    Lam name1 exp1              -> True
    App exp1 exp2               -> False
    LitB bool                   -> True
    LitN numb                   -> False
    PrimUni Not exp1            -> True
    -- PrimUni a exp1           -> True
    PrimBin binOp1 exp1 exp2    -> notElem binOp1 [Add, Mul, Min, Div] 
    If exp1 exp2 exp3           -> False
    Let name1 exp1 exp2         -> False
    LetRec name1 exp1 exp2      -> False
    AST xs                      -> True
    GenSym                      -> False
    DownA exp1                  -> False
    UpA exp1                    -> True
    Eval exp1                   -> False
    EvalA t1 exp1               -> False
    TagExpr tag1                -> True
    Error exp1                  -> True
    ErrorLoop exp1              -> True
    -- other                    -> False
    

isTag :: Expr -> Bool
isTag (TagExpr tag) = True
isTag _             = False

tagExprToTag :: Expr -> Tag
tagExprToTag (TagExpr tag1) = tag1
tagExprToTag _              = error "expression not a tag"

  
    
--checks whether an expression is a value or just an expression.    
isValue :: Expr -> Bool
isValue (Var name1)             = True
isValue (Lam name1 exp1)        = True
isValue (App exp1 exp2)         = not ((isValue exp1) && (isValue exp2)) -- ???
isValue (LitN int1)             = True
isValue (LitB bool1)            = True
isValue (PrimUni not1 bool1)    = not (isBool bool1)
isValue (PrimBin binOp1 a1 a2)  = not ((isInt a1) && (isInt a2))
isValue (If bool1 exp1 exp2)    = ((not ((isBoolT bool1) && (isValue exp1))) || (not ((isBoolF bool1) && (isValue exp2))))
isValue (Let name1 exp1 exp2)   = False
isValue (LetRec name1 exp1 exp2) = False
isValue (AST exp1)              = True
isValue GenSym                  = False
isValue (DownA exp1)            = True
isValue (UpA exp1)              = True
isValue (Eval exp1)             = False
isValue (EvalA t1 exp1)             = False
-- isValue (Lift exp1)          = False??????????????????
isValue (LetDA name1 exp1 exp2) = False
isValue (TagExpr tag1)          = False
isValue (Error exp1)            = False   
isValue (ErrorLoop exp1)        = False    
    
   

   
-- Test:    

-- testFalseAll ::[(String, Bool)]
-- testFalseAll = testFalseRT ++ testFalseCT ++ testFalseULDL


newTest1 :: (Expr, Expr, Expr, Expr, Expr, Expr, Expr, String) -> (String, Bool, Bool, Bool, Bool, Bool, Bool, Bool)
newTest1 (eExp, rExpCT, rExpRT, rExpCTRT, rExpDL, rExpUL, rExpULDL, string1) = (string1, (rExpCT == evalCT eExp), (rExpRT == evalRT eExp), (rExpCTRT == evalCTRT eExp), (rExpDL == evalDL eExp), (rExpUL == evalUL eExp), (rExpULDL == evalULDL eExp), (evalCTULDL eExp == evalCT eExp))

-- tVar1 = (eVar1, rVar1CT, rVar1RT, rVar1CTRT, rVar1DL, rVar1UL ,rVar1ULDL, "tVar1")


newTest :: [(Expr, Expr, Expr, Expr, Expr, Expr, Expr, String)] -> [(String, Bool, Bool, Bool, Bool, Bool, Bool, Bool)]
newTest xs = map newTest1 xs

testAnyFalse (s, b1, b2, b3, b4, b5, b6, b7) = not $ and [b1, b2, b3, b4, b5, b6, b7]


filterAll = filter testAnyFalse (newTest ALL.tAll)
filterAST = filter testAnyFalse (newTest ALL.tAST)

--function to test UL DL :


funcULDL1 :: Expr
funcULDL1 = Lam "y" (DownA (UpA (Var "y")))

funcUL1 :: Expr
funcUL1 = Lam "y" (UpA (Var "y"))


funcXTimes :: Expr -> Expr -> Expr -> Expr
funcXTimes func count exp = Let "exp" exp (Let "count" count (Let "func" func (LetRec "rec" (Lam "count1" (If (PrimBin Lt (LitN 0) (Var "count1")) (App (Var "func") (App (Var "rec") (PrimBin Min (Var "count1") (LitN 1)))) (Var "exp"))) (App (Var "rec") (Var "count")))))

    


-- testEvalListULDL :: [TestTriple] -> [(String, Bool)]
-- testEvalListULDL xs = map testEvalSingleTripleULDL xs

-- testEvalSingleTripleULDL :: (Expr, Expr, String) -> (String, Bool)
-- testEvalSingleTripleULDL (e1, r1, str1) 
    -- | evalULDLe1 == e1 = (str1, True)
    -- | (length (show evalULDLe1) > limitx) = ("Loops " ++ str1, False)
    -- | evalULDLe1 /= e1 && containsUpADownA e1 = ("Contains UpA or DownA " ++ str1, False)
    -- | evalULDLe1 /= e1 = (str1, False)
    -- -- | otherwise = error
    -- where evalULDLe1 = evalDL $ evalUL e1    

-- testFalseULDL :: [(String, Bool)]    
-- testFalseULDL = filter (\(str, bool) -> (bool == False)) (testEvalListULDL (RT.tAllRT ++ CT.tAllCT))

-- containsUpADownA :: Expr -> Bool
-- containsUpADownA exp1 = case exp1 of
    -- VarRep name1            -> False
    -- Var name1               -> False
    -- Lam name1 exp1          -> containsUpADownA exp1
    -- App exp1 exp2           -> containsUpADownA exp1 || containsUpADownA exp2
    -- LitN int1               -> False
    -- LitB bool1              -> False
    -- PrimUni uniOp1 exp1     -> containsUpADownA exp1
    -- PrimBin binOp1 exp1 exp2 -> containsUpADownA exp1 || containsUpADownA exp2
    -- If exp1 exp2 exp3       -> containsUpADownA exp1 || containsUpADownA exp2 || containsUpADownA exp3
    -- Let name1 exp1 exp2     -> containsUpADownA exp1 || containsUpADownA exp2
    -- LetRec name1 exp1 exp2  -> containsUpADownA exp1 || containsUpADownA exp2
    -- AST exprL               -> and $ map containsUpADownA  exprL
    -- DownA exp1              -> True
    -- UpA exp1                -> True
    -- Eval exp1               -> containsUpADownA exp1
    -- LetDA name1 exp1 exp2   -> containsUpADownA exp1 || containsUpADownA exp2
    -- TagExpr tag1            -> False
    -- Error exp1              -> False

    
    
-- DL :

-- testEvalListDL :: [TestTriple] -> [(String, Bool)]    
-- testEvalListDL xs = map testEvalSingleTripleDL xs

-- testFalseDL :: [(String, Bool)]
-- testFalseDL = filter  (\(str, bool) -> (bool == False)) (testEvalListDL DL.tAllDL)

-- testEvalSingleTripleDL :: (Expr, Expr, String) -> (String, Bool)
-- testEvalSingleTripleDL (e1, r1, str1) 
    -- | evalDLe1 == r1 = (str1, True)
    -- | evalDLe1 /= r1 = (str1, False)
    -- -- | otherwise = error
    -- where evalDLe1 = evalDL e1    
    
    
-- --RT :    
    
-- testEvalListRT :: [TestTriple] -> [(String, Bool)]    
-- testEvalListRT xs = map testEvalSingleTripleRT xs

-- testFalseRT :: [(String, Bool)]    
-- testFalseRT = filter (\(str, bool) -> (bool == False)) (testEvalListRT RT.tAllRT)

-- testEvalSingleTripleRT :: (Expr, Expr, String) -> (String, Bool)
-- testEvalSingleTripleRT (e1, r1, str1) 
    -- | evalRTe1 == r1                    = (str1, True)
    -- | length (show evalRTe1) > limitx   = ("Loops " ++ str1, False)
    -- | evalRTe1 /= r1                    = (str1, False)
    -- -- | otherwise = error
    -- where evalRTe1 = evalRT e1    
    
-- --CT:
-- testEvalListCT :: [TestTriple] -> [(String, Bool)]    
-- testEvalListCT xs = map testEvalSingleTripleCT xs

-- testFalseCT :: [(String, Bool)]    
-- testFalseCT = filter (\(str, bool) -> (bool == False)) (testEvalListCT CT.tAllCT)

filterFalse ::  [(String, Bool)] ->  [(String, Bool)]
filterFalse = filter (\(str, bool) -> (bool == False))

-- testEvalSingleTripleCT :: (Expr, Expr, String) -> (String, Bool)
-- testEvalSingleTripleCT (e1, r1, str1) 
    -- | evalCTe1 == r1                    = (str1, True)
    -- | length (show evalCTe1) > limitx   = ("Loops " ++ str1, False)
    -- | evalCTe1 /= r1                    = (str1, False)
    -- -- | otherwise = error
    -- where evalCTe1 = evalCT e1    
    
-- testEval :: [TestTriple] -> (Expr -> Expr) -> [(String, String, String)]
-- testEval testtriple evalPro = map (\(x, y, z) -> (z, show y, show $ evalPro y)) testtriple
    
testValueList :: [TestTriple] -> [(String, Bool)]
testValueList xs = map (\(e, r, st) -> (st, isValue r)) xs    
    