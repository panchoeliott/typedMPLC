
    
module EvalMPLC where
  -- runEval
-- ) where

--syntax could be written here
import SyntaxMPLC
import Data.List
import Data.Maybe
import System.IO  
import TestALL as ALL
import TypeCheckerUnParam

--evaluation at compile time then run-time (lambda)
evalCTRT :: Expr -> Expr
evalCTRT e1 = fst (evalFCRT $ evalFCCT (e1, 1))

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


typeExpCT :: Expr -> Maybe Type 
typeExpCT e = applyUnifyCon' $ genCon $ evalCT e












--COMPILE-TIME EVALUATION:

evalFCCT :: (Expr, FreshCounter)        -> (Expr, FreshCounter)
evalFCCT (t, n) = case t of 
    VarRep nm1                          -> (VarRep nm1, n)
    Var nm1                             -> (Var nm1, n)
    Lam nm1 e1  ->
        let (e1', n')                   = evalFCCT (e1, n)
            exp'                        = Lam nm1 e1' in
        case isNotError e1'  of
                True                    -> (exp', n')
                False                   -> (Error exp', n')
    App e1 e2   -> 
        let ([e1', e2'], n')            = evalFCCTList ([e1, e2], n) 
            exp'                        = App e1' e2' in
        case areNotError [e1', e2'] of
            True                        -> (exp', n')
            False                       -> (Error exp', n')
    LitN int1                           -> (LitN int1, n)
    LitB bool1                          -> (LitB bool1, n)
    PrimUni uOp1 e1 ->
        let  (e1', n')                  = evalFCCT (e1, n)
             exp'                       = (PrimUni uOp1 e1') in
        case isNotError e1' of 
            True                        -> (exp', n')
            False                       -> (Error exp', n')
    PrimBin bOp1 e1 e2 -> 
        let ([e1', e2'], n')            = evalFCCTList ([e1, e2], n) 
            exp'                        = PrimBin bOp1 e1' e2' in 
        case areNotError [e1', e2'] of
            True                        -> (exp', n')
            False                       -> (Error exp', n')
    If e1 e2 exp3 -> 
        let ([e1', e2', e3'], n')       = evalFCCTList ([e1, e2, exp3], n) 
            exp'                        = If e1' e2' e3' in 
        case areNotError [e1', e2', exp3] of
            True                        -> (exp', n')
            False                       -> (Error exp', n')

    Let nm1 e1 e2 ->
        let ([e1', e2'], n')            = evalFCCTList ([e1, e2], n) 
            exp'                        = Let nm1  e1' e2'    in
        case areNotError [e1', e2'] of  
            True                        -> (exp', n')
            False                       -> (Error exp', n')
    LetRec nm1 e1 e2 -> 
        let ([e1', e2'], n')            = evalFCCTList ([e1, e2], n)
            exp'                        = LetRec nm1 e1' e2' in 
        case areNotError [e1', e2'] of
            True                        -> (exp', n)
            False                       -> (Error exp', n)
    AST xs ->
        let (xs', n')                   = evalFCCTList (xs, n)
            exp'                        = AST xs' in 
        case areNotError xs' of
            True                        -> (exp', n')
            False                       -> (Error exp', n')
    GenSym                              -> (GenSym, n)       
    DownA e1    -> 
        let (e1CT, n')                  = evalFCCT (e1, n) in
        case  areNotError [e1CT] && typeExp e1CT == Just TyCode of
            True                        -> evalFCDL $ evalFCRT (e1CT, n')
            False                       -> (Error (DownA e1CT), n')
    UpA e1 -> 
        case  isNotError e1 of
            True                        -> evalFCUL (e1, n)
            False                       -> (Error (UpA e1), n)
    EvalA t1 e1 ->
        let (e1', n')                   = evalFCCT (e1,n) in 
        case isNotError e1' of
            True                        -> (EvalA t1 e1', n')
            False                       -> (Error (EvalA t1 e1'), n')
   
-- --LIFT CT
-- evalFCCT (Lift e1, n) 
    -- | isNotError e1'             = (Lift e1', n')
    -- | otherwise                      = (Error (Lift e1'), n')
    -- where (e1', n')            = evalFCCT (e1, n)
    LetDA nm1 e1 e2 ->
        let (newCTRTe1, n'')            = evalFCRT $ evalFCCT (e1, n)
            (newSubCTe2, n')            = evalFCCT $ sub (newCTRTe1, nm1) (e2, n'') in 
        case areNotError [newSubCTe2] of    
            True                        -> (newSubCTe2, n')
            False                       -> (Error newSubCTe2, n')
    TagExpr tag1                        -> (TagExpr tag1, n)
    Error exp0                          -> (Error (Error exp0), n)
    ErrorLoop exp0                      -> (Error (ErrorLoop exp0), n)
--just in case other -> error

--evaluates a list of expressions in compile time consecutively carrying the fresh counter
evalFCCTList :: ([Expr], FreshCounter) -> ([Expr], FreshCounter)
evalFCCTList ([], n)        = ([], n)
evalFCCTList ((x:xs), n)    = ((newx : newxs), n'')
    where (newx, n')        = evalFCCT (x, n)
          (newxs, n'')      = evalFCCTList (xs, n')











--RUN-TIME EVALUATION:

evalFCRT :: (Expr, FreshCounter)    -> (Expr, FreshCounter)
evalFCRT (t, n) = case t of 
    VarRep nm1                      -> (VarRep nm1, n)
    Var x                           -> (Var x, n)
--LAM RT (neta reduction)
    Lam x e1  -> 
        let (e1', n')               = (evalFCRT (e1, n))  in
        case isNotError e1' of        
        True                        -> (Lam x e1', n')
        False                       -> (Error (Lam x (Error e1)), n')
    
--APP RT (beta reduction)
-- evalFCRT (App e1 e2, n )        = case (e1, e2) of    
    -- ((Lam x eM), (eN))          -> do evalFCRT (sub (eN, x) (eM, n''))
    -- other                       -> case (e1', e2') of
        -- ((Lam x eM), eN)        -> do evalFCRT (sub (eN, x) (eM, n''))
        -- otherwise               -> ((App e1' e2'), n'')
    -- where (e1', n')             = evalFCRT (e1, n)
          -- (e2', n'')            = evalFCRT (e2, n')
    App e1 e2   ->
        let (e1', n')               = evalFCRT (e1, n)
            (e2', n'')              = evalFCRT (e2, n') in 
        case (e1', e2') of    
        ((Lam x eM), eN )           -> evalFCRT (sub (eN, x) (eM, n''))
        (e1', e2') | not $ areNotError [e1', e2'] ->
            case (e1, e2) of
                (Lam x eM, eN)      -> evalFCRT (sub (eN, x) (eM, n''))
        -- (Error e1, e2')             -> case (e1, e2) of
            -- ((Lam x eM), eN)        -> case evalFCRT (sub (eN, x) (eM, n'')) of
                -- (Error eM', n''')   -> evalFCRT (sub (eN, x) (eM, n'')) 
                -- (exp, m) | not (hasError exp)    -> (exp, m)
                -- (other, m)          -> (Error other, m)
            -- otherwise               -> (Error (App e1 e2), n'')
        -- (e1' , Error e2)            -> case (e1, e2) of
            -- ((Lam x eM), eN)        -> evalFCRT (sub (eN, x) (eM, n''))
            -- otherwise               -> (Error (App e1 e2), n'')
        ((eM), (eN))                -> (App eM eN, n'')
    LitN numb                       -> (LitN numb, n)
    LitB bool                       -> (LitB bool, n)
    PrimUni Not e1  -> 
        let (e1', n')   = evalFCRT (e1, n) in
        case e1' of
            LitB False              -> (LitB True, n')
            LitB True               -> (LitB False, n')
            LitN numb               -> (Error (PrimUni Not e1'), n)
            other                   -> ((PrimUni Not e1'), n')
    PrimBin binOp e1 e2 ->
        let ([e1', e2'], n')    = evalFCRTList ([e1,e2], n) in 
        case areNotError [e1', e2'] of  
            True                    -> evalPrimBin (PrimBin binOp e1' e2', n')
            False                   -> (Error (PrimBin binOp e1' e2'), n')
    
    If bool1 e1 e2 ->  
        let (bool1', n')            = evalFCRT (bool1, n)  
            trueNotError            = (isBoolT bool1') && (areNotError [bool1', e1])
            falseNotError           = (isBoolF bool1') && (areNotError [bool1', e2]) in 
        case  trueNotError of 
            True                    -> evalFCRT (e1, n') 
            False | falseNotError   -> evalFCRT (e2, n') 
            False | (isNotBool bool1') -> (Error (If bool1' e1 e2), n)
            otherwise               -> (If bool1' e1 e2, n') 
    Let nm1 e1 e2 -> 
        case areNotError [e1, e2] of
            True                    -> evalFCRT (sub (e1, nm1) (e2, n))--uses sub
            False                   -> (Error (Let nm1 e1 e2), n)
-- evalFCRT (exp0@(LetRec nm1 e1 e2), n)
    -- | loopsIndefiniedtly            = (ErrorLoop exp0, n)
    -- | notError && noSub             = (e2, n) 
    -- | notError                      = evalFCRT (sub (e1, nm1) (e2',n'))
    -- | otherwise                     = (Error exp0, n)
    -- where loopsIndefiniedtly        = (lengthExpr exp0) >= limitx 
          -- (e2', n')             = evalFCRT (e2, n)
          -- noSub                     = exp0 == (fst $ evalFCRT (sub (e1, nm1) (e2',n')))
          -- notError                  = areNotError [e1, e2']
-- evalFCRT (exp0@(LetRec nm1 e1 e2), n)
    -- | loopsIndefiniedtly            = (ErrorLoop exp0, n)
    -- | otherwise                     = evalFCRTLetRec (exp0, n)
    -- where (newLetRec, n')         = (evalFCRTLetRec (exp0, n))
          -- loopsIndefiniedtly        = areNotError [e1, e2] && lengthExpr newLetRec >= limitx 
    LetRec nm1 e1 e2 ->
        let (t', n')                = evalFCRTLetRec (t, n)  
            growingLoopLimit        = areNotError [e1, e2] && lengthExpr t' > limitx 
            stationaryLoop          = areNotError [e1, e2] && t' == t
            otherLoop               = areNotError [e1, e2] in
        case growingLoopLimit of
            True  ->  case t' of 
                ErrorLoop expn      -> (ErrorLoop t, n')
                other               -> (ErrorLoop other, n')
            False | stationaryLoop  ->  case grows nm1 e2 of
                True                -> (t, n')
                False               -> (e2, n)
            False | otherLoop       -> evalFCRT (t', n') 
            otherwise               -> (Error t, n)
    AST []                          -> (Error (AST []), n)
    AST (tag1 : xs) -> 
        let ((newtag1:newxs), n')   = evalFCRTList ((tag1:xs), n)
            validASTList            = areNotError (newtag1:newxs) && isTagLengthValid (newtag1:newxs) in
        case validASTList of
            True                    -> (AST (newtag1:newxs), n')
            False                   -> (Error (AST (newtag1:newxs)), n')
    GenSym                          -> (VarRep ("freshZ" ++ show n), n+1)
    DownA e1                        -> (Error (DownA e1), n)
    UpA e1                          -> (Error (UpA e1), n)
    EvalA t1 e1 ->  
        let (e1', n')               = evalFCDL $ evalFCRT (e1, n) in 
        case isNotError e1' && typeExp e1' == Just t1 of
            True                    -> evalFCRT (e1', n')
            False                   -> (Error e1', n')
    
-- --LIFT RT
-- evalFCRT (Lift e1, n) = case e1 of
    -- VarRep nm1             -> (AST [(TagExpr TVarRep), VarRep nm1], n)
    -- Var nm1                -> (AST [(TagExpr TVar), VarRep nm1], n)
    -- Lam nm1 e1           -> (AST [TagExpr TLam, n'm1, e1'], n')
    -- App e1 e2            -> (AST [TagExpr TApp, e1', e2'], n')
    -- LitN numb1               -> (AST [TagExpr TLitN, LitN numb1], n)
    -- LitB bool1               -> (AST [TagExpr TLitB, LitB bool1], n)
    -- PrimUni uniOp1 e1      -> (AST [TagExpr (TPrimUni uniOp1), e1'], n')
    -- PrimBin binOp1 e1 e2 -> (AST [TagExpr (TPrimBin binOp1), e1' e2'], n')
    -- If bool1 e1 e2       -> (AST [TagExpr TIf, bool1', e1', e2'], n')
    -- Let nm1 e1 e2      -> (AST [TagExpr TLet, n'm1, e1', e2'], n')
    -- LetRec nm1 e1 e2   -> (AST [TagExpr TLetRec, n'm1, e1', e2'], n')
    -- AST e1                 -> (AST ((TagExpr TPromote): e1'), n')
    -- DownA e1               -> (Error (Lift (DownA e1)), n)
    -- DownA e1               -> (AST [TagExpr TDownA, e1'], n')
    -- --UpA e1               -> (Error (Lift (UpA e1)), n)
    -- UpA e1                 -> (AST [TagExpr TUpA, e1'], n')
    -- EvalA t e1                -> (AST [TagExpr (TEval t), e1'], n')
    -- -- Lift e1             -> (AST [TagExpr TLift, e1'], n')
    -- LetDA e1               -> (Error e1, n)
    -- TagExpr tag1             -> (TagExpr tag1, n)
    -- Error e1               -> (Error (Lift (Error e1)), n)
    LetDA nm1 e1 e2                 ->  (Error (LetDA  nm1 e1 e2), n)
    TagExpr tag1                    -> (TagExpr tag1, n)     
    Error e1                        -> (Error e1, n)
    ErrorLoop e1                    -> (ErrorLoop e1, n)


--evaluates a list of expressions in compile time consecutively carrying the fresh counter
evalFCRTList :: ([Expr], FreshCounter) -> ([Expr], FreshCounter)
evalFCRTList ([], n)        = ([], n)
evalFCRTList ((x:xs), n)    = ((newx : newxs), n'')
    where (newx, n')        = evalFCRT (x, n)
          (newxs, n'')       = evalFCRTList (xs, n')

          
evalFCRTLetRec  ::  (Expr, FreshCounter) -> (Expr, FreshCounter)
evalFCRTLetRec (exp0@(LetRec nm1 e1 e2), n) 
    | largeLoop                     = (ErrorLoop newLetRec, n') --bound on length of infinite growth
    | stationaryLoop                = case grows nm1 e2 of
        True                        -> (newLetRec, n') -- equal but keep growing, leave LetRec 
        False                       -> (e2, n)    -- equal but does not grow, remove LetRec
    | otherLoop                     = evalFCRTLetRec (newLetRec, n') 
    | otherwise                     = (Error exp0, n)
    where (newLetRec, n')         = evalFCRTLetRec1 (exp0, n)
          largeLoop                 = areNotError [e1, e2] && ((lengthExpr newLetRec) >= limitx)
          stationaryLoop            = areNotError [e1, e2] && newLetRec == exp0
          otherLoop                 = areNotError [e1, e2]
    -- -- --maybe require alpha equivalence rather than just equivalence         
 -- evalFCRTLetRec (exp0@(LetRec nm1 e1 e2), n) 
    -- | loopsIndefiniedtly            = (ErrorLoop exp0, n)
    -- | notError && noSub             = (e2, n) 
    -- | notError                      = evalFCRT (sub (exp0, nm1) (e2',n'))
    -- | otherwise                     = (Error exp0, n)
    -- where loopsIndefiniedtly        = areNotError [e1, e2] && (lengthExpr exp0) >= limitx 
          -- (e2', n')             = evalFCRT (e2, n)
          -- noSub                     = exp0 == (fst $ evalFCRT (sub (exp0, nm1) (e2',n')))
          -- notError                  = areNotError [e1, e2']
evalFCRTLetRec (other, n)           = (Error other, n)       

 
--eval one step (substitution) of LetRec:
evalFCRTLetRec1 :: (Expr, FreshCounter) -> (Expr, FreshCounter)
evalFCRTLetRec1 (exp0@(LetRec nm1 e1 e2), n)
    | areNotError [e1', e2']    = (newLetRec, n')
    | otherwise                 = (Error newLetRec, n')
    where ([e1',e2'], n'1)      = evalFCRTList ([e1, e2], n)
          -- (newLete2, n')     = evalFCRT (Let nm1 e1' e2', n'1)
          (newLete2, n')        = (sub (e1', nm1)  (e2', n'1))
          newLetRec             = LetRec nm1 e1' newLete2
    
evalFCRTLetRec1 (exp, n) = (Error exp, n)
-- (LetRec f M N) ---> (LetRec f M (N[M/f]))

--essentially says whether variable is contained (free) in another expression.
grows :: Name -> Expr -> Bool
grows nm1 e1 = case e1 of
    Var nm2             -> nm1 == nm2
    Lam nm2 e2          -> case nm1 == nm2 of
        True            -> False 
        otherwise       -> grows nm1 e2
    App e2 e3         -> ((grows nm1 e2) || (grows nm1 e3))
    PrimUni uOp1 e2     -> grows nm1 e2
    PrimBin bOp1 e2 e3  -> ((grows nm1 e2) || (grows nm1 e3))
    If e2 e3 e4         -> ((grows nm1 e2) || (grows nm1 e3) || (grows nm1 e4))
    Let nm2 e2 e3       -> case nm1 == nm2 of
        True            -> grows nm1 e2
        False           -> ((grows nm1 e2) || (grows nm1 e3))
    LetRec nm2 e2 e3    -> case nm1 == nm2 of
        True            -> grows nm1 e2
        False           -> ((grows nm1 e2) || (grows nm1 e3))
    AST eL              -> or (map (\x -> grows nm1 x) eL)
    DownA e2            -> grows nm1 e2
    UpA e2              -> grows nm1 e2
    EvalA t e2          -> grows nm1 e2
    LetDA nm2 e2 e3     -> case nm1 == nm2 of
        True            -> grows nm1 e2
        False           -> ((grows nm1 e2) || (grows nm1 e3))
    Error e2            -> error ("Is error " ++ show e2) 
    ErrorLoop e2        -> error ("Is errorLoop " ++ show e2) 
    otherwise           -> False

          
          
          

evalPrimBin (PrimBin binOp1 e1 e2, n)
    | (elem binOp1 binOpBool) && (isNotBool e1 || isNotBool e2 ) = (Error (PrimBin binOp1 e1 e2), n)
    | (elem binOp1 binOpBool)   = case (e1, e2) of
        (LitB bool1, LitB bool2)    -> case binOp1 of
            And                         -> (LitB (bool1 && bool2), n)
            Or                          -> (LitB (bool1 || bool2), n)
            other                       -> (Error (PrimBin binOp1 e1 e2), n)
        (a, b)                      -> case (isNotBool a && isNotBool b) of
            True                        -> (Error (PrimBin binOp1 e1 e2), n)
            False                       -> (PrimBin binOp1 e1 e2, n)
    | (elem binOp1 binOpInt)    = case (e1, e2) of
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
            other                       -> (Error (PrimBin binOp1 e1 e2), n)
        (a, b)                      -> case (isNotInt a || isNotInt b) of
            True                        -> (Error (PrimBin binOp1 e1 e2), n)
            False                       -> (PrimBin binOp1 e1 e2, n)
    | otherwise                         = (Error (Error(PrimBin binOp1 e1 e2)), n)
    




    
    
    
    
    
    
--DOWN META-LEVEL EVALUATION:

evalFCDL :: (Expr, FreshCounter) -> (Expr, FreshCounter)
evalFCDL (t, n) = 
    case t of 
-- GenSym                              -> (Error (DownA t), n)
        (AST ((TagExpr tg) :ulxs))  -> 
            let (xs, n')        = evalFCDLList (ulxs, n) in 
            case areNotError xs of
                True            -> case tg of
                    TVarRep     -> case ulxs of
                        [VarRep nm1]        -> (VarRep nm1, n')
                        otherwise           -> (Error (DownA t), n')
                    TVar        -> case xs of
                        [VarRep nm1]        -> (Var nm1, n')
                        otherwise           -> (Error (DownA t), n')
                    TLam        -> case xs of
                        [Var nm1, e1]       -> (Lam nm1 e1, n')
                        otherwise           -> (Error (DownA t), n')
                    TApp        -> case xs of 
                        [e1, e2]            -> (App e1 e2, n')
                        otherwise           -> (Error (DownA t), n')
                    TLitN       -> case ulxs of
                        [LitN nb1]          -> (LitN nb1, n')
                        otherwise           -> (Error (DownA t), n')     
                    TLitB       -> case ulxs of
                        [LitB bl1]          -> (LitB bl1, n')
                        otherwise           -> (Error (DownA t), n')
                    (TPrimUni uOp1) -> case xs of 
                        [e1]                -> ((PrimUni uOp1 e1), n')
                        otherwise           -> (Error (DownA t), n')
                    (TPrimBin bOp1) -> case xs of 
                        [e1, e2]            -> ((PrimBin bOp1 e1 e2), n')
                        otherwise           -> (Error (DownA t), n')
                    TIf         -> case xs of 
                        [b1, e1, e2]        -> (If b1 e1 e2, n')
                        otherwise           -> (Error (DownA t), n')
                    TLet        -> case xs of 
                        [Var nm1, e1, e2]   -> (Let nm1 e1 e2, n')
                        otherwise           -> (Error (DownA t), n')
                    TLetRec     -> case xs of 
                        [Var nm1, e1, e2]   -> (LetRec nm1 e1 e2, n')
                        otherwise           -> (Error (DownA t), n')
                    TGenSym     -> case xs of 
                        []                  -> (GenSym, n')
                        otherwise           -> (Error (DownA t), n')
                    TPromote    -> (AST xs, n')
                    (TEvalA ty) -> case xs of
                        [e1]                -> (EvalA ty e1, n')
                        otherwise           -> (Error (DownA t), n')
                False           ->  case tg of
                    TVarRep     -> case ulxs of 
                        [VarRep nm1]        -> (VarRep nm1, n)
                        otherwise           -> (Error (DownA t), n)     
                    TLitN       -> case ulxs of
                        [LitN nb1]          -> (LitN nb1, n)
                        otherwise           -> (Error (DownA t), n)     
                    TLitB       -> case ulxs of
                        [LitB bl1]          -> (LitB bl1, n')
                        otherwise           -> (Error (DownA t), n)
                   
                    otherwise               -> (Error (DownA t), n)
        (DownA e1)  -> 
            let (e1', n')       = evalFCDL $ evalFCDL (e1, n) in
            case isNotError e1' of
                True                        -> (e1', n')
                False                       -> (Error (DownA e1'), n')
        (UpA e1)  -> 
            let (e1', n')       = evalFCDL $ evalFCUL (e1, n) in
            case isNotError e1' of
                True                        -> (e1', n')
                False                       -> (Error (DownA e1'), n')
    -- -- Lift e1                  -> (Error (DownA (Lift e1)), n)
        TagExpr tag1                        -> (TagExpr tag1, n)
        otherwise                           -> (Error (DownA t), n)
        
     
evalFCDLList :: ([Expr], FreshCounter) -> ([Expr], FreshCounter)
evalFCDLList ([], n)        = ([], n)
evalFCDLList ((x:xs), n)    = ((x':xs'), n'')
    where (x', n')          = evalFCDL (x, n)
          (xs', n'')        = evalFCDLList (xs, n')
    
          


          
          
          
          
          
          
  
--UP META-LEVEL EVALUATION:
evalFCUL :: (Expr, FreshCounter) -> (Expr, FreshCounter)
evalFCUL (t, n) = 
    case t of 
        VarRep string1          -> (AST [(TagExpr TVarRep), (VarRep string1)], n)
        Var nm1                 -> (AST [(TagExpr TVar), AST [TagExpr TVarRep, VarRep nm1] ], n)
        Lam nm1 e1          ->   
            let (ulList, n')    = evalFCULList ([Var nm1, e1], n) in 
            case areNotError ulList of
                True            -> (AST ((TagExpr TLam):ulList), n')
                False           -> (Error (UpA (Lam nm1 e1)), n)
        App e1 e2           ->
            let (ulList, n')    = evalFCULList ([e1, e2], n) in
            case areNotError ulList of
                True            -> (AST ((TagExpr TApp):ulList), n')
                False           -> (Error (UpA (App e1 e2)), n)
        LitN int1               -> (AST [TagExpr TLitN, LitN int1], n)
        LitB bool1              -> (AST [TagExpr TLitB, LitB bool1], n)
        PrimUni uniOp1 e1   -> 
            let ([ule1], n')    = evalFCULList ([e1], n) in
            case areNotError [ule1] of
                True            -> (AST [TagExpr (TPrimUni uniOp1), ule1], n')
                False           -> (Error (UpA (PrimUni uniOp1 e1)), n) 
        PrimBin binOp1 e1 e2 -> 
            let (ulList, n')    = evalFCULList ([e1, e2], n) in
            case areNotError ulList of
                True            -> (AST ((TagExpr (TPrimBin binOp1)):ulList), n')
                False           -> (Error (UpA (PrimBin binOp1 e1 e2)), n) 
        If bool1 e1 e2      -> 
            let (ulList, n')    = evalFCULList ([bool1, e1, e2], n) in
            case areNotError ulList of
                True            -> (AST ((TagExpr TIf):ulList), n')
                False           -> (Error (UpA (If bool1 e1 e2)), n) 
        Let nm1 e1 e2       ->
            let (ulList, n')    = evalFCULList ([Var nm1, e1, e2], n) in
            case areNotError ulList of
                True            -> (AST ((TagExpr TLet):ulList), n')
                False           -> (Error (UpA (Let nm1 e1 e2)), n) 
        LetRec nm1 e1 e2    ->
            let (ulList, n')    = evalFCULList ([Var nm1, e1, e2], n) in
            case areNotError ulList of
                True            -> (AST ((TagExpr TLetRec): ulList), n')
                False           -> (Error (UpA (LetRec nm1 e1 e2)), n)
        AST []                  -> (Error (UpA (AST [])), n)
        AST expL -> 
            let (ulexpL, n')    = evalFCULList (expL, n) in 
            case areNotError ulexpL of 
                True            -> (AST ((TagExpr TPromote) :  ulexpL), n')
                False           -> (Error (UpA (AST expL)), n) 
        GenSym                  -> (AST [TagExpr TGenSym], n)
        DownA  e1 ->  
            let (dle1, n'')     = evalFCDL (e1, n)
                (uldle1, n')    = evalFCUL (dle1, n'') in 
            case areNotError [uldle1] of    
                True            -> (uldle1, n')
                False           ->(Error (UpA (DownA  e1)), n) 
        UpA e1 ->
            case areNotError [e1] of
                True            -> evalFCUL $ evalFCUL (e1, n)
                False           -> (Error (UpA (UpA e1)), n) 
        EvalA t1 e1 ->
            let (ule1, n')      = evalFCUL (e1, n) in
            case areNotError [e1] of
                True            -> (AST [TagExpr (TEvalA t1), ule1], n')
                False           -> (Error (UpA (EvalA t1 e1)), n)
-- evalFCUL (Lift e1, n) = (AST [TagExpr TLift, ule1], n')
    -- where (ule1, n') = evalFCUL (e1, n) 
        LetDA nm1 e1 e2 ->
            case areNotError [e1, e2] of
                True            -> (Error (UpA (LetDA  nm1 e1 e2)), n)
                False           -> (Error (UpA (LetDA nm1 e1 e2)), n) 
        TagExpr tag             -> (TagExpr tag, n)
        Error e1                -> (Error (UpA e1), n)
        ErrorLoop e1            -> (ErrorLoop (UpA e1), n)
            
          
    
evalFCULList :: ([Expr], FreshCounter) -> ([Expr], FreshCounter)
evalFCULList ([], n) = ([], n)
evalFCULList ((x:xs), n) = ((newx:newxs), n'')
    where (newx, n')    = evalFCUL (x, n)
          (newxs, n'') = evalFCULList (xs, n')




          
          
          
          
          
      




          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
    

--Substitution function:
sub :: (Expr, Name)  -> (Expr, FreshCounter) -> (Expr, FreshCounter)

--Variable Representaiton:
sub (expRep, nameRep) (VarRep nm1, n)   = (VarRep nm1, n) 
--Variable :
sub (expRep, nameRep) (Var nameInit, n) 
    | (nameRep == nameInit)             = (expRep, n)
    | (nameRep /= nameInit)             = (Var nameInit, n)    
--Lambda :
sub (eN, x) (Lam y eM, n) 
    | (x == y)                          = (Lam y eM, n)
    | noInterference                    = (Lam y neweM, n')
    | interference                      = (sub (eN, x) (Lam freshz neweM', n'' +1))
    where freshz                        = "freshz"++ show n
          (neweM, n')                   = sub (eN, x) (eM, n)
          (neweM', n'')                 = (sub (Var freshz, y) (eM, n))
          noInterference                = (x /= y) && ((x `notElem` (fv eM)) ||  (y  `notElem` (fv eN)))
          interference                  = (x /= y) && (x `elem` (fv eM)) &&  (y  `elem` (fv eN))
--Application :          
sub (eN, x) (App e1 e2, n)              = (App e1' e2', n')
    where ([e1', e2'], n')              = subListExp (eN, x) ([e1, e2], n)
--Literal Numbers :
sub (eN, x) (LitN numb, n)              = (LitN numb, n)
--Literal Booleans :
sub (eN, x) (LitB bool, n)              = (LitB bool, n)
--PrimUni:
sub (eN, x) (PrimUni Not bool, n)       = (PrimUni Not newbool, n') 
    where (newbool, n')                 = sub (eN, x) (bool, n)
--PrimBin :
sub (eN, x) (PrimBin binOp1 e1 e2, n)   = (PrimBin binOp1 e1' e2', n')
    where ([e1', e2'], n')              = subListExp (eN, x) ([e1, e2], n)
--If then else :
sub (eN, x) (If bool1 e1 e2, n)         = (If bool1' e1' e2', n')
    where ([bool1', e1', e2'], n')      = subListExp (eN, x) ([bool1, e1, e2], n)
--Let :
sub (eN, x) (Let nm1 e1 e2, n)          = (Let nm1 e1' e2', n')
    where ([e1', e2'], n')              = subListExp (eN, x) ([e1, e2], n)
--LetRec :
sub (eN, x) (LetRec nm1 e1 e2, n)       = (LetRec nm1 e1' e2', n')
    where ([e1', e2'], n')              = subListExp (eN, x) ([e1, e2], n)
--AST :
sub (eN, x) (AST e1L, n)                = (AST e1'L, n')
    where (e1'L, n')                    = subListExp (eN, x) (e1L, n)
--GenSym :
sub (eN, x) (GenSym, n)                 = (GenSym, n)
--DownA :
sub (eN, x) (DownA e1, n)               = (DownA e1', n')
    where (e1', n')                     = sub (eN, x) (e1, n)
-- UpA :
sub (eN, x) (UpA e1, n)                 = (UpA e1', n')
    where (e1', n')                     = sub (eN, x) (e1, n)
--EvalA :
sub (eN, x) (EvalA  t1 e1, n)           = (EvalA t1 e1', n')
    where (e1', n')                     = sub (eN, x) (e1, n)
    -- -- Lift :
-- sub (eN, x) (Lift e1, n)             = (Lift e1', n')
    -- where (e1', n')                  = sub (eN, x) (e1, n)
-- LetDA :
sub (eN, x) (LetDA nm1 e1 e2, n)        = (LetDA nm1 e1' e2', n')
    where ([e1', e2'], n')              = subListExp (eN, x) ([e1, e2], n)
          
--TagExpr :
sub (eN, x) (TagExpr tag1, n)           = (TagExpr tag1, n)          
--Error :          
sub (eN, x) (Error e1, n)               = (Error e1, n)
sub (eN, x) (ErrorLoop e1, n)           = (ErrorLoop e1, n)
sub (eN, x) (other, n)                  = error "error in substitution"





subListSub :: [(Expr, Name)] -> (Expr, FreshCounter) -> (Expr, FreshCounter)
subListSub [] expFC     = expFC
subListSub (x:xs) expFC = sub x (subListSub xs expFC)
          
subListExp :: (Expr, Name) -> ([Expr], FreshCounter) -> ([Expr], FreshCounter)
subListExp sub1 ([], n)     =  ([], n)
subListExp sub1 ((x:xs), n) = ((newx:newxs), n')
    where (newx, n'')     = (sub sub1 (x, n))
          (newxs, n')     = (subListExp sub1 (xs, n''))
    
    --test:
    --sub (Var "r", "m") (Lam "r" (Var "m"), 1)
    -- sub (Var "p", "m") (sub (Var "r", "m") (Lam "r" (Lam "p" (Var "m")), 1))





isTagLengthValid :: [Expr] -> Bool 
isTagLengthValid [TagExpr TGenSym]                  = True
isTagLengthValid ((TagExpr TPromote):newxs)         = True
isTagLengthValid [(TagExpr tag1), e1] = case tag1 of 
    x | elem tag1 tagLength1                        -> True
    (TEvalA x)                                      -> True
    otherwise                                       -> False
isTagLengthValid [(TagExpr tag2), e1, e2]           = elem tag2 tagLength2
isTagLengthValid [(TagExpr tag3), e1, e2, exp3]     = elem tag3 tagLength3
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
isError (Error e1)      = True
isError (ErrorLoop e1)  = True
isError _               = False

isNotError :: Expr -> Bool
isNotError (Error e1)       = False
isNotError (ErrorLoop e1)   = False
isNotError _                = True

areNotError :: [Expr] -> Bool
areNotError []      = True
areNotError (x:xs)  = isNotError x && areNotError xs

isNotBool :: Expr -> Bool
isNotBool e1 = case e1 of
    VarRep nm1              -> True
    Var nm1                 -> False
    Lam nm1 e1              -> True
    App e1 e2               -> False
    LitB bool               -> False
    LitN numb               -> True
    PrimUni Not e1          -> False
    -- PrimUni a e1         -> True
    PrimBin binOp1 e1 e2    -> notElem binOp1 (binOpBool ++ binOpIntBool)
    If e1 e2 exp3           -> False
    Let nm1 e1 e2           -> False
    LetRec nm1 e1 e2        -> False
    AST xs                  -> True
    GenSym                  -> False
    DownA e1                -> False
    UpA e1                  -> True
    EvalA t1 e1             -> False
    TagExpr tag1            -> True
    Error e1                -> True
    ErrorLoop e1            -> True
    -- other                -> False
    

isNotInt :: Expr -> Bool
isNotInt e1 = case e1 of
    VarRep nm1                -> True
    Var nm1                   -> False
    Lam nm1 e1              -> True
    App e1 e2               -> False
    LitB bool                   -> True
    LitN numb                   -> False
    PrimUni Not e1            -> True
    -- PrimUni a e1           -> True
    PrimBin binOp1 e1 e2    -> notElem binOp1 [Add, Mul, Min, Div] 
    If e1 e2 exp3           -> False
    Let nm1 e1 e2         -> False
    LetRec nm1 e1 e2      -> False
    AST xs                      -> True
    GenSym                      -> False
    DownA e1                  -> False
    UpA e1                    -> True
    EvalA t1 e1               -> False
    TagExpr tag1                -> True
    Error e1                  -> True
    ErrorLoop e1              -> True
    -- other                    -> False
    

isTag :: Expr -> Bool
isTag (TagExpr tag) = True
isTag _             = False

tagExprToTag :: Expr -> Tag
tagExprToTag (TagExpr tag1) = tag1
tagExprToTag _              = error "expression not a tag"

  
    
--checks whether an expression is a value or just an expression.    
isValue :: Expr -> Bool
isValue (Var nm1)               = True
isValue (Lam nm1 e1)            = True
isValue (App e1 e2)             = not ((isValue e1) && (isValue e2)) -- ???
isValue (LitN int1)             = True
isValue (LitB bool1)            = True
isValue (PrimUni not1 bool1)    = not (isBool bool1)
isValue (PrimBin binOp1 a1 a2)  = not ((isInt a1) && (isInt a2))
isValue (If bool1 e1 e2)        = ((not ((isBoolT bool1) && (isValue e1))) || (not ((isBoolF bool1) && (isValue e2))))
isValue (Let nm1 e1 e2)         = False
isValue (LetRec nm1 e1 e2)      = False
isValue (AST e1)                = True
isValue GenSym                  = False
isValue (DownA e1)              = True
isValue (UpA e1)                = True
isValue (EvalA t1 e1)           = False
-- isValue (Lift e1)          = False??????????????????
isValue (LetDA nm1 e1 e2)       = False
isValue (TagExpr tag1)          = False
isValue (Error e1)              = False   
isValue (ErrorLoop e1)          = False    
    
   

   
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
    