-- Syntax test:
module TestALL where

import SyntaxMPLC
-- import EvalLC

--List of terms and expected type and name or test (number possibly)


tAll' = map annotateCT tAll

tAll = tVar ++ tLam ++ tApp ++ tLitN ++ tLitB ++ tPrimUni ++ tPrimBin  ++ tIf  ++ tLet ++ tLetRec ++ tFact ++ tFib  ++ tMartin  ++ tRandom ++ tAST  ++ tEval ++ tLetDA
tyAll = tyVar ++ tyLam ++ tyApp ++ tyLitN ++ tyLitB ++ tyPrimUni ++ tyPrimBin  ++ tyIf  ++ tyLet ++ tyLetRec ++ tyFact ++ tyFib  ++ tyMartin  ++ tyRandom ++ tyAST  ++ tyEval ++ tyLetDA

typedAll = zip tAll tyAll

annotateCT :: TestList -> (String, String, String, String, String, String, String, String)
annotateCT (a, b, c, d, e, f, g, z) = (show a, show b ++ "CT", show  c ++ "RT", show d ++ "CTRT", show e ++ "UL", show f ++ "DL", show g ++ "ULDL", z)

--TVar:
tVar :: [TestList]
tVar = [tVar1, tVar2]
tyVar = [tyVar1, tyVar2]

tVar1 = (eVar1, rVar1CT, rVar1RT, rVar1CTRT, rVar1DL, rVar1UL ,rVar1ULDL, "tVar1")
eVar1 = Var "x"
rVar1CT = Var "x"
rVar1RT = Var "x"
rVar1CTRT = Var "x"
rVar1DL = Error (DownA (Var "x"))
rVar1UL = AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]
rVar1ULDL = Var "x"
tyVar1 = TyVar "Type1{x}"

tVar2 = (eVar2, rVar2CT, rVar2RT, rVar2CTRT, rVar2DL, rVar2UL ,rVar2ULDL, "tVar2")
eVar2 = (Var "z")
rVar2CT = (Var "z")
rVar2RT = Var "z"
rVar2CTRT = Var "z"
rVar2DL = Error (DownA (Var "z"))
rVar2UL = AST [TagExpr TVar, AST [TagExpr TVarRep, VarRep "z"]]
rVar2ULDL = Var "z"
tyVar2 = TyVar "Type1{z}"
    
--Lambda:
tLam :: [TestList]
tLam = [tLam1, tLam2, tLam3, tLam4, tLam5, tLam6]    
tyLam = [tyLam1, tyLam2, tyLam3, tyLam4, tyLam5, tyLam6]     

tLam1 = (eLam1, rLam1CT, rLam1RT, rLam1CTRT, rLam1DL, rLam1UL ,rLam1ULDL, "tLam1")
eLam1 = Lam "x" (Var "x")
rLam1CT = Lam "x" (Var "x")
rLam1RT = Lam "x" (Var "x")
rLam1CTRT = Lam "x" (Var "x")
rLam1DL = Error (DownA (Lam "x" (Var "x")))
rLam1UL = AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]]
rLam1ULDL = Lam "x" (Var "x")
tyLam1 = TyFunc (TyVar "Type2{fresh}"   ) (TyVar "Type2{fresh}"   )

tLam2 = (eLam2, rLam2CT, rLam2RT, rLam2CTRT, rLam2DL, rLam2UL ,rLam2ULDL, "tLam2")
eLam2 = Lam "x" (Var "y")
rLam2CT = Lam "x" (Var "y")
rLam2RT = Lam "x" (Var "y")
rLam2CTRT = Lam "x" (Var "y")
rLam2DL = Error (DownA (Lam "x" (Var "y")))
rLam2UL = AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]
rLam2ULDL = Lam "x" (Var "y")
tyLam2 = TyFunc (TyVar "Type3{fresh}"   ) (TyVar "Type2{y}"   )

tLam3 = (eLam3, rLam3CT, rLam3RT, rLam3CTRT, rLam3DL, rLam3UL ,rLam3ULDL, "tLam3")
eLam3 = Lam "x" (Lam "y" (Var "x"))
rLam3CT = Lam "x" (Lam "y" (Var "x"))
rLam3RT = Lam "x" (Lam "y" (Var "x"))
rLam3CTRT = Lam "x" (Lam "y" (Var "x"))
rLam3DL = Error (DownA (Lam "x" (Lam "y" (Var "x"))))
rLam3UL = AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]], AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]]]
rLam3ULDL = eLam3
tyLam3 = TyFunc (TyVar "Type3{fresh}"   ) (TyFunc (TyVar "Type4{fresh}"   ) (TyVar "Type3{fresh}"   ))

tLam4 = (eLam4, rLam4CT, rLam4RT, rLam4CTRT, rLam4DL, rLam4UL ,rLam4ULDL, "tLam4")
eLam4 = Lam "x" (Lam "y" (Var "y"))
rLam4CT = Lam "x" (Lam "y" (Var "y"))
rLam4RT = Lam "x" (Lam "y" (Var "y"))
rLam4CTRT = Lam "x" (Lam "y" (Var "y"))
rLam4DL = Error (DownA (Lam "x" (Lam "y" (Var "y"))))
rLam4UL = AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]], AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]
rLam4ULDL = eLam4
tyLam4 =TyFunc (TyVar "Type3{fresh}"   ) (TyFunc (TyVar "Type4{fresh}"   ) (TyVar "Type4{fresh}"   ))

tLam5 = (eLam5, rLam5CT, rLam5RT, rLam5CTRT, rLam5DL, rLam5UL ,rLam5ULDL, "tLam5")
eLam5 = Lam "x" (Lam "y" (LitN 4))
rLam5CT = Lam "x" (Lam "y" (LitN 4))
rLam5RT = Lam "x" (Lam "y" (LitN 4))
rLam5CTRT = Lam "x" (Lam "y" (LitN 4))
rLam5DL = Error (DownA (Lam "x" (Lam "y" (LitN 4))))
rLam5UL = AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]], AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TLitN, (LitN 4)]]]
rLam5ULDL = eLam5
tyLam5 = TyFunc (TyVar "Type3{fresh}"   ) (TyFunc (TyVar "Type4{fresh}"   ) TyInt)

tLam6 = (eLam6, rLam6CT, rLam6RT, rLam6CTRT, rLam6DL, rLam6UL ,rLam6ULDL, "tLam6")
eLam6 = Lam "x" (App (Lam "y" (Var "x")) (LitN 3))
rLam6CT = Lam "x" (App (Lam "y" (Var "x")) (LitN 3))
rLam6RT = Lam "x" (Var "x")
rLam6CTRT = Lam "x" (Var "x")
rLam6DL = Error (DownA (Lam "x" (App (Lam "y" (Var "x")) (LitN 3))))
rLam6UL =   AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]],AST [TagExpr TLitN,LitN 3]]]
rLam6ULDL = eLam6
tyLam6 = TyFunc (TyVar "Type6{fresh}"   ) (TyVar "Type6{fresh}"   )

--App:
tApp :: [TestList]
tApp = [tApp1, tApp2, tApp3, tApp4, tApp5, tApp6, tApp7]
tyApp = [tyApp1, tyApp2, tyApp3, tyApp4, tyApp5, tyApp6, tyApp7]

tApp1 = (eApp1, rApp1CT, rApp1RT, rApp1CTRT, rApp1DL, rApp1UL ,rApp1ULDL, "tApp1")
eApp1 = App (Lam "y" (Var "y")) (LitN 4)
rApp1CT = App (Lam "y" (Var "y")) (LitN 4)
rApp1RT = LitN 4
rApp1CTRT = LitN 4
rApp1DL =Error (DownA (App (Lam "y" (Var "y")) (LitN 4)))
rApp1UL = AST [ TagExpr TApp, AST [TagExpr TLam, AST [TagExpr TVar, AST [ TagExpr TVarRep, VarRep "y"]], AST [TagExpr TVar, AST [ TagExpr TVarRep, VarRep "y"]]], AST [TagExpr TLitN, LitN 4]]
rApp1ULDL = App (Lam "y" (Var "y")) (LitN 4)
tyApp1 = TyInt

tApp2 = (eApp2, rApp2CT, rApp2RT, rApp2CTRT, rApp2DL, rApp2UL ,rApp2ULDL, "tApp2")
eApp2 = App (Lam "y" (Var "x")) (LitN 4)
rApp2CT = App (Lam "y" (Var "x")) (LitN 4)
rApp2RT = Var "x"
rApp2CTRT = Var "x"
rApp2DL = Error (DownA (App (Lam "y" (Var "x")) (LitN 4)))
rApp2UL = AST [ TagExpr TApp, AST [TagExpr TLam, AST [TagExpr TVar, AST [ TagExpr TVarRep, VarRep "y"]], AST [TagExpr TVar, AST [ TagExpr TVarRep, VarRep "x"]]], AST [TagExpr TLitN, LitN 4]]
rApp2ULDL = App (Lam "y" (Var "x")) (LitN 4)
tyApp2 =  TyVar "Type5{fresh}"  

tApp3 :: TestList
tApp3 = (eApp3, rApp3CT, rApp3RT, rApp3CTRT, rApp3DL, rApp3UL ,rApp3ULDL, "tApp3")
eApp3 = App (Lam "x" (App (Lam "x" (Var "x"))(Var "x"))) (LitN 4)
rApp3CT = App (Lam "x" (App (Lam "x" (Var "x"))(Var "x"))) (LitN 4)
rApp3RT = (LitN 4)
rApp3CTRT = (LitN 4)
rApp3DL = Error (DownA (eApp3))
rApp3UL =AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]]],AST [TagExpr TLitN,LitN 4]]
rApp3ULDL =   App (Lam "x" (App (Lam "x" (Var "x"))(Var "x"))) (LitN 4)
tyApp3 = TyInt

tApp4 :: TestList
tApp4 = (eApp4, rApp4CT, rApp4RT, rApp4CTRT, rApp4DL, rApp4UL ,rApp4ULDL, "tApp4")
eApp4 = App (Var "x") (Var "y")
rApp4CT = App (Var "x") (Var "y")
rApp4RT = App (Var "x") (Var "y")
rApp4CTRT = App (Var "x") (Var "y")
rApp4DL = Error (DownA (App (Var "x") (Var "y")))
rApp4UL = AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]
rApp4ULDL =  App (Var "x") (Var "y")
tyApp4 = TyVar "Type3{fresh}"  

tApp5 = (eApp5, rApp5CT, rApp5RT, rApp5CTRT, rApp5DL, rApp5UL ,rApp5ULDL, "tApp5")
eApp5 = App (Lam "x" (App (Var "x") (LitB True))) (Lam "x" (PrimUni Not (Var "x")))
rApp5CT = App (Lam "x" (App (Var "x") (LitB True))) (Lam "x" (PrimUni Not (Var "x")))
rApp5RT = LitB False
rApp5CTRT = LitB False
rApp5DL = Error (DownA (eApp5))
rApp5UL =AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLitB,LitB True]]],AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimUni Not),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]]]]
rApp5ULDL =  eApp5
tyApp5 = TyBool

tApp6 = (eApp6, rApp6CT, rApp6RT, rApp6CTRT, rApp6DL, rApp6UL ,rApp6ULDL, "tApp6")
eApp6 = App (Lam "x" (App (Lam "y" (PrimBin Add (Var "x") (Var "y"))) (LitN 4))) (LitN 7)
rApp6CT = App (Lam "x" (App (Lam "y" (PrimBin Add (Var "x") (Var "y"))) (LitN 4))) (LitN 7)
rApp6RT = LitN 11
rApp6CTRT = LitN 11
rApp6DL = Error (DownA (eApp6))
rApp6UL = AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr (TPrimBin Add),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]],AST [TagExpr TLitN,LitN 4]]],AST [TagExpr TLitN,LitN 7]]
rApp6ULDL =  eApp6
tyApp6 = TyInt

tApp7 = (eApp7, rApp7CT, rApp7RT, rApp7CTRT, rApp7DL, rApp7UL ,rApp7ULDL, "tApp7")
eApp7 = App (LitN 3) (LitN 4)
rApp7CT = App (LitN 3) (LitN 4)
rApp7RT = App (LitN 3) (LitN 4)
rApp7CTRT = App (LitN 3) (LitN 4)
rApp7DL = Error (DownA eApp7)
rApp7UL = AST [ TagExpr TApp, AST [TagExpr TLitN, LitN 3],  AST [TagExpr TLitN, LitN 4]]
rApp7ULDL =  eApp7
tyApp7 = TyErrorEq

--Literal Numbers:
tLitN :: [TestList]
tLitN = [tLitN1, tLitN2, tLitN3]
tyLitN = [tyLitN1, tyLitN2, tyLitN3]

tLitN1 =(eLitN1, rtLitN1CT, rtLitN1RT, rtLitN1CTRT, rtLitN1DL, rtLitN1UL ,rtLitN1ULDL, "ttLitN1")
eLitN1 = LitN 0
rtLitN1CT = LitN 0
rtLitN1RT = LitN 0
rtLitN1CTRT = LitN 0
rtLitN1DL = Error (DownA (LitN 0))
rtLitN1UL = AST [TagExpr TLitN, LitN 0]
rtLitN1ULDL =LitN 0
tyLitN1 = TyInt

tLitN2 =(eLitN2, rtLitN2CT, rtLitN2RT, rtLitN2CTRT, rtLitN2DL, rtLitN2UL ,rtLitN2ULDL, "ttLitN2")
eLitN2 = LitN 3
rtLitN2CT = LitN 3
rtLitN2RT = LitN 3
rtLitN2CTRT = LitN 3
rtLitN2DL = Error (DownA (LitN 3))
rtLitN2UL = AST [TagExpr TLitN, LitN 3]
rtLitN2ULDL =LitN 3
tyLitN2 = TyInt

tLitN3 =(eLitN3, rtLitN3CT, rtLitN3RT, rtLitN3CTRT, rtLitN3DL, rtLitN3UL ,rtLitN3ULDL, "ttLitN3")
eLitN3 = LitN 100
rtLitN3CT = LitN 100
rtLitN3RT = LitN 100
rtLitN3CTRT = LitN 100
rtLitN3DL = Error (DownA (LitN 100))
rtLitN3UL = AST [TagExpr TLitN, LitN 100]
rtLitN3ULDL =LitN 100
tyLitN3 = TyInt

--Literal Booleans
tLitB :: [TestList]
tLitB = [tLitB1, tLitB2]
tyLitB = [tyLitB1, tyLitB2]

tLitB1 =(eLitB1, rtLitB1CT, rtLitB1RT, rtLitB1CTRT, rtLitB1DL, rtLitB1UL ,rtLitB1ULDL, "ttLitB1")
eLitB1 = LitB True
rtLitB1CT = LitB True
rtLitB1RT = LitB True
rtLitB1CTRT = LitB True
rtLitB1DL = Error (DownA (LitB True))
rtLitB1UL = AST [TagExpr TLitB, LitB True]
rtLitB1ULDL = LitB True
tyLitB1 = TyBool

tLitB2 =(eLitB2, rtLitB2CT, rtLitB2RT, rtLitB2CTRT, rtLitB2DL, rtLitB2UL ,rtLitB2ULDL, "ttLitB2")
eLitB2 = LitB False
rtLitB2CT = LitB False
rtLitB2RT = LitB False
rtLitB2CTRT = LitB False
rtLitB2DL = Error (DownA (LitB False))
rtLitB2UL = AST [TagExpr TLitB, LitB False]
rtLitB2ULDL = LitB False
tyLitB2 = TyBool


--PrimUni:

tPrimUni :: [TestList]
tPrimUni = [tPrimUni1, tPrimUni2, tPrimUni3]
tyPrimUni = [tyPrimUni1, tyPrimUni2, tyPrimUni3]

tPrimUni1 =(ePrimUni1, rtPrimUni1CT, rtPrimUni1RT, rtPrimUni1CTRT, rtPrimUni1DL, rtPrimUni1UL ,rtPrimUni1ULDL, "ttPrimUni1")
ePrimUni1 = (PrimUni Not (PrimUni Not (LitB True)))
rtPrimUni1CT = (PrimUni Not (PrimUni Not (LitB True)))
rtPrimUni1RT =  (LitB True)
rtPrimUni1CTRT = (LitB True)
rtPrimUni1UL = AST [TagExpr (TPrimUni Not), AST [TagExpr (TPrimUni Not),AST [TagExpr TLitB, (LitB True)]]]
rtPrimUni1DL = Error (DownA (PrimUni Not (PrimUni Not (LitB True))))
rtPrimUni1ULDL = ePrimUni1
tyPrimUni1 = TyBool

tPrimUni2 =(ePrimUni2, rtPrimUni2CT, rtPrimUni2RT, rtPrimUni2CTRT, rtPrimUni2DL, rtPrimUni2UL ,rtPrimUni2ULDL, "ttPrimUni2")
ePrimUni2 = PrimUni Not (LitB True)
rtPrimUni2CT = (PrimUni Not (LitB True))
rtPrimUni2RT =  (LitB False)
rtPrimUni2CTRT = (LitB False)
rtPrimUni2UL = AST [TagExpr (TPrimUni Not), AST [TagExpr TLitB, (LitB True)]]
rtPrimUni2DL = Error (DownA (PrimUni Not (LitB True)))
rtPrimUni2ULDL = ePrimUni2
tyPrimUni2 = TyBool

tPrimUni3 =(ePrimUni3, rtPrimUni3CT, rtPrimUni3RT, rtPrimUni3CTRT, rtPrimUni3DL, rtPrimUni3UL ,rtPrimUni3ULDL, "ttPrimUni3")
ePrimUni3 = PrimUni Not (LitN 4)
rtPrimUni3CT = (PrimUni Not (LitN 4))
rtPrimUni3RT =  Error (PrimUni Not (LitN 4))
rtPrimUni3CTRT = Error (PrimUni Not (LitN 4))
rtPrimUni3UL = AST [TagExpr (TPrimUni Not), AST [TagExpr TLitN, (LitN 4)]]
rtPrimUni3DL = Error (DownA (PrimUni Not (LitN 4)))
rtPrimUni3ULDL = ePrimUni3
tyPrimUni3 = TyError (LitN 4) TyInt TyBool


--PrimBin:

tPrimBin :: [TestList]
tPrimBin = [tPrimBin1, tPrimBin2, tPrimBin3 , tPrimBin4 , tPrimBin5, tPrimBin6, tPrimBin7, tPrimBin8, tPrimBin9, tPrimBin10, tPrimBin11,  tPrimBin12, tPrimBin13, tPrimBin14, tPrimBin15, tPrimBin16, tPrimBin17, tPrimBin18, tPrimBin19, tPrimBin20, tPrimBin21, tPrimBin22, tPrimBin23, tPrimBin24, tPrimBin25, tPrimBin26, tPrimBin27, tPrimBin28]
tyPrimBin = [tyPrimBin1, tyPrimBin2, tyPrimBin3 , tyPrimBin4 , tyPrimBin5, tyPrimBin6, tyPrimBin7, tyPrimBin8, tyPrimBin9, tyPrimBin10, tyPrimBin11,  tyPrimBin12, tyPrimBin13, tyPrimBin14, tyPrimBin15, tyPrimBin16, tyPrimBin17, tyPrimBin18, tyPrimBin19, tyPrimBin20, tyPrimBin21, tyPrimBin22, tyPrimBin23, tyPrimBin24, tyPrimBin25, tyPrimBin26, tyPrimBin27, tyPrimBin28]

tPrimBin1 = (ePrimBin1, rtPrimBin1CT, rtPrimBin1RT, rtPrimBin1CTRT, rtPrimBin1DL, rtPrimBin1UL ,rtPrimBin1ULDL, "ttPrimBin1")
ePrimBin1 = PrimBin Add (LitN 3) (LitN 5)
rtPrimBin1CT =  PrimBin Add (LitN 3) (LitN 5)
rtPrimBin1RT =  (LitN 8)
rtPrimBin1CTRT = LitN 8
rtPrimBin1DL = Error (DownA ePrimBin1)
rtPrimBin1UL = AST [ TagExpr (TPrimBin Add), AST [TagExpr TLitN, LitN 3], AST [TagExpr TLitN, LitN 5]]
rtPrimBin1ULDL = ePrimBin1
tyPrimBin1 = TyInt

tPrimBin2 =(ePrimBin2, rtPrimBin2CT, rtPrimBin2RT, rtPrimBin2CTRT, rtPrimBin2DL, rtPrimBin2UL ,rtPrimBin2ULDL, "ttPrimBin2")
ePrimBin2 = PrimBin Mul (LitB True) (LitN 5)
rtPrimBin2CT = PrimBin Mul (LitB True) (LitN 5)
rtPrimBin2RT = Error (PrimBin Mul (LitB True) (LitN 5))
rtPrimBin2CTRT = rtPrimBin2RT
rtPrimBin2DL = Error (DownA ePrimBin2)
rtPrimBin2UL = AST [ TagExpr (TPrimBin Mul), AST [TagExpr TLitB, LitB True], AST [TagExpr TLitN, LitN 5]]
rtPrimBin2ULDL = ePrimBin2
tyPrimBin2 = TyError (LitB True) TyBool TyInt

tPrimBin3 =(ePrimBin3, rtPrimBin3CT, rtPrimBin3RT, rtPrimBin3CTRT, rtPrimBin3DL, rtPrimBin3UL ,rtPrimBin3ULDL, "ttPrimBin3")
ePrimBin3 = PrimBin Mul (LitN 3) (LitN 5)
rtPrimBin3CT =  PrimBin Mul (LitN 3) (LitN 5)
rtPrimBin3RT = LitN 15
rtPrimBin3CTRT = rtPrimBin3RT
rtPrimBin3DL = Error (DownA ePrimBin3)
rtPrimBin3UL = AST [ TagExpr (TPrimBin Mul), AST [TagExpr TLitN, LitN 3], AST [TagExpr TLitN, LitN 5]]
rtPrimBin3ULDL = ePrimBin3
tyPrimBin3 = TyInt

tPrimBin4 =(ePrimBin4, rtPrimBin4CT, rtPrimBin4RT, rtPrimBin4CTRT, rtPrimBin4DL, rtPrimBin4UL ,rtPrimBin4ULDL, "ttPrimBin4")
ePrimBin4 = PrimBin Min (LitN 3) (LitN 5)
rPrimBin4CT = PrimBin Min (LitN 3) (LitN 5)
rtPrimBin4CT =  PrimBin Min (LitN 3) (LitN 5)
rtPrimBin4RT = LitN (-2)
rtPrimBin4CTRT = rtPrimBin4RT
rtPrimBin4DL = Error (DownA ePrimBin4)
rtPrimBin4UL = AST [ TagExpr (TPrimBin Min), AST [TagExpr TLitN, LitN 3], AST [TagExpr TLitN, LitN 5]]
rtPrimBin4ULDL = ePrimBin4
tyPrimBin4 = TyInt

tPrimBin5 =(ePrimBin5, rtPrimBin5CT, rtPrimBin5RT, rtPrimBin5CTRT, rtPrimBin5DL, rtPrimBin5UL ,rtPrimBin5ULDL, "ttPrimBin5")
ePrimBin5 = PrimBin Min (LitN 13) (LitN 5)
rtPrimBin5CT =  PrimBin Min (LitN 13) (LitN 5)
rtPrimBin5RT = LitN (8)
rtPrimBin5CTRT = rtPrimBin5RT
rtPrimBin5DL = Error (DownA ePrimBin5)
rtPrimBin5UL = AST [ TagExpr (TPrimBin Min), AST [TagExpr TLitN, LitN 13], AST [TagExpr TLitN, LitN 5]]
rtPrimBin5ULDL = ePrimBin5
tyPrimBin5 = TyInt

tPrimBin6 =(ePrimBin6, rtPrimBin6CT, rtPrimBin6RT, rtPrimBin6CTRT, rtPrimBin6DL, rtPrimBin6UL ,rtPrimBin6ULDL, "ttPrimBin6")
ePrimBin6 = PrimBin Div (LitN 10) (LitN 5)
rtPrimBin6CT =  PrimBin Div (LitN 10) (LitN 5)
rtPrimBin6RT = LitN (2)
rtPrimBin6CTRT = rtPrimBin6RT
rtPrimBin6DL = Error (DownA ePrimBin6)
rtPrimBin6UL = AST [ TagExpr (TPrimBin Div), AST [TagExpr TLitN, LitN 10], AST [TagExpr TLitN, LitN 5]]
rtPrimBin6ULDL = ePrimBin6
tyPrimBin6 = TyInt

tPrimBin7 =(ePrimBin7, rtPrimBin7CT, rtPrimBin7RT, rtPrimBin7CTRT, rtPrimBin7DL, rtPrimBin7UL ,rtPrimBin7ULDL, "ttPrimBin7")
ePrimBin7 = PrimBin Div (LitN 3) (LitN 5)
rtPrimBin7CT = PrimBin Div (LitN 3) (LitN 5)
rtPrimBin7RT = LitN (0)
rtPrimBin7CTRT = rtPrimBin7RT
rtPrimBin7DL = Error (DownA ePrimBin7)
rtPrimBin7UL = AST [ TagExpr (TPrimBin Div), AST [TagExpr TLitN, LitN 3], AST [TagExpr TLitN, LitN 5]]
rtPrimBin7ULDL = ePrimBin7
tyPrimBin7 = TyInt

tPrimBin8 =(ePrimBin8, rtPrimBin8CT, rtPrimBin8RT, rtPrimBin8CTRT, rtPrimBin8DL, rtPrimBin8UL ,rtPrimBin8ULDL, "ttPrimBin8")
ePrimBin8 = PrimBin Div (LitN 0) (LitN 5)
rtPrimBin8CT = PrimBin Div (LitN 0) (LitN 5)
rtPrimBin8RT = LitN (0)
rtPrimBin8CTRT = rtPrimBin8RT
rtPrimBin8DL = Error (DownA ePrimBin8)
rtPrimBin8UL = AST [ TagExpr (TPrimBin Div), AST [TagExpr TLitN, LitN 0], AST [TagExpr TLitN, LitN 5]]
rtPrimBin8ULDL = ePrimBin8
tyPrimBin8 = TyInt

tPrimBin9 =(ePrimBin9, rtPrimBin9CT, rtPrimBin9RT, rtPrimBin9CTRT, rtPrimBin9DL, rtPrimBin9UL ,rtPrimBin9ULDL, "ttPrimBin9")
ePrimBin9 = PrimBin Div (LitN 12) (LitN 0)
rtPrimBin9CT = PrimBin Div (LitN 12) (LitN 0)
rtPrimBin9RT = Error (PrimBin Div (LitN 12) (Error (LitN 0)))
rtPrimBin9CTRT = rtPrimBin9RT
rtPrimBin9DL = Error (DownA ePrimBin9)
rtPrimBin9UL = AST [ TagExpr (TPrimBin Div), AST [TagExpr TLitN, LitN 12], AST [TagExpr TLitN, LitN 0]]
rtPrimBin9ULDL = ePrimBin9
tyPrimBin9 = TyInt

tPrimBin10 =(ePrimBin10, rtPrimBin10CT, rtPrimBin10RT, rtPrimBin10CTRT, rtPrimBin10DL, rtPrimBin10UL ,rtPrimBin10ULDL, "ttPrimBin10")
ePrimBin10 = PrimBin Eq (LitN 0) (LitN 5)
rtPrimBin10CT = PrimBin Eq (LitN 0) (LitN 5)
rtPrimBin10RT = LitB False
rtPrimBin10CTRT = rtPrimBin10RT
rtPrimBin10DL = Error (DownA ePrimBin10)
rtPrimBin10UL = AST [ TagExpr (TPrimBin Eq), AST [TagExpr TLitN, LitN 0], AST [TagExpr TLitN, LitN 5]]
rtPrimBin10ULDL = ePrimBin10
tyPrimBin10 = TyBool

tPrimBin11 =(ePrimBin11, rtPrimBin11CT, rtPrimBin11RT, rtPrimBin11CTRT, rtPrimBin11DL, rtPrimBin11UL ,rtPrimBin11ULDL, "ttPrimBin11")
ePrimBin11 = PrimBin Eq (LitN 5) (LitN 5)
rtPrimBin11CT = ePrimBin11
rtPrimBin11RT = LitB True
rtPrimBin11CTRT = rtPrimBin11RT
rtPrimBin11DL = Error (DownA ePrimBin11)
rtPrimBin11UL = AST [ TagExpr (TPrimBin Eq), AST [TagExpr TLitN, LitN 5], AST [TagExpr TLitN, LitN 5]]
rtPrimBin11ULDL = ePrimBin11
tyPrimBin11 = TyBool

tPrimBin12 =(ePrimBin12, rtPrimBin12CT, rtPrimBin12RT, rtPrimBin12CTRT, rtPrimBin12DL, rtPrimBin12UL ,rtPrimBin12ULDL, "ttPrimBin12")
ePrimBin12 = PrimBin Eq (LitB True) (LitN 5)
rtPrimBin12CT = ePrimBin12
rtPrimBin12RT = Error (PrimBin Eq (LitB True) (LitN 5))
rtPrimBin12CTRT = rtPrimBin12RT
rtPrimBin12DL = Error (DownA ePrimBin12)
rtPrimBin12UL = AST [ TagExpr (TPrimBin Eq), AST [TagExpr TLitB, LitB True], AST [TagExpr TLitN, LitN 5]]
rtPrimBin12ULDL = ePrimBin12
tyPrimBin12 = TyError (LitB True) TyBool TyInt

tPrimBin13 =(ePrimBin13, rtPrimBin13CT, rtPrimBin13RT, rtPrimBin13CTRT, rtPrimBin13DL, rtPrimBin13UL ,rtPrimBin13ULDL, "ttPrimBin13")
ePrimBin13 = PrimBin Eq (Var "x") (Var "x")
rtPrimBin13CT = ePrimBin13
rtPrimBin13RT = rtPrimBin13CT
rtPrimBin13CTRT = rtPrimBin13RT
rtPrimBin13DL = Error (DownA ePrimBin13)
rtPrimBin13UL = AST [TagExpr (TPrimBin Eq),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]]
rtPrimBin13ULDL = ePrimBin13
tyPrimBin13 = TyBool

tPrimBin14 =(ePrimBin14, rtPrimBin14CT, rtPrimBin14RT, rtPrimBin14CTRT, rtPrimBin14DL, rtPrimBin14UL ,rtPrimBin14ULDL, "ttPrimBin14")
ePrimBin14 = PrimBin Eq (Var "x") (App (Lam "y" (Var "y")) (Var "x"))
rtPrimBin14CT = ePrimBin14
rtPrimBin14RT = PrimBin Eq (Var "x") (Var "x")
rtPrimBin14CTRT = rtPrimBin14RT
rtPrimBin14DL = Error (DownA ePrimBin14)
rtPrimBin14UL = AST [TagExpr (TPrimBin Eq),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]]]
rtPrimBin14ULDL = ePrimBin14
tyPrimBin14 = TyBool

tPrimBin15 =(ePrimBin15, rtPrimBin15CT, rtPrimBin15RT, rtPrimBin15CTRT, rtPrimBin15DL, rtPrimBin15UL ,rtPrimBin15ULDL, "ttPrimBin15")
ePrimBin15 = PrimBin Eq (Var "x") (App (Lam "y" (Var "y")) (Var "y"))
rtPrimBin15CT = ePrimBin15
rtPrimBin15RT = PrimBin Eq (Var "x") (Var "y")
rtPrimBin15CTRT = rtPrimBin15RT
rtPrimBin15DL = Error (DownA ePrimBin15)
rtPrimBin15UL = AST [TagExpr (TPrimBin Eq),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]
rtPrimBin15ULDL = ePrimBin15
tyPrimBin15 = TyBool

tPrimBin16 =(ePrimBin16, rtPrimBin16CT, rtPrimBin16RT, rtPrimBin16CTRT, rtPrimBin16DL, rtPrimBin16UL ,rtPrimBin16ULDL, "ttPrimBin16")
ePrimBin16 = PrimBin Lt (Var "x") (App (Lam "y" (Var "y")) (Var "y"))
rtPrimBin16CT = ePrimBin16
rtPrimBin16RT = PrimBin Lt (Var "x") (Var "y")
rtPrimBin16CTRT = rtPrimBin16RT
rtPrimBin16DL = Error (DownA ePrimBin16)
rtPrimBin16UL = AST [TagExpr (TPrimBin Lt),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]
rtPrimBin16ULDL = ePrimBin16
tyPrimBin16 = TyBool

tPrimBin17 =(ePrimBin17, rtPrimBin17CT, rtPrimBin17RT, rtPrimBin17CTRT, rtPrimBin17DL, rtPrimBin17UL ,rtPrimBin17ULDL, "ttPrimBin17")
ePrimBin17 = PrimBin Lt (LitN 4) (LitN 2)
rtPrimBin17CT = ePrimBin17
rtPrimBin17RT = LitB False
rtPrimBin17CTRT = rtPrimBin17RT
rtPrimBin17DL = Error (DownA ePrimBin17)
rtPrimBin17UL = AST [TagExpr (TPrimBin Lt),AST [TagExpr TLitN,LitN 4],AST [TagExpr TLitN,LitN 2]]
rtPrimBin17ULDL = ePrimBin17
tyPrimBin17 = TyBool

tPrimBin18 =(ePrimBin18, rtPrimBin18CT, rtPrimBin18RT, rtPrimBin18CTRT, rtPrimBin18DL, rtPrimBin18UL ,rtPrimBin18ULDL, "ttPrimBin18")
ePrimBin18 = PrimBin Lt (LitN 0) (LitN 20)
rtPrimBin18CT = ePrimBin18
rtPrimBin18RT = LitB True
rtPrimBin18CTRT = rtPrimBin18RT
rtPrimBin18DL = Error (DownA ePrimBin18)
rtPrimBin18UL = AST [TagExpr (TPrimBin Lt),AST [TagExpr TLitN,LitN 0],AST [TagExpr TLitN,LitN 20]]
rtPrimBin18ULDL = ePrimBin18
tyPrimBin18 = TyBool

tPrimBin19 =(ePrimBin19, rtPrimBin19CT, rtPrimBin19RT, rtPrimBin19CTRT, rtPrimBin19DL, rtPrimBin19UL ,rtPrimBin19ULDL, "ttPrimBin19")
ePrimBin19 = PrimBin Gt (LitN 3) (LitN 8)
rtPrimBin19CT = ePrimBin19
rtPrimBin19RT = LitB False
rtPrimBin19CTRT = rtPrimBin19RT
rtPrimBin19DL = Error (DownA ePrimBin19)
rtPrimBin19UL = AST [TagExpr (TPrimBin Gt),AST [TagExpr TLitN,LitN 3],AST [TagExpr TLitN,LitN 8]]
rtPrimBin19ULDL = ePrimBin19
tyPrimBin19 = TyBool

tPrimBin20 =(ePrimBin20, rtPrimBin20CT, rtPrimBin20RT, rtPrimBin20CTRT, rtPrimBin20DL, rtPrimBin20UL ,rtPrimBin20ULDL, "ttPrimBin20")
ePrimBin20 = PrimBin Gt (LitN (-1)) (LitN (-8))
rtPrimBin20CT = ePrimBin20
rtPrimBin20RT = LitB True
rtPrimBin20CTRT = rtPrimBin20RT
rtPrimBin20DL = Error (DownA ePrimBin20)
rtPrimBin20UL = AST [TagExpr (TPrimBin Gt),AST [TagExpr TLitN,LitN (-1)],AST [TagExpr TLitN,LitN (-8)]]
rtPrimBin20ULDL = ePrimBin20
tyPrimBin20 = TyBool

tPrimBin21 =(ePrimBin21, rtPrimBin21CT, rtPrimBin21RT, rtPrimBin21CTRT, rtPrimBin21DL, rtPrimBin21UL ,rtPrimBin21ULDL, "ttPrimBin21")
ePrimBin21 = PrimBin GtEq (LitN (-1)) (LitN (-1))
rtPrimBin21CT = ePrimBin21
rtPrimBin21RT = LitB True
rtPrimBin21CTRT = rtPrimBin21RT
rtPrimBin21DL = Error (DownA ePrimBin21)
rtPrimBin21UL = AST [TagExpr (TPrimBin GtEq),AST [TagExpr TLitN,LitN (-1)],AST [TagExpr TLitN,LitN (-1)]]
rtPrimBin21ULDL = ePrimBin21
tyPrimBin21 = TyBool

tPrimBin22 =(ePrimBin22, rtPrimBin22CT, rtPrimBin22RT, rtPrimBin22CTRT, rtPrimBin22DL, rtPrimBin22UL ,rtPrimBin22ULDL, "ttPrimBin22")
ePrimBin22 = PrimBin GtEq (LitN (1)) (LitN (-8))
rtPrimBin22CT = ePrimBin22
rtPrimBin22RT = LitB True
rtPrimBin22CTRT = rtPrimBin22RT
rtPrimBin22DL = Error (DownA ePrimBin22)
rtPrimBin22UL = AST [TagExpr (TPrimBin GtEq),AST [TagExpr TLitN,LitN 1],AST [TagExpr TLitN,LitN (-8)]]
rtPrimBin22ULDL = ePrimBin22
tyPrimBin22 = TyBool

tPrimBin23 =(ePrimBin23, rtPrimBin23CT, rtPrimBin23RT, rtPrimBin23CTRT, rtPrimBin23DL, rtPrimBin23UL ,rtPrimBin23ULDL, "ttPrimBin23")
ePrimBin23 = PrimBin And (LitB True) (LitB True)
rtPrimBin23CT = ePrimBin23
rtPrimBin23RT = LitB True
rtPrimBin23CTRT = rtPrimBin23RT
rtPrimBin23DL = Error (DownA ePrimBin23)
rtPrimBin23UL = AST [TagExpr (TPrimBin And),AST [TagExpr TLitB,LitB True],AST [TagExpr TLitB,LitB True]]
rtPrimBin23ULDL = ePrimBin23
tyPrimBin23 = TyBool

tPrimBin24 =(ePrimBin24, rtPrimBin24CT, rtPrimBin24RT, rtPrimBin24CTRT, rtPrimBin24DL, rtPrimBin24UL ,rtPrimBin24ULDL, "ttPrimBin24")
ePrimBin24 = PrimBin And (LitB True) (LitB False)
rtPrimBin24CT = ePrimBin24
rtPrimBin24RT = LitB False
rtPrimBin24CTRT = rtPrimBin24RT
rtPrimBin24DL = Error (DownA ePrimBin24)
rtPrimBin24UL = AST [TagExpr (TPrimBin And),AST [TagExpr TLitB,LitB True],AST [TagExpr TLitB,LitB False]]
rtPrimBin24ULDL = ePrimBin24
tyPrimBin24 = TyBool

tPrimBin25 =(ePrimBin25, rtPrimBin25CT, rtPrimBin25RT, rtPrimBin25CTRT, rtPrimBin25DL, rtPrimBin25UL ,rtPrimBin25ULDL, "ttPrimBin25")
ePrimBin25 = PrimBin Or (LitB True) (LitB True)
rtPrimBin25CT = ePrimBin25
rtPrimBin25RT = LitB True
rtPrimBin25CTRT = rtPrimBin25RT
rtPrimBin25DL = Error (DownA ePrimBin25)
rtPrimBin25UL = AST [TagExpr (TPrimBin Or),AST [TagExpr TLitB,LitB True],AST [TagExpr TLitB,LitB True]]
rtPrimBin25ULDL = ePrimBin25
tyPrimBin25 = TyBool

tPrimBin26 =(ePrimBin26, rtPrimBin26CT, rtPrimBin26RT, rtPrimBin26CTRT, rtPrimBin26DL, rtPrimBin26UL ,rtPrimBin26ULDL, "ttPrimBin26")
ePrimBin26 = PrimBin Or (LitB True) (LitB False)
rtPrimBin26CT = ePrimBin26
rtPrimBin26RT = LitB True
rtPrimBin26CTRT = rtPrimBin26RT
rtPrimBin26DL = Error (DownA ePrimBin26)
rtPrimBin26UL = AST [TagExpr (TPrimBin Or),AST [TagExpr TLitB,LitB True],AST [TagExpr TLitB,LitB False]]
rtPrimBin26ULDL = ePrimBin26
tyPrimBin26 = TyBool

tPrimBin27 =(ePrimBin27, rtPrimBin27CT, rtPrimBin27RT, rtPrimBin27CTRT, rtPrimBin27DL, rtPrimBin27UL ,rtPrimBin27ULDL, "ttPrimBin27")
ePrimBin27 = PrimBin Or (LitB False) (LitB False)
rtPrimBin27CT = ePrimBin27
rtPrimBin27RT = LitB False
rtPrimBin27CTRT = rtPrimBin27RT
rtPrimBin27DL = Error (DownA ePrimBin27)
rtPrimBin27UL = AST [TagExpr (TPrimBin Or),AST [TagExpr TLitB,LitB False],AST [TagExpr TLitB,LitB False]]
rtPrimBin27ULDL = ePrimBin27
tyPrimBin27 = TyBool

tPrimBin28 =(ePrimBin28, rtPrimBin28CT, rtPrimBin28RT, rtPrimBin28CTRT, rtPrimBin28DL, rtPrimBin28UL ,rtPrimBin28ULDL, "ttPrimBin28")
ePrimBin28 = PrimBin Or (LitN 3) (LitB False)
rtPrimBin28CT = ePrimBin28
rtPrimBin28RT = Error (PrimBin Or (LitN 3) (LitB False))
rtPrimBin28CTRT = rtPrimBin28RT
rtPrimBin28DL = Error (DownA ePrimBin28)
rtPrimBin28UL = AST [TagExpr (TPrimBin Or),AST [TagExpr TLitN,LitN 3],AST [TagExpr TLitB,LitB False]]
rtPrimBin28ULDL = ePrimBin28
tyPrimBin28 = TyError (LitN 3) TyInt TyBool



--If then Else
tIf :: [TestList]
tIf = [tIf1, tIf2, tIf3, tIf4, tIf5, tIf6]
tyIf = [tyIf1, tyIf2, tyIf3, tyIf4, tyIf5, tyIf6]

tIf1 =(eIf1, rtIf1CT, rtIf1RT, rtIf1CTRT, rtIf1DL, rtIf1UL ,rtIf1ULDL, "ttIf1")
eIf1 = If (LitB True) (LitN 3) (LitN 5)
rtIf1CT = eIf1  
rtIf1RT = LitN 3
rtIf1CTRT = rtIf1RT
rtIf1DL = Error (DownA eIf1)
rtIf1UL = AST [TagExpr TIf,AST [TagExpr TLitB,LitB True],AST [TagExpr TLitN,LitN 3],AST [TagExpr TLitN,LitN 5]]
rtIf1ULDL = eIf1
tyIf1 = TyInt

tIf2 =(eIf2, rtIf2CT, rtIf2RT, rtIf2CTRT, rtIf2DL, rtIf2UL ,rtIf2ULDL, "ttIf2")
eIf2 = If (LitB False) (LitN 3) (LitN 5)
rtIf2CT = eIf2  
rtIf2RT = LitN 5
rtIf2CTRT = rtIf2RT
rtIf2DL = Error (DownA eIf2)
rtIf2UL = AST [TagExpr TIf,AST [TagExpr TLitB,LitB False],AST [TagExpr TLitN,LitN 3],AST [TagExpr TLitN,LitN 5]]
rtIf2ULDL = eIf2
tyIf2 = TyInt

tIf3 =(eIf3, rtIf3CT, rtIf3RT, rtIf3CTRT, rtIf3DL, rtIf3UL ,rtIf3ULDL, "ttIf3")
eIf3 = If (LitN 3) (LitN 3) (LitN 5)
rtIf3CT = eIf3  
rtIf3RT = Error (If (LitN 3) (LitN 3) (LitN 5))
rtIf3CTRT = rtIf3RT
rtIf3DL = Error (DownA eIf3)
rtIf3UL = AST [TagExpr TIf,AST [TagExpr TLitN,LitN 3],AST [TagExpr TLitN,LitN 3],AST [TagExpr TLitN,LitN 5]]
rtIf3ULDL = eIf3
tyIf3 = TyInt

tIf4 =(eIf4, rtIf4CT, rtIf4RT, rtIf4CTRT, rtIf4DL, rtIf4UL ,rtIf4ULDL, "ttIf4")
eIf4 = If (Var "x") (LitN 3) (LitN 5)
rtIf4CT = eIf4  
rtIf4RT = eIf4
rtIf4CTRT = rtIf4RT
rtIf4DL = Error (DownA eIf4)
rtIf4UL = AST [TagExpr TIf,AST [TagExpr TVar ,AST [TagExpr TVarRep, VarRep "x"]],AST [TagExpr TLitN,LitN 3],AST [TagExpr TLitN,LitN 5]]
rtIf4ULDL = eIf4
tyIf4 = TyInt

tIf5 =(eIf5, rtIf5CT, rtIf5RT, rtIf5CTRT, rtIf5DL, rtIf5UL ,rtIf5ULDL, "ttIf5")
eIf5 = Let "x" (LitB True) (If (Var "x") (LitN 3) (LitN 5))
rtIf5CT = eIf5  
rtIf5RT = LitN 3
rtIf5CTRT = rtIf5RT
rtIf5DL = Error (DownA eIf5)
rtIf5UL =AST [TagExpr TLet,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLitB,LitB True],AST [TagExpr TIf,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLitN,LitN 3],AST [TagExpr TLitN,LitN 5]]]
rtIf5ULDL = eIf5
tyIf5 = TyInt

tIf6 =(eIf6, rtIf6CT, rtIf6RT, rtIf6CTRT, rtIf6DL, rtIf6UL ,rtIf6ULDL, "ttIf6")
eIf6 = If (PrimUni Not (LitB True)) (LitN 3) (Let "x" (Var "y") (App (Var "x") (Var "x")))
rtIf6CT = eIf6  
rtIf6RT = App (Var "y") (Var "y")
rtIf6CTRT = rtIf6RT
rtIf6DL = Error (DownA eIf6)
rtIf6UL = AST [TagExpr TIf,AST [TagExpr (TPrimUni Not),AST [TagExpr TLitB,LitB True]],AST [TagExpr TLitN,LitN 3],AST [TagExpr TLet,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]]]]
rtIf6ULDL = eIf6
tyIf6 = TyErrorEq

--Let
tLet :: [TestList]
tLet = [tLet1, tLet2, tLet3, tLet4, tLet5, tLet6, tLet7, tLet8, tLet9, tLet10, tLet11,  tLet12]--, tLet13, tLet14, tLet15, tLet16, tLet17, tLet18, tLet19, tLet20, tLet21, tLet22, tLet23, tLet24, tLet25, tLet26, tLet27, tLet28]
tyLet = [tyLet1, tyLet2, tyLet3, tyLet4, tyLet5, tyLet6, tyLet7, tyLet8, tyLet9, tyLet10, tyLet11, tyLet12]--, tLet13, tLet14, tLet15, tLet16, tLet17, tLet18, tLet19, tLet20, tLet21, tLet22, tLet23, tLet24, tLet25, tLet26, tLet27, tLet28]

tLet1 =(eLet1, rtLet1CT, rtLet1RT, rtLet1CTRT, rtLet1DL, rtLet1UL ,rtLet1ULDL, "ttLet1")
eLet1 = Let "x" (LitN 3) (PrimBin Add (Var "x") (Var "y"))
rtLet1CT = eLet1
rtLet1RT = (PrimBin Add (LitN 3) (Var "y"))
rtLet1CTRT = rtLet1RT
rtLet1DL = Error (DownA eLet1)
rtLet1UL = AST [TagExpr TLet,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLitN,LitN 3],AST [TagExpr (TPrimBin Add),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]
rtLet1ULDL = eLet1
tyLet1 = TyInt

tLet2 =(eLet2, rtLet2CT, rtLet2RT, rtLet2CTRT, rtLet2DL, rtLet2UL ,rtLet2ULDL, "ttLet2")
eLet2 = Let "x" (LitN 3) (Let "y" (LitN 4) (PrimBin Add (Var "x") (Var "y")))
rtLet2CT = eLet2
rtLet2RT = LitN 7
rtLet2CTRT = rtLet2RT
rtLet2DL = Error (DownA eLet2)
rtLet2UL = AST [TagExpr TLet,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLitN,LitN 3],AST [TagExpr TLet,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TLitN,LitN 4],AST [TagExpr (TPrimBin Add),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]]
rtLet2ULDL = eLet2
tyLet2 = TyInt

tLet3 =(eLet3, rtLet3CT, rtLet3RT, rtLet3CTRT, rtLet3DL, rtLet3UL ,rtLet3ULDL, "ttLet3")
eLet3 = Let "x" (LitN 3) (Let "y" (Var "x") (PrimBin Add (Var "x") (Var "y")))
rtLet3CT = eLet3
rtLet3RT = LitN 6
rtLet3CTRT = rtLet3RT
rtLet3DL = Error (DownA eLet3)
rtLet3UL = AST [TagExpr TLet,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLitN,LitN 3],AST [TagExpr TLet,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimBin Add),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]]
rtLet3ULDL = eLet3
tyLet3 = TyInt

tLet4 =(eLet4, rtLet4CT, rtLet4RT, rtLet4CTRT, rtLet4DL, rtLet4UL ,rtLet4ULDL, "ttLet4")
eLet4 = Let "x" (Var "x") (PrimBin Add (Var "x") (Var "y"))
rtLet4CT = eLet4
rtLet4RT = (PrimBin Add (Var "x") (Var "y"))
rtLet4CTRT = rtLet4RT
rtLet4DL = Error (DownA eLet4)
rtLet4UL = AST [TagExpr TLet,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimBin Add),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]
rtLet4ULDL = eLet4
tyLet4 = TyInt

tLet5 =(eLet5, rtLet5CT, rtLet5RT, rtLet5CTRT, rtLet5DL, rtLet5UL ,rtLet5ULDL, "ttLet5")
eLet5 = Let "x" (Var "y") (PrimBin Add (Var "x") (Var "y"))
rtLet5CT = eLet5
rtLet5RT = (PrimBin Add (Var "y") (Var "y"))
rtLet5CTRT = rtLet5RT
rtLet5DL = Error (DownA eLet5)
rtLet5UL = AST [TagExpr TLet,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr (TPrimBin Add),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]
rtLet5ULDL = eLet5
tyLet5 = TyInt

tLet6 =(eLet6, rtLet6CT, rtLet6RT, rtLet6CTRT, rtLet6DL, rtLet6UL ,rtLet6ULDL, "ttLet6")
eLet6 = Let "x" (PrimBin Add (Var "x") (Var "y")) (Var "x")
rtLet6CT = eLet6
rtLet6RT = (PrimBin Add (Var "x") (Var "y"))
rtLet6CTRT = rtLet6RT
rtLet6DL = Error (DownA eLet6)
rtLet6UL = AST [TagExpr TLet,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimBin Add),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]]
rtLet6ULDL = eLet6
tyLet6 = TyInt

tLet7 =(eLet7, rtLet7CT, rtLet7RT, rtLet7CTRT, rtLet7DL, rtLet7UL ,rtLet7ULDL, "ttLet7")
eLet7 = Let "x" (Lam "x" (Var "x")) (App (Var "x") (Var "y"))
rtLet7CT = eLet7
rtLet7RT = Var "y"
rtLet7CTRT = rtLet7RT
rtLet7DL = Error (DownA eLet7)
rtLet7UL = AST [TagExpr TLet,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]
rtLet7ULDL = eLet7
tyLet7 = TyVar "Type5{fresh}"   

tLet8 =(eLet8, rtLet8CT, rtLet8RT, rtLet8CTRT, rtLet8DL, rtLet8UL ,rtLet8ULDL, "ttLet8")
eLet8 = Let "x" (App (Var "x") (Var "r")) (App (Var "x") (Var "y"))
rtLet8CT = eLet8
rtLet8RT = (App (App (Var "x") (Var "r")) (Var "y"))
rtLet8CTRT = rtLet8RT
rtLet8DL = Error (DownA eLet8)
rtLet8UL = AST [TagExpr TLet,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "r"]]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]
rtLet8ULDL = eLet8
tyLet8 = TyVar "Type5{fresh}"   

tLet9 =(eLet9, rtLet9CT, rtLet9RT, rtLet9CTRT, rtLet9DL, rtLet9UL ,rtLet9ULDL, "ttLet9")
eLet9 = Let "x" (LitB True) (PrimBin Add (If (Var "x") (LitN 3) (LitN 4)) (Var "y"))
rtLet9CT = eLet9
rtLet9RT = (PrimBin Add (LitN 3) (Var "y"))
rtLet9CTRT = rtLet9RT
rtLet9DL = Error (DownA eLet9)
rtLet9UL = AST [TagExpr TLet,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLitB,LitB True],AST [TagExpr (TPrimBin Add),AST [TagExpr TIf,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLitN,LitN 3],AST [TagExpr TLitN,LitN 4]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]
rtLet9ULDL = eLet9
tyLet9 = TyInt

tLet10 =(eLet10, rtLet10CT, rtLet10RT, rtLet10CTRT, rtLet10DL, rtLet10UL ,rtLet10ULDL, "ttLet10")
eLet10 = Let "x" (LitN 3) (PrimBin Add (Var "x") (Var "y"))
rtLet10CT = eLet10
rtLet10RT = (PrimBin Add (LitN 3) (Var "y"))
rtLet10CTRT = rtLet10RT
rtLet10DL = Error (DownA eLet10)
rtLet10UL = AST [TagExpr TLet,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLitN,LitN 3],AST [TagExpr (TPrimBin Add),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]
rtLet10ULDL = eLet10
tyLet10 = TyInt 

tLet11 =(eLet11, rtLet11CT, rtLet11RT, rtLet11CTRT, rtLet11DL, rtLet11UL ,rtLet11ULDL, "ttLet11")
eLet11 = Let "x" (LitN 3) (Let "y" (LitN 8) (PrimBin Mul (Var "x") (Var "y")))
rtLet11CT = eLet11
rtLet11RT = LitN 24
rtLet11CTRT = rtLet11RT
rtLet11DL = Error (DownA eLet11)
rtLet11UL = AST [TagExpr TLet,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLitN,LitN 3],AST [TagExpr TLet,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TLitN,LitN 8],AST [TagExpr (TPrimBin Mul),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]]
rtLet11ULDL = eLet11
tyLet11 = TyInt

tLet12 =(eLet12, rtLet12CT, rtLet12RT, rtLet12CTRT, rtLet12DL, rtLet12UL ,rtLet12ULDL, "ttLet12")
eLet12 = Let "x" (LitN 3) (UpA (PrimBin Add (Var "x") (Var "x")))
--Let "x" (LitN 3) (UpA (Var "x"))
rtLet12CT = Let "x" (LitN 3) (AST [TagExpr (TPrimBin Add),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]])
rtLet12RT = Error (UpA (PrimBin Add (LitN 3) (LitN 3)))
rtLet12CTRT = AST [TagExpr (TPrimBin Add),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]]
rtLet12DL = Error (DownA eLet12)
rtLet12UL = AST [TagExpr TLet,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLitN,LitN 3],AST [TagExpr TPromote,TagExpr (TPrimBin Add),AST [TagExpr TPromote,TagExpr TVar,AST [TagExpr TPromote,TagExpr TVarRep,AST [TagExpr TVarRep,VarRep "x"]]],AST [TagExpr TPromote,TagExpr TVar,AST [TagExpr TPromote,TagExpr TVarRep,AST [TagExpr TVarRep,VarRep "x"]]]]]
rtLet12ULDL = Let "x" (LitN 3) (AST [TagExpr (TPrimBin Add),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]])
tyLet12 = TyCodeUnP



--LetRec
tLetRec :: [TestList]
tLetRec = [tLetRec1, tLetRec2, tLetRec3, tLetRec4, tLetRec5, tLetRec6, tLetRec7, tLetRec8, tLetRec9, tLetRec10, tLetRec11]--,  tLetRec12, tLetRec13, tLetRec14, tLetRec15, tLetRec16, tLetRec17, tLetRec18, tLetRec19, tLetRec20, tLetRec21, tLetRec22, tLetRec23, tLetRec24, tLetRec25, tLetRec26, tLetRec27, tLetRec28]
tyLetRec = [tyLetRec1, tyLetRec2, tyLetRec3, tyLetRec4, tyLetRec5, tyLetRec6, tyLetRec7, tyLetRec8, tyLetRec9, tyLetRec10, tyLetRec11]--,  tLetRec12, tLetRec13, tLetRec14, tLetRec15, tLetRec16, tLetRec17, tLetRec18, tLetRec19, tLetRec20, tLetRec21, tLetRec22, tLetRec23, tLetRec24, tLetRec25, tLetRec26, tLetRec27, tLetRec28]

tLetRec1 =(eLetRec1, rtLetRec1CT, rtLetRec1RT, rtLetRec1CTRT, rtLetRec1DL, rtLetRec1UL ,rtLetRec1ULDL, "ttLetRec1")
eLetRec1 = LetRec "x" (LitN 3) (PrimBin Add (Var "x") (Var "y"))
rtLetRec1CT = eLetRec1
rtLetRec1RT = (PrimBin Add (LitN 3) (Var "y"))
rtLetRec1CTRT = rtLetRec1RT
rtLetRec1DL = Error (DownA eLetRec1)
rtLetRec1UL = AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLitN,LitN 3],AST [TagExpr (TPrimBin Add),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]
rtLetRec1ULDL = eLetRec1
tyLetRec1 = TyInt

tLetRec2 =(eLetRec2, rtLetRec2CT, rtLetRec2RT, rtLetRec2CTRT, rtLetRec2DL, rtLetRec2UL ,rtLetRec2ULDL, "ttLetRec2")
eLetRec2 = LetRec "x" (LitN 2) (LetRec "y" (LitN 4) (PrimBin Add (Var "x") (Var "y")))
rtLetRec2CT = eLetRec2
rtLetRec2RT = LitN 6
rtLetRec2CTRT = rtLetRec2RT
rtLetRec2DL = Error (DownA eLetRec2)
rtLetRec2UL = AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLitN,LitN 2],AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TLitN,LitN 4],AST [TagExpr (TPrimBin Add),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]]
rtLetRec2ULDL = eLetRec2
tyLetRec2 = TyInt


tLetRec3 =(eLetRec3, rtLetRec3CT, rtLetRec3RT, rtLetRec3CTRT, rtLetRec3DL, rtLetRec3UL ,rtLetRec3ULDL, "ttLetRec3")
eLetRec3 = LetRec "x" (LitN 3) (LetRec "y" (Var "x") (PrimBin Add (Var "x") (Var "y")))
rtLetRec3CT = eLetRec3
rtLetRec3RT = LitN 6
rtLetRec3CTRT = rtLetRec3RT
rtLetRec3DL = Error (DownA eLetRec3)
rtLetRec3UL = AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLitN,LitN 3],AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimBin Add),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]]
rtLetRec3ULDL = eLetRec3
tyLetRec3 = TyInt


tLetRec4 =(eLetRec4, rtLetRec4CT, rtLetRec4RT, rtLetRec4CTRT, rtLetRec4DL, rtLetRec4UL ,rtLetRec4ULDL, "ttLetRec4")
eLetRec4 = LetRec "x" (Var "x") (PrimBin Add (Var "x") (Var "y"))
rtLetRec4CT = eLetRec4 --(PrimBin Add (Var "x") (Var "y"))
rtLetRec4RT = rtLetRec4CT
rtLetRec4CTRT = rtLetRec4RT
rtLetRec4DL = Error (DownA eLetRec4)
rtLetRec4UL = AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimBin Add),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]
rtLetRec4ULDL = eLetRec4
tyLetRec4 = TyInt

tLetRec5 =(eLetRec5, rtLetRec5CT, rtLetRec5RT, rtLetRec5CTRT, rtLetRec5DL, rtLetRec5UL ,rtLetRec5ULDL, "ttLetRec5")
eLetRec5 = LetRec "x" (Var "y") (PrimBin Add (Var "x") (Var "y"))
rtLetRec5CT = eLetRec5
rtLetRec5RT = (PrimBin Add (Var "y") (Var "y"))
rtLetRec5CTRT = rtLetRec5RT
rtLetRec5DL = Error (DownA eLetRec5)
rtLetRec5UL = AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr (TPrimBin Add),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]
rtLetRec5ULDL = eLetRec5
tyLetRec5 = TyInt


--changes with change of length of limit
tLetRec6 =(eLetRec6, rtLetRec6CT, rtLetRec6RT, rtLetRec6CTRT, rtLetRec6DL, rtLetRec6UL ,rtLetRec6ULDL, "ttLetRec6")
eLetRec6 = LetRec "x" (PrimBin Add (Var "x") (Var "y")) (Var "x")
rtLetRec6CT = eLetRec6
rtLetRec6RT = ErrorLoop eLetRec6
rtLetRec6CTRT = ErrorLoop eLetRec6
rtLetRec6DL = Error (DownA eLetRec6)
rtLetRec6UL = AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimBin Add),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]]
rtLetRec6ULDL = eLetRec6
tyLetRec6 = TyInt


tLetRec7 =(eLetRec7, rtLetRec7CT, rtLetRec7RT, rtLetRec7CTRT, rtLetRec7DL, rtLetRec7UL ,rtLetRec7ULDL, "ttLetRec7")
eLetRec7 = LetRec "x" (Lam "x" (Var "x")) (App (Var "x") (Var "y"))
rtLetRec7CT = eLetRec7
rtLetRec7RT = Var "y"
rtLetRec7CTRT = rtLetRec7RT
rtLetRec7DL = Error (DownA eLetRec7)
rtLetRec7UL = AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]
rtLetRec7ULDL = eLetRec7
tyLetRec7 = TyVar "Type5{fresh}"

--changes with change of length of limit
tLetRec8 =(eLetRec8, rtLetRec8CT, rtLetRec8RT, rtLetRec8CTRT, rtLetRec8DL, rtLetRec8UL ,rtLetRec8ULDL, "ttLetRec8")
eLetRec8 = LetRec "x" (App (Var "x") (Var "r")) (App (Var "x") (Var "y"))
rtLetRec8CT = eLetRec8
rtLetRec8RT = ErrorLoop eLetRec8
rtLetRec8CTRT = rtLetRec8RT
rtLetRec8DL = Error (DownA eLetRec8)
rtLetRec8UL = AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "r"]]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]
rtLetRec8ULDL = eLetRec8
tyLetRec8 = TyVar "Type5{fresh}"

tLetRec9 =(eLetRec9, rtLetRec9CT, rtLetRec9RT, rtLetRec9CTRT, rtLetRec9DL, rtLetRec9UL ,rtLetRec9ULDL, "ttLetRec9")
eLetRec9 = LetRec "x" (LitN 3) (PrimBin Add (Var "x") (Var "y"))
rtLetRec9CT = eLetRec9
rtLetRec9RT = (PrimBin Add (LitN 3) (Var "y"))
rtLetRec9CTRT = rtLetRec9RT
rtLetRec9DL = Error (DownA eLetRec9)
rtLetRec9UL = AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLitN,LitN 3],AST [TagExpr (TPrimBin Add),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]
rtLetRec9ULDL = eLetRec9
tyLetRec9 = TyInt

tLetRec10 =(eLetRec10, rtLetRec10CT, rtLetRec10RT, rtLetRec10CTRT, rtLetRec10DL, rtLetRec10UL ,rtLetRec10ULDL, "ttLetRec10")
eLetRec10 = LetRec "x" (UpA (LitN 3)) (UpA (PrimBin Add (Var "x") (LitN 3)))
rtLetRec10CT = LetRec "x" (AST [TagExpr TLitN,LitN 3]) (AST [TagExpr (TPrimBin Add),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLitN,LitN 3]])
rtLetRec10RT = Error (LetRec "x" (Error (UpA (LitN 3))) (Error (UpA (PrimBin Add (Var "x") (LitN 3)))))
rtLetRec10CTRT =AST [TagExpr (TPrimBin Add),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLitN,LitN 3]]
rtLetRec10DL = Error (DownA eLetRec10)
rtLetRec10UL = AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TPromote,TagExpr TLitN,AST [TagExpr TLitN,LitN 3]],AST [TagExpr TPromote,TagExpr (TPrimBin Add),AST [TagExpr TPromote,TagExpr TVar,AST [TagExpr TPromote,TagExpr TVarRep,AST [TagExpr TVarRep,VarRep "x"]]],AST [TagExpr TPromote,TagExpr TLitN,AST [TagExpr TLitN,LitN 3]]]]
rtLetRec10ULDL = rtLetRec10CT
tyLetRec10 = TyCodeUnP

tLetRec11 =(eLetRec11, rtLetRec11CT, rtLetRec11RT, rtLetRec11CTRT, rtLetRec11DL, rtLetRec11UL ,rtLetRec11ULDL, "ttLetRec11")
eLetRec11 = LetRec "x" (LitN 3) (LetRec "y" (LitN 8) (PrimBin Mul (Var "x") (Var "y")))
rtLetRec11CT = eLetRec11
rtLetRec11RT = LitN 24
rtLetRec11CTRT = rtLetRec11RT
rtLetRec11DL = Error (DownA eLetRec11)
rtLetRec11UL = AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLitN,LitN 3],AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TLitN,LitN 8],AST [TagExpr (TPrimBin Mul),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]]
rtLetRec11ULDL = eLetRec11
tyLetRec11 = TyInt

tFact :: [TestList]
tFact = [tLetRecFact0, tLetRecFact1, tLetRecFact2, tLetRecFact3,  tLetRecFactn3, tLetRecFact8, tLetRecFact10 , tLetRecFactTrue]
tyFact = [tyLetRecFact0, tyLetRecFact1, tyLetRecFact2, tyLetRecFact3,  tyLetRecFactn3, tyLetRecFact8, tyLetRecFact10 , tyLetRecFactTrue]

tLetRecFact1        = (eLetRecFact1, rtLetRecFact1CT, rtLetRecFact1RT, rtLetRecFact1CTRT, rtLetRecFact1DL, rtLetRecFact1UL ,rtLetRecFact1ULDL, "ttLetRecFact1")
eLetRecFact1        = App eLetRecFactx (LitN 1)
rtLetRecFact1CT     = (App (Lam "y" (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Lt (Var "n") (LitN 1)) (PrimBin Eq (Var "n") (LitN 0))) (LitN 1) (PrimBin Mul (Var "n") (App (Var "x") (PrimBin Min  (Var "n") (LitN 1)))))) (App (Var "x") (Var "y") ))) (LitN 1))
rtLetRecFact1RT     = LitN 1
rtLetRecFact1CTRT   = LitN 1  
rtLetRecFact1DL     = Error (DownA (App (Lam "y" (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Lt (Var "n") (LitN 1)) (PrimBin Eq (Var "n") (LitN 0))) (LitN 1) (PrimBin Mul (Var "n") (App (Var "x") (PrimBin Min (Var "n") (LitN 1)))))) (App (Var "x") (Var "y")))) (LitN 1)))
rtLetRecFact1UL     = AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TIf,AST [TagExpr (TPrimBin Or),AST [TagExpr (TPrimBin Lt),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]],AST [TagExpr (TPrimBin Eq),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 0]]],AST [TagExpr TLitN,LitN 1],AST [TagExpr (TPrimBin Mul),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimBin Min),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]]]]]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]],AST [TagExpr TLitN,LitN 1]]
rtLetRecFact1ULDL   = eLetRecFact1
tyLetRecFact1 = TyInt

tLetRecFact0 =(eLetRecFact0, rtLetRecFact0CT, rtLetRecFact0RT, rtLetRecFact0CTRT, rtLetRecFact0DL, rtLetRecFact0UL ,rtLetRecFact0ULDL, "ttLetRecFact0")
eLetRecFact0        = App eLetRecFactx (LitN 0)
rtLetRecFact0CT     = App (Lam "y" (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Lt (Var "n") (LitN 1)) (PrimBin Eq (Var "n") (LitN 0))) (LitN 1) (PrimBin Mul (Var "n") (App (Var "x") (PrimBin Min (Var "n") (LitN 1)))))) (App (Var "x") (Var "y")))) (LitN 0)
rtLetRecFact0RT     = LitN 1
rtLetRecFact0CTRT   =  rtLetRecFact0RT
rtLetRecFact0DL     = Error (DownA (App (Lam "y" (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Lt (Var "n") (LitN 1)) (PrimBin Eq (Var "n") (LitN 0))) (LitN 1) (PrimBin Mul (Var "n") (App (Var "x") (PrimBin Min (Var "n") (LitN 1)))))) (App (Var "x") (Var "y")))) (LitN 0)))
rtLetRecFact0UL     = AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TIf,AST [TagExpr (TPrimBin Or),AST [TagExpr (TPrimBin Lt),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]],AST [TagExpr (TPrimBin Eq),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 0]]],AST [TagExpr TLitN,LitN 1],AST [TagExpr (TPrimBin Mul),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimBin Min),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]]]]]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]],AST [TagExpr TLitN,LitN 0]]
rtLetRecFact0ULDL   = eLetRecFact0
tyLetRecFact0 = TyInt


tLetRecFact2 =(eLetRecFact2, rtLetRecFact2CT, rtLetRecFact2RT, rtLetRecFact2CTRT, rtLetRecFact2DL, rtLetRecFact2UL ,rtLetRecFact2ULDL, "ttLetRecFact2")
eLetRecFact2        = App eLetRecFactx (LitN 2)
rtLetRecFact2CT     = App (Lam "y" (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Lt (Var "n") (LitN 1)) (PrimBin Eq (Var "n") (LitN 0))) (LitN 1) (PrimBin Mul (Var "n") (App (Var "x") (PrimBin Min (Var "n") (LitN 1)))))) (App (Var "x") (Var "y")))) (LitN 2)
rtLetRecFact2RT     = LitN 2
rtLetRecFact2CTRT   =  rtLetRecFact2RT
rtLetRecFact2DL     = Error (DownA (App (Lam "y" (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Lt (Var "n") (LitN 1)) (PrimBin Eq (Var "n") (LitN 0))) (LitN 1) (PrimBin Mul (Var "n") (App (Var "x") (PrimBin Min (Var "n") (LitN 1)))))) (App (Var "x") (Var "y")))) (LitN 2)))
rtLetRecFact2UL     = AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TIf,AST [TagExpr (TPrimBin Or),AST [TagExpr (TPrimBin Lt),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]],AST [TagExpr (TPrimBin Eq),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 0]]],AST [TagExpr TLitN,LitN 1],AST [TagExpr (TPrimBin Mul),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimBin Min),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]]]]]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]],AST [TagExpr TLitN,LitN 2]]
rtLetRecFact2ULDL   = eLetRecFact2
tyLetRecFact2 = TyInt

tLetRecFact3 =(eLetRecFact3, rtLetRecFact3CT, rtLetRecFact3RT, rtLetRecFact3CTRT, rtLetRecFact3DL, rtLetRecFact3UL ,rtLetRecFact3ULDL, "ttLetRecFact3")
eLetRecFact3 = App eLetRecFactx (LitN 3)
rtLetRecFact3CT     = App (Lam "y" (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Lt (Var "n") (LitN 1)) (PrimBin Eq (Var "n") (LitN 0))) (LitN 1) (PrimBin Mul (Var "n") (App (Var "x") (PrimBin Min (Var "n") (LitN 1)))))) (App (Var "x") (Var "y")))) (LitN 3)
rtLetRecFact3RT     = LitN 6
rtLetRecFact3CTRT   =  rtLetRecFact3RT
rtLetRecFact3DL     = Error (DownA (App (Lam "y" (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Lt (Var "n") (LitN 1)) (PrimBin Eq (Var "n") (LitN 0))) (LitN 1) (PrimBin Mul (Var "n") (App (Var "x") (PrimBin Min (Var "n") (LitN 1)))))) (App (Var "x") (Var "y")))) (LitN 3)))
rtLetRecFact3UL     = AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TIf,AST [TagExpr (TPrimBin Or),AST [TagExpr (TPrimBin Lt),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]],AST [TagExpr (TPrimBin Eq),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 0]]],AST [TagExpr TLitN,LitN 1],AST [TagExpr (TPrimBin Mul),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimBin Min),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]]]]]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]],AST [TagExpr TLitN,LitN 3]]
rtLetRecFact3ULDL   = eLetRecFact3
tyLetRecFact3 = TyInt

tLetRecFactn3 =(eLetRecFactn3, rtLetRecFactn3CT, rtLetRecFactn3RT, rtLetRecFactn3CTRT, rtLetRecFactn3DL, rtLetRecFactn3UL ,rtLetRecFactn3ULDL, "ttLetRecFactn3")
eLetRecFactn3 = App eLetRecFactx (LitN (-3))
rtLetRecFactn3CT     = App (Lam "y" (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Lt (Var "n") (LitN 1)) (PrimBin Eq (Var "n") (LitN 0))) (LitN 1) (PrimBin Mul (Var "n") (App (Var "x") (PrimBin Min (Var "n") (LitN 1)))))) (App (Var "x") (Var "y")))) (LitN (-3))
rtLetRecFactn3RT     = LitN 1
rtLetRecFactn3CTRT   =  rtLetRecFactn3RT
rtLetRecFactn3DL     = Error (DownA (App (Lam "y" (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Lt (Var "n") (LitN 1)) (PrimBin Eq (Var "n") (LitN 0))) (LitN 1) (PrimBin Mul (Var "n") (App (Var "x") (PrimBin Min (Var "n") (LitN 1)))))) (App (Var "x") (Var "y")))) (LitN (-3))))
rtLetRecFactn3UL     = AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TIf,AST [TagExpr (TPrimBin Or),AST [TagExpr (TPrimBin Lt),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]],AST [TagExpr (TPrimBin Eq),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 0]]],AST [TagExpr TLitN,LitN 1],AST [TagExpr (TPrimBin Mul),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimBin Min),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]]]]]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]],AST [TagExpr TLitN,LitN (-3)]]
rtLetRecFactn3ULDL   = eLetRecFactn3
tyLetRecFactn3 = TyInt


tLetRecFact8 =(eLetRecFact8, rtLetRecFact8CT, rtLetRecFact8RT, rtLetRecFact8CTRT, rtLetRecFact8DL, rtLetRecFact8UL ,rtLetRecFact8ULDL, "ttLetRecFact8")
eLetRecFact8 = App eLetRecFactx (LitN 8)
rtLetRecFact8CT     = App (Lam "y" (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Lt (Var "n") (LitN 1)) (PrimBin Eq (Var "n") (LitN 0))) (LitN 1) (PrimBin Mul (Var "n") (App (Var "x") (PrimBin Min (Var "n") (LitN 1)))))) (App (Var "x") (Var "y")))) (LitN 8)
rtLetRecFact8RT     = LitN 40320
rtLetRecFact8CTRT   =  rtLetRecFact8RT
rtLetRecFact8DL     = Error (DownA (App (Lam "y" (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Lt (Var "n") (LitN 1)) (PrimBin Eq (Var "n") (LitN 0))) (LitN 1) (PrimBin Mul (Var "n") (App (Var "x") (PrimBin Min (Var "n") (LitN 1)))))) (App (Var "x") (Var "y")))) (LitN 8)))
rtLetRecFact8UL     = AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TIf,AST [TagExpr (TPrimBin Or),AST [TagExpr (TPrimBin Lt),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]],AST [TagExpr (TPrimBin Eq),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 0]]],AST [TagExpr TLitN,LitN 1],AST [TagExpr (TPrimBin Mul),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimBin Min),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]]]]]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]],AST [TagExpr TLitN,LitN 8]]
rtLetRecFact8ULDL   = eLetRecFact8
tyLetRecFact8 = TyInt

tLetRecFact10 =(eLetRecFact10, rtLetRecFact10CT, rtLetRecFact10RT, rtLetRecFact10CTRT, rtLetRecFact10DL, rtLetRecFact10UL ,rtLetRecFact10ULDL, "ttLetRecFact10")
eLetRecFact10 = App eLetRecFactx (LitN 10)
rtLetRecFact10CT     = App (Lam "y" (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Lt (Var "n") (LitN 1)) (PrimBin Eq (Var "n") (LitN 0))) (LitN 1) (PrimBin Mul (Var "n") (App (Var "x") (PrimBin Min (Var "n") (LitN 1)))))) (App (Var "x") (Var "y")))) (LitN 10)
rtLetRecFact10RT     = LitN 3628800
rtLetRecFact10CTRT   =  rtLetRecFact10RT
rtLetRecFact10DL     = Error (DownA (App (Lam "y" (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Lt (Var "n") (LitN 1)) (PrimBin Eq (Var "n") (LitN 0))) (LitN 1) (PrimBin Mul (Var "n") (App (Var "x") (PrimBin Min (Var "n") (LitN 1)))))) (App (Var "x") (Var "y")))) (LitN 10)))
rtLetRecFact10UL     = AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TIf,AST [TagExpr (TPrimBin Or),AST [TagExpr (TPrimBin Lt),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]],AST [TagExpr (TPrimBin Eq),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 0]]],AST [TagExpr TLitN,LitN 1],AST [TagExpr (TPrimBin Mul),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimBin Min),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]]]]]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]],AST [TagExpr TLitN,LitN 10]]
rtLetRecFact10ULDL   = eLetRecFact10
tyLetRecFact10 = TyInt

-- tLetRecFactx =
eLetRecFactx = Lam "y" (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Lt (Var "n") (LitN 1)) (PrimBin Eq (Var "n") (LitN 0))) (LitN 1) (PrimBin Mul (Var "n") (App (Var "x") (PrimBin Min  (Var "n") (LitN 1)))))) (App (Var "x") (Var "y") ))
rLetRecFactxCT = Lam "y" (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Lt (Var "n") (LitN 1)) (PrimBin Eq (Var "n") (LitN 0))) (LitN 1) (PrimBin Mul (Var "n") (App (Var "x") (PrimBin Min  (Var "n") (LitN 1)))))) (App (Var "x") (Var "y") )) 
tyLetRecFactx = TyFunc TyInt TyInt

tLetRecFactTrue = (eLetRecFactTrue, rtLetRecFactTrueCT, rtLetRecFactTrueRT, rtLetRecFactTrueCTRT, rtLetRecFactTrueDL, rtLetRecFactTrueUL ,rtLetRecFactTrueULDL, "ttLetRecFactTrue")
eLetRecFactTrue = App eLetRecFactx (LitB True)
rtLetRecFactTrueCT     = App (Lam "y" (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Lt (Var "n") (LitN 1)) (PrimBin Eq (Var "n") (LitN 0))) (LitN 1) (PrimBin Mul (Var "n") (App (Var "x") (PrimBin Min (Var "n") (LitN 1)))))) (App (Var "x") (Var "y")))) (LitB True)
rtLetRecFactTrueRT     = Error (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Lt (Var "n") (LitN 1)) (PrimBin Eq (Var "n") (LitN 0))) (LitN 1) (PrimBin Mul (Var "n") (App (Var "x") (PrimBin Min (Var "n") (LitN 1)))))) (Error (If (Error (PrimBin Or (Error (PrimBin Lt (LitB True) (LitN 1))) (Error (PrimBin Eq (LitB True) (LitN 0))))) (LitN 1) (PrimBin Mul (LitB True) (App (Var "x") (PrimBin Min (LitB True) (LitN 1)))))))
rtLetRecFactTrueCTRT   =  rtLetRecFactTrueRT
rtLetRecFactTrueDL     = Error (DownA (App (Lam "y" (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Lt (Var "n") (LitN 1)) (PrimBin Eq (Var "n") (LitN 0))) (LitN 1) (PrimBin Mul (Var "n") (App (Var "x") (PrimBin Min (Var "n") (LitN 1)))))) (App (Var "x") (Var "y")))) (LitB True)))
rtLetRecFactTrueUL     = AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TIf,AST [TagExpr (TPrimBin Or),AST [TagExpr (TPrimBin Lt),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]],AST [TagExpr (TPrimBin Eq),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 0]]],AST [TagExpr TLitN,LitN 1],AST [TagExpr (TPrimBin Mul),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimBin Min),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]]]]]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]],AST [TagExpr TLitB,LitB True]]
rtLetRecFactTrueULDL   = App (Lam "y" (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Lt (Var "n") (LitN 1)) (PrimBin Eq (Var "n") (LitN 0))) (LitN 1) (PrimBin Mul (Var "n") (App (Var "x") (PrimBin Min (Var "n") (LitN 1)))))) (App (Var "x") (Var "y")))) (LitB True)
tyLetRecFactTrue = TyError (LitB True) TyBool TyInt



tFib :: [TestList]
tFib = [tFib0, tFib1, tFib2, tFib3, tFib4, tFib8, tFibFalse]
tyFib = [tyFib0, tyFib1, tyFib2, tyFib3, tyFib4, tyFib8, tyFibFalse]

tFib0 = (eFib0, rtFib0CT, rtFib0RT, rtFib0CTRT, rtFib0DL, rtFib0UL ,rtFib0ULDL, "ttFib0")
eFib0       = App eLetRecFibx (LitN 0) 
rtFib0CT    = eFib0
rtFib0RT    = LitN 1
rtFib0CTRT  = LitN 1
rtFib0DL    = Error (DownA (App (Lam "y" (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Eq (Var "n") (LitN 1)) (PrimBin Lt (Var "n") (LitN 1))) (LitN 1) (PrimBin Add (App (Var "x") (PrimBin Min (Var "n") (LitN 1))) (App (Var "x") (PrimBin Min (Var "n") (LitN 2)))))) (App (Var "x") (Var "y")))) (LitN 0)))
rtFib0UL    = AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TIf,AST [TagExpr (TPrimBin Or),AST [TagExpr (TPrimBin Eq),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]],AST [TagExpr (TPrimBin Lt),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]]],AST [TagExpr TLitN,LitN 1],AST [TagExpr (TPrimBin Add),AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimBin Min),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimBin Min),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 2]]]]]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]],AST [TagExpr TLitN,LitN 0]]
rtFib0ULDL  = eFib0
tyFib0 = TyInt 

tFib1 =(eFib1, rtFib1CT, rtFib1RT, rtFib1CTRT, rtFib1DL, rtFib1UL ,rtFib1ULDL, "ttFib1")
eFib1 = App eLetRecFibx (LitN 1) 
rtFib1CT    = eFib1
rtFib1RT    = LitN 1
rtFib1CTRT  = LitN 1
rtFib1DL    = Error (DownA (App (Lam "y" (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Eq (Var "n") (LitN 1)) (PrimBin Lt (Var "n") (LitN 1))) (LitN 1) (PrimBin Add (App (Var "x") (PrimBin Min (Var "n") (LitN 1))) (App (Var "x") (PrimBin Min (Var "n") (LitN 2)))))) (App (Var "x") (Var "y")))) (LitN 1)))
rtFib1UL    = AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TIf,AST [TagExpr (TPrimBin Or),AST [TagExpr (TPrimBin Eq),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]],AST [TagExpr (TPrimBin Lt),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]]],AST [TagExpr TLitN,LitN 1],AST [TagExpr (TPrimBin Add),AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimBin Min),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimBin Min),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 2]]]]]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]],AST [TagExpr TLitN,LitN 1]]
rtFib1ULDL  = eFib1
tyFib1 = TyInt


tFib2 =(eFib2, rtFib2CT, rtFib2RT, rtFib2CTRT, rtFib2DL, rtFib2UL ,rtFib2ULDL, "ttFib2")
eFib2 = App eLetRecFibx (LitN 2) 
rtFib2CT    = eFib2
rtFib2RT    = LitN 2
rtFib2CTRT  = rtFib2RT
rtFib2DL    = Error (DownA (App (Lam "y" (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Eq (Var "n") (LitN 1)) (PrimBin Lt (Var "n") (LitN 1))) (LitN 1) (PrimBin Add (App (Var "x") (PrimBin Min (Var "n") (LitN 1))) (App (Var "x") (PrimBin Min (Var "n") (LitN 2)))))) (App (Var "x") (Var "y")))) (LitN 2)))
rtFib2UL    = AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TIf,AST [TagExpr (TPrimBin Or),AST [TagExpr (TPrimBin Eq),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]],AST [TagExpr (TPrimBin Lt),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]]],AST [TagExpr TLitN,LitN 1],AST [TagExpr (TPrimBin Add),AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimBin Min),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimBin Min),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 2]]]]]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]],AST [TagExpr TLitN,LitN 2]]
rtFib2ULDL  = eFib2
tyFib2 = TyInt


tFib3 =(eFib3, rtFib3CT, rtFib3RT, rtFib3CTRT, rtFib3DL, rtFib3UL ,rtFib3ULDL, "ttFib3")
eFib3 = App eLetRecFibx (LitN 3) 
rtFib3CT    = eFib3
rtFib3RT    = LitN 3
rtFib3CTRT  = rtFib3RT
rtFib3DL    = Error (DownA (App (Lam "y" (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Eq (Var "n") (LitN 1)) (PrimBin Lt (Var "n") (LitN 1))) (LitN 1) (PrimBin Add (App (Var "x") (PrimBin Min (Var "n") (LitN 1))) (App (Var "x") (PrimBin Min (Var "n") (LitN 2)))))) (App (Var "x") (Var "y")))) (LitN 3)))
rtFib3UL    = AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TIf,AST [TagExpr (TPrimBin Or),AST [TagExpr (TPrimBin Eq),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]],AST [TagExpr (TPrimBin Lt),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]]],AST [TagExpr TLitN,LitN 1],AST [TagExpr (TPrimBin Add),AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimBin Min),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimBin Min),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 2]]]]]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]],AST [TagExpr TLitN,LitN 3]]
rtFib3ULDL  = eFib3
tyFib3 = TyInt


tFib4 =(eFib4, rtFib4CT, rtFib4RT, rtFib4CTRT, rtFib4DL, rtFib4UL ,rtFib4ULDL, "ttFib4")
eFib4 = App eLetRecFibx (LitN 4) 
rtFib4CT    = eFib4
rtFib4RT    = LitN 5
rtFib4CTRT  = rtFib4RT
rtFib4DL    = Error (DownA (App (Lam "y" (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Eq (Var "n") (LitN 1)) (PrimBin Lt (Var "n") (LitN 1))) (LitN 1) (PrimBin Add (App (Var "x") (PrimBin Min (Var "n") (LitN 1))) (App (Var "x") (PrimBin Min (Var "n") (LitN 2)))))) (App (Var "x") (Var "y")))) (LitN 4)))
rtFib4UL    = AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TIf,AST [TagExpr (TPrimBin Or),AST [TagExpr (TPrimBin Eq),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]],AST [TagExpr (TPrimBin Lt),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]]],AST [TagExpr TLitN,LitN 1],AST [TagExpr (TPrimBin Add),AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimBin Min),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimBin Min),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 2]]]]]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]],AST [TagExpr TLitN,LitN 4]]
rtFib4ULDL  = eFib4
tyFib4 = TyInt

-- tFib5 =(eFib5, rtFib5CT, rtFib5RT, rtFib5CTRT, rtFib5DL, rtFib5UL ,rtFib5ULDL, "ttFib5")
-- eFib5 = App eLetRecFibx (LitN 5) 
-- rFib5CT = App eLetRecFibx (LitN 5) 

-- tFib6 =(eFib6, rtFib6CT, rtFib6RT, rtFib6CTRT, rtFib6DL, rtFib6UL ,rtFib6ULDL, "ttFib6")
-- eFib6 = App eLetRecFibx (LitN 6) 
-- rFib6CT = App eLetRecFibx (LitN 6) 

-- tFib7 =(eFib7, rtFib7CT, rtFib7RT, rtFib7CTRT, rtFib7DL, rtFib7UL ,rtFib7ULDL, "ttFib7")
-- eFib7 = App eLetRecFibx (LitN 7) 
-- rFib7CT = App eLetRecFibx (LitN 7) 

eFib80 = App eLetRecFibx (LitN 80) 

tFib8 =(eFib8, rtFib8CT, rtFib8RT, rtFib8CTRT, rtFib8DL, rtFib8UL ,rtFib8ULDL, "ttFib8")
eFib8 = App eLetRecFibx (LitN 8) 
rtFib8CT    = eFib8
rtFib8RT    = LitN 34
rtFib8CTRT  = rtFib8RT
rtFib8DL    = Error (DownA (App (Lam "y" (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Eq (Var "n") (LitN 1)) (PrimBin Lt (Var "n") (LitN 1))) (LitN 1) (PrimBin Add (App (Var "x") (PrimBin Min (Var "n") (LitN 1))) (App (Var "x") (PrimBin Min (Var "n") (LitN 2)))))) (App (Var "x") (Var "y")))) (LitN 8)))
rtFib8UL    = AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TIf,AST [TagExpr (TPrimBin Or),AST [TagExpr (TPrimBin Eq),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]],AST [TagExpr (TPrimBin Lt),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]]],AST [TagExpr TLitN,LitN 1],AST [TagExpr (TPrimBin Add),AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimBin Min),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimBin Min),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 2]]]]]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]],AST [TagExpr TLitN,LitN 8]]
rtFib8ULDL  = eFib8
tyFib8 = TyInt

tFibFalse =(eFibFalse, rtFibFalseCT, rtFibFalseRT, rtFibFalseCTRT, rtFibFalseDL, rtFibFalseUL ,rtFibFalseULDL, "ttFibFalse")
eFibFalse = App eLetRecFibx (LitB False) 
rtFibFalseCT    = App (Lam "y" (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Eq (Var "n") (LitN 1)) (PrimBin Lt (Var "n") (LitN 1))) (LitN 1) (PrimBin Add (App (Var "x") (PrimBin Min (Var "n") (LitN 1))) (App (Var "x") (PrimBin Min (Var "n") (LitN 2)))))) (App (Var "x") (Var "y")))) (LitB False)
rtFibFalseRT    = Error (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Eq (Var "n") (LitN 1)) (PrimBin Lt (Var "n") (LitN 1))) (LitN 1) (PrimBin Add (App (Var "x") (PrimBin Min (Var "n") (LitN 1))) (App (Var "x") (PrimBin Min (Var "n") (LitN 2)))))) (Error (If (Error (PrimBin Or (Error (PrimBin Eq (LitB False) (LitN 1))) (Error (PrimBin Lt (LitB False) (LitN 1))))) (LitN 1) (PrimBin Add (App (Var "x") (PrimBin Min (LitB False) (LitN 1))) (App (Var "x") (PrimBin Min (LitB False) (LitN 2)))))))
rtFibFalseCTRT  = rtFibFalseRT
rtFibFalseDL    = Error (DownA (App (Lam "y" (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Eq (Var "n") (LitN 1)) (PrimBin Lt (Var "n") (LitN 1))) (LitN 1) (PrimBin Add (App (Var "x") (PrimBin Min (Var "n") (LitN 1))) (App (Var "x") (PrimBin Min (Var "n") (LitN 2)))))) (App (Var "x") (Var "y")))) (LitB False)))
rtFibFalseUL    = AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TIf,AST [TagExpr (TPrimBin Or),AST [TagExpr (TPrimBin Eq),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]],AST [TagExpr (TPrimBin Lt),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]]],AST [TagExpr TLitN,LitN 1],AST [TagExpr (TPrimBin Add),AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimBin Min),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 1]]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimBin Min),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "n"]],AST [TagExpr TLitN,LitN 2]]]]]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]]],AST [TagExpr TLitB,LitB False]]
rtFibFalseULDL  = eFibFalse
tyFibFalse = TyError (LitB False) TyBool TyInt



-- tLetRecFibx =(eLetRecFibx, rLetRecFibx, "tLetRecFibx")
eLetRecFibx = Lam "y" (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Eq (Var "n") (LitN 1)) (PrimBin Lt (Var "n") (LitN 1))) (LitN 1) (PrimBin Add (App (Var "x") (PrimBin Min  (Var  "n") (LitN 1))) (App (Var "x") (PrimBin Min  (Var  "n") (LitN 2)))))) (App (Var "x") (Var "y")))

-- rLetRecFibxCT = Lam "y" (LetRec "x" (Lam "n" (If (PrimBin Or (PrimBin Lt (Var "n") (LitN 1)) (PrimBin Eq (Var "n") (LitN 0))) (LitN 1) (PrimBin Add (App (Var "x") (Var "n")) (App (Var "x") (PrimBin Min  (Var "n") (LitN 1)))))) (App (Var "x") (Var "y") ))
    

tMartin :: [TestList]
tMartin = [tLetRecMartin, tLetRecMartinPlus, tLetRecMartinId]
tyMartin = [tyLetRecMartin, tyLetRecMartinPlus, tyLetRecMartinId]

tLetRecMartin =(eLetRecMartin, rtLetRecMartinCT, rtLetRecMartinRT, rtLetRecMartinCTRT, rtLetRecMartinDL, rtLetRecMartinUL ,rtLetRecMartinULDL, "ttLetRecMartin")
eLetRecMartin = LetRec "f" (Var "f") (LetRec "x" (Var "x") (App (Var "f") (Var "x")))
rtLetRecMartinCT = eLetRecMartin
rtLetRecMartinRT = eLetRecMartin
rtLetRecMartinCTRT =  eLetRecMartin
rtLetRecMartinDL = Error (DownA eLetRecMartin)
rtLetRecMartinUL = AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "f"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "f"]],AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "f"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]]]]
rtLetRecMartinULDL = eLetRecMartin
tyLetRecMartin = TyVar "Type3{fresh}" 

tLetRecMartinPlus =(eLetRecMartinPlus, rtLetRecMartinPlusCT, rtLetRecMartinPlusRT, rtLetRecMartinPlusCTRT, rtLetRecMartinPlusDL, rtLetRecMartinPlusUL ,rtLetRecMartinPlusULDL, "ttLetRecMartinPlus")
eLetRecMartinPlus = (Let "f" (Lam "y" (PrimBin Add (Var "y") (Var "y")) ) (Let "x" (LitN 2)  (LetRec "f" (Var "f") (LetRec "x" (Var "x") (App (Var "f") (Var "x")))))) 
rtLetRecMartinPlusCT = eLetRecMartinPlus
rtLetRecMartinPlusRT = LitN 4
rtLetRecMartinPlusCTRT = rtLetRecMartinPlusRT
rtLetRecMartinPlusDL =Error (DownA eLetRecMartinPlus)
rtLetRecMartinPlusUL = AST [TagExpr TLet,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "f"]],AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr (TPrimBin Add),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]]],AST [TagExpr TLet,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLitN,LitN 2],AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "f"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "f"]],AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "f"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]]]]]]
rtLetRecMartinPlusULDL = eLetRecMartinPlus
tyLetRecMartinPlus = TyInt



tLetRecMartinId =(eLetRecMartinId, rtLetRecMartinIdCT, rtLetRecMartinIdRT, rtLetRecMartinIdCTRT, rtLetRecMartinIdDL, rtLetRecMartinIdUL ,rtLetRecMartinIdULDL, "ttLetRecMartinId")
eLetRecMartinId= (LetRec "f" (Lam "x" (App (Var "f") (Var "x"))) (App (Var "f") (LitN 17)))
rLetRecMartinIdCT = (LetRec "f" (Lam "x" (App (Var "f") (Var "x"))) (App (Var "f") (LitN 17)))
rtLetRecMartinIdCT = eLetRecMartinId 
rtLetRecMartinIdRT = LetRec "f" (Lam "x" (App (Var "f") (Var "x"))) (App (Lam "x" (App (Var "f") (Var "x"))) (LitN 17))
rtLetRecMartinIdCTRT = rtLetRecMartinIdRT
rtLetRecMartinIdDL = Error (DownA  eLetRecMartinId)
rtLetRecMartinIdUL = AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "f"]],AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "f"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "f"]],AST [TagExpr TLitN,LitN 17]]]
rtLetRecMartinIdULDL = eLetRecMartinId
tyLetRecMartinId = TyVar "Type6{fresh}" 










tRandom :: [TestList]
tRandom = [tRandom1, tRandom2, tRandom3]
tyRandom = [tyRandom1, tyRandom2, tyRandom3]

tRandom1 = (eRandom1, rtRandom1CT, rtRandom1RT, rtRandom1CTRT, rtRandom1DL, rtRandom1UL ,rtRandom1ULDL, "ttRandom1")
eRandom1 = LetRec "y"  (Lam "x" (If (PrimBin Eq (Var "x") (LitN 1)) (App (Var "y") (Lam "x" (PrimBin Add (Var "x") (LitN 1))))(App (Var "y") (Lam "x" (PrimBin Min (Var "x") (LitN 1))))))  (App (Var "y") (LitN 3))
rtRandom1CT = eRandom1
rtRandom1RT = Error (LetRec "y" (Lam "x" (If (PrimBin Eq (Var "x") (LitN 1)) (App (Var "y") (Lam "x" (PrimBin Add (Var "x") (LitN 1)))) (App (Var "y") (Lam "x" (PrimBin Min (Var "x") (LitN 1)))))) (Error (If (Error (PrimBin Eq (Lam "x" (PrimBin Min (Var "x") (LitN 1))) (LitN 1))) (App (Var "y") (Lam "x" (PrimBin Add (Var "x") (LitN 1)))) (App (Var "y") (Lam "x" (PrimBin Min (Var "x") (LitN 1)))))))
rtRandom1CTRT = rtRandom1RT
rtRandom1DL = Error (DownA eRandom1)
rtRandom1UL = AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TIf,AST [TagExpr (TPrimBin Eq),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLitN,LitN 1]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimBin Add),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLitN,LitN 1]]]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr (TPrimBin Min),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLitN,LitN 1]]]]]],AST [TagExpr TApp,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TLitN,LitN 3]]]
rtRandom1ULDL = eRandom1
tyRandom1 = TyVar "Type11{fresh}"  

tRandom2 =(eRandom2, rtRandom2CT, rtRandom2RT, rtRandom2CTRT, rtRandom2DL, rtRandom2UL ,rtRandom2ULDL, "ttRandom2")
eRandom2 = Eval (AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]],AST [TagExpr TLitN, (PrimBin Add (LitN 3) (LitN 2))]])
rtRandom2CT = eRandom2
rtRandom2RT = LitN 5
rtRandom2CTRT = rtRandom2RT
rtRandom2DL = Error (DownA eRandom2)
rtRandom2UL = AST [TagExpr TEval,AST [TagExpr TPromote,TagExpr TApp,AST [TagExpr TPromote,TagExpr TLam,AST [TagExpr TPromote,TagExpr TVar,AST [TagExpr TPromote,TagExpr TVarRep,AST [TagExpr TVarRep,VarRep "x"]]],AST [TagExpr TPromote,TagExpr TVar,AST [TagExpr TPromote,TagExpr TVarRep,AST [TagExpr TVarRep,VarRep "x"]]]],AST [TagExpr TPromote,TagExpr TLitN,AST [TagExpr (TPrimBin Add),AST [TagExpr TLitN,LitN 3],AST [TagExpr TLitN,LitN 2]]]]]
rtRandom2ULDL = eRandom2
tyRandom2 = TyInt

tRandom3 =(eRandom3, rtRandom3CT, rtRandom3RT, rtRandom3CTRT, rtRandom3DL, rtRandom3UL ,rtRandom3ULDL, "ttRandom3")
eRandom3 = Eval (AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]],AST [TagExpr TLitN, (PrimBin Add (LitN 3) (LitN 2))]])
rtRandom3CT = eRandom3
rtRandom3RT = LitN 5
rtRandom3CTRT = rtRandom3RT
rtRandom3DL = Error (DownA eRandom3)
rtRandom3UL = AST [TagExpr TEval,AST [TagExpr TPromote,TagExpr TApp,AST [TagExpr TPromote,TagExpr TLam,AST [TagExpr TPromote,TagExpr TVar,AST [TagExpr TPromote,TagExpr TVarRep,AST [TagExpr TVarRep,VarRep "x"]]],AST [TagExpr TPromote,TagExpr TVar,AST [TagExpr TPromote,TagExpr TVarRep,AST [TagExpr TVarRep,VarRep "x"]]]],AST [TagExpr TPromote,TagExpr TLitN,AST [TagExpr (TPrimBin Add),AST [TagExpr TLitN,LitN 3],AST [TagExpr TLitN,LitN 2]]]]]
rtRandom3ULDL = eRandom2
tyRandom3 = TyCodeUnP
























tAST :: [TestList]
tAST = tASTTVar ++ tASTTLam ++ tEval ++ tLetDA -- ++ tASTTApp ++ tASTTPrimBinAdd ++ tASTTIf
tyAST = tyASTTVar ++ tyASTTLam ++ tyEval ++ tyLetDA -- ++ tASTTApp ++ tASTTPrimBinAdd ++ tASTTIf


tASTTVar :: [TestList]
tASTTVar = [tASTTVar1, tASTTVar2, tASTTVar3, tASTTVar4]
tyASTTVar = [tyASTTVar1, tyASTTVar2, tyASTTVar3, tyASTTVar4]

tASTTVar1 =(eASTTVar1, rtASTTVar1CT, rtASTTVar1RT, rtASTTVar1CTRT, rtASTTVar1DL, rtASTTVar1UL ,rtASTTVar1ULDL, "ttASTTVar1")
eASTTVar1 = AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "c"]]
rtASTTVar1CT = AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "c"]]
rtASTTVar1RT = rtASTTVar1CT
rtASTTVar1CTRT =  rtASTTVar1RT
rtASTTVar1DL = Var "c"
rtASTTVar1UL = AST [TagExpr TPromote,TagExpr TVar,AST [TagExpr TPromote,TagExpr TVarRep,AST [TagExpr TVarRep,VarRep "c"]]]
rtASTTVar1ULDL =eASTTVar1
tyASTTVar1 = TyCodeUnP

tASTTVar2 =(eASTTVar2, rtASTTVar2CT, rtASTTVar2RT, rtASTTVar2CTRT, rtASTTVar2DL, rtASTTVar2UL ,rtASTTVar2ULDL, "ttASTTVar2")
eASTTVar2 = AST [TagExpr TVar, AST [TagExpr TVarRep, (LitB True)]]
rtASTTVar2CT = AST [TagExpr TVar,AST [TagExpr TVarRep,(LitB True)]]
rtASTTVar2RT = rtASTTVar2CT
rtASTTVar2CTRT =  rtASTTVar2RT
rtASTTVar2DL = Error (DownA (AST [TagExpr TVar,Error (Error (DownA (AST [TagExpr TVarRep,LitB True])))]))
rtASTTVar2UL = AST [TagExpr TPromote,TagExpr TVar,AST [TagExpr TPromote,TagExpr TVarRep,AST [TagExpr TLitB,LitB True]]]
rtASTTVar2ULDL =eASTTVar2
tyASTTVar2 = TyCodeUnP


tASTTVar3 =(eASTTVar3, rtASTTVar3CT, rtASTTVar3RT, rtASTTVar3CTRT, rtASTTVar3DL, rtASTTVar3UL ,rtASTTVar3ULDL, "ttASTTVar3")
eASTTVar3 = AST [TagExpr TVar, DownA (AST [TagExpr TPromote,TagExpr TVarRep,AST [TagExpr TVarRep,VarRep "x"]])]
rtASTTVar3CT = AST [TagExpr TVar, AST [TagExpr TVarRep,VarRep "x"]]
rtASTTVar3RT = Error (AST [TagExpr TVar,Error (DownA (AST [TagExpr TPromote,TagExpr TVarRep,AST [TagExpr TVarRep,VarRep "x"]]))])
rtASTTVar3CTRT =  AST [TagExpr TVar, AST [TagExpr TVarRep,VarRep "x"]]
rtASTTVar3DL = Var "x"
rtASTTVar3UL = AST [TagExpr TPromote,TagExpr TVar,AST [TagExpr TPromote,TagExpr TVarRep,AST [TagExpr TVarRep,VarRep "x"]]]
rtASTTVar3ULDL =AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]
tyASTTVar3 = TyCodeUnP


tASTTVar4 =(eASTTVar4, rtASTTVar4CT, rtASTTVar4RT, rtASTTVar4CTRT, rtASTTVar4DL, rtASTTVar4UL ,rtASTTVar4ULDL, "ttASTTVar4")
eASTTVar4 = (LetDA "x" (AST [TagExpr TVarRep, VarRep "y"]) (AST [TagExpr TVarRep, DownA (App (Lam "x" (Var "x")) (Var "x"))]))
rtASTTVar4CT = AST [TagExpr TVarRep,VarRep "y"]
rtASTTVar4RT = Error eASTTVar4
rtASTTVar4CTRT = AST [TagExpr TVarRep,VarRep "y"]
rtASTTVar4DL = Error (DownA eASTTVar4)
rtASTTVar4UL = Error (UpA eASTTVar4)
rtASTTVar4ULDL = Error (Error (UpA eASTTVar4))
tyASTTVar4 = TyCodeUnP

tASTTLam :: [TestList]
tASTTLam = [tASTTLam1, tASTTLam2, tASTTLam3, tASTTLam4]
tyASTTLam = [tyASTTLam1, tyASTTLam2, tyASTTLam3, tyASTTLam4]

tASTTLam1 =(eASTTLam1, rtASTTLam1CT, rtASTTLam1RT, rtASTTLam1CTRT, rtASTTLam1DL, rtASTTLam1UL ,rtASTTLam1ULDL, "ttASTTLam1")
eASTTLam1 = AST [TagExpr TLam, (AST [TagExpr TVar, AST [TagExpr TVarRep, (VarRep "x")]]), (AST [TagExpr TVar, AST [TagExpr TVarRep, (VarRep "x")]])]
rtASTTLam1CT = eASTTLam1
rtASTTLam1RT = eASTTLam1
rtASTTLam1CTRT = eASTTLam1
rtASTTLam1DL = Lam "x" (Var "x")
rtASTTLam1UL = AST [TagExpr TPromote,TagExpr TLam,AST [TagExpr TPromote,TagExpr TVar,AST [TagExpr TPromote,TagExpr TVarRep,AST [TagExpr TVarRep,VarRep "x"]]],AST [TagExpr TPromote,TagExpr TVar,AST [TagExpr TPromote,TagExpr TVarRep,AST [TagExpr TVarRep,VarRep "x"]]]]
rtASTTLam1ULDL = eASTTLam1
tyASTTLam1 = TyCodeUnP

tASTTLam2 =(eASTTLam2, rtASTTLam2CT, rtASTTLam2RT, rtASTTLam2CTRT, rtASTTLam2DL, rtASTTLam2UL ,rtASTTLam2ULDL, "ttASTTLam2")
eASTTLam2 = AST [TagExpr TLam, (LitB True), (LitB True)]
rtASTTLam2CT = eASTTLam2
rtASTTLam2RT = eASTTLam2
rtASTTLam2CTRT = eASTTLam2
rtASTTLam2DL = Error (DownA eASTTLam2)
rtASTTLam2UL = AST [TagExpr TPromote, TagExpr TLam, AST [TagExpr TLitB, (LitB True)], AST [TagExpr TLitB, (LitB True)]]
rtASTTLam2ULDL = eASTTLam2
tyASTTLam2 = TyCodeUnP

tASTTLam3 =(eASTTLam3, rtASTTLam3CT, rtASTTLam3RT, rtASTTLam3CTRT, rtASTTLam3DL, rtASTTLam3UL ,rtASTTLam3ULDL, "ttASTTLam3")
eASTTLam3 = AST [TagExpr TLam, (LitB True), (VarRep "x")]
rtASTTLam3CT = eASTTLam3
rtASTTLam3RT = eASTTLam3
rtASTTLam3CTRT = eASTTLam3
rtASTTLam3DL =Error (DownA eASTTLam3)
rtASTTLam3UL = AST [TagExpr TPromote, TagExpr TLam, AST [TagExpr TLitB, (LitB True)], AST [TagExpr TVarRep, (VarRep "x")]] 
rtASTTLam3ULDL = eASTTLam3
tyASTTLam3 = TyCodeUnP

tASTTLam4 =(eASTTLam4, rtASTTLam4CT, rtASTTLam4RT, rtASTTLam4CTRT, rtASTTLam4DL, rtASTTLam4UL ,rtASTTLam4ULDL, "ttASTTLam4")
eASTTLam4 = AST [TagExpr TLam, AST [TagExpr TVar, UpA (VarRep "x")], (AST [TagExpr TVar, AST [TagExpr TVarRep, GenSym]])]
rtASTTLam4CT =AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,GenSym]]]
rtASTTLam4RT = Error (AST [TagExpr TLam,Error (AST [TagExpr TVar,Error (UpA (VarRep "x"))]),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "freshZ1"]]])
rtASTTLam4CTRT = AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "freshZ1"]]]
rtASTTLam4DL = Error (DownA eASTTLam4)
rtASTTLam4UL = AST [TagExpr TPromote,TagExpr TLam,AST [TagExpr TPromote,TagExpr TVar,AST [TagExpr TPromote,TagExpr TVarRep,AST [TagExpr TVarRep,VarRep "x"]]],AST [TagExpr TPromote,TagExpr TVar,AST [TagExpr TPromote,TagExpr TVarRep,AST [TagExpr TGenSym]]]]
rtASTTLam4ULDL = AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,GenSym]]]
tyASTTLam4 = TyCodeUnP


-- tASTTApp :: [TestList]
-- tASTTApp = [tASTTApp1, tASTTApp2, tASTTApp3, tASTTApp4, tASTTApp5]

-- tASTTApp1 =(eASTTApp1, rtASTTApp1CT, rtASTTApp1RT, rtASTTApp1CTRT, rtASTTApp1DL, rtASTTApp1UL ,rtASTTApp1ULDL, "ttASTTApp1")
-- eASTTApp1 = AST [TagExpr TApp, ( AST [TagExpr TVar, (VarRep "x")]), (AST [TagExpr TVar, (VarRep "x")])]
-- rASTTApp1CT = AST [TagExpr TApp, ( AST [TagExpr TVar, (VarRep "x")]), (AST [TagExpr TVar, (VarRep "x")])]

-- tASTTApp2 =(eASTTApp2, rtASTTApp2CT, rtASTTApp2RT, rtASTTApp2CTRT, rtASTTApp2DL, rtASTTApp2UL ,rtASTTApp2ULDL, "ttASTTApp2")
-- eASTTApp2 = AST [TagExpr TApp, (App (Lam "x" (PrimBin Mul (Var "x") (Var "x"))) (PrimBin Add (LitN 3) (LitN 2))), (Var "x")]
-- rASTTApp2CT = AST [TagExpr TApp, (App (Lam "x" (PrimBin Mul (Var "x") (Var "x"))) (PrimBin Add (LitN 3) (LitN 2))), (Var "x")]

-- tASTTApp3 =(eASTTApp3, rtASTTApp3CT, rtASTTApp3RT, rtASTTApp3CTRT, rtASTTApp3DL, rtASTTApp3UL ,rtASTTApp3ULDL, "ttASTTApp3")
-- eASTTApp3 = AST [TagExpr TApp, (LitB True), (VarRep "x")]
-- rASTTApp3CT = AST [TagExpr TApp, (LitB True), (VarRep "x")]

-- tASTTApp4 =(eASTTApp4, rtASTTApp4CT, rtASTTApp4RT, rtASTTApp4CTRT, rtASTTApp4DL, rtASTTApp4UL ,rtASTTApp4ULDL, "ttASTTApp4")
-- eASTTApp4 = AST [TagExpr TApp, (LitB True), (AST [TagExpr TVar, (LitN 3)])]
-- rASTTApp4CT = AST [TagExpr TApp, (LitB True), (AST [TagExpr TVar, (LitN 3)])]

-- tASTTApp5 =(eASTTApp5, rtASTTApp5CT, rtASTTApp5RT, rtASTTApp5CTRT, rtASTTApp5DL, rtASTTApp5UL ,rtASTTApp5ULDL, "ttASTTApp5")
-- eASTTApp5 = AST [TagExpr TApp, (LitB True), (AST [TagExpr TVar, (LitN 3)])]
-- rASTTApp5CT = AST [TagExpr TApp, (LitB True), (AST [TagExpr TVar, (LitN 3)])]



-- tASTTPrimBinAdd :: [TestList]
-- tASTTPrimBinAdd = [tASTTPrimBinAdd1, tASTTPrimBinAdd2, tASTTPrimBinAdd3, tASTTPrimBinAdd4, tASTTPrimBinAdd5]

-- tASTTPrimBinAdd1 =(eASTTPrimBinAdd1, rtASTTPrimBinAdd1CT, rtASTTPrimBinAdd1RT, rtASTTPrimBinAdd1CTRT, rtASTTPrimBinAdd1DL, rtASTTPrimBinAdd1UL ,rtASTTPrimBinAdd1ULDL, "ttASTTPrimBinAdd1")
-- eASTTPrimBinAdd1 = (AST [TagExpr (TPrimBin Add), UpA (Let "x" (LitN 3) (Var "x")), AST [TagExpr TVar, (AST [TagExpr TVarRep, VarRep "x"])]])
-- rASTTPrimBinAdd1CT = AST [TagExpr (TPrimBin Add),AST [TagExpr TLet,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLitN,LitN 3],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]]

-- tASTTPrimBinAdd2 =(eASTTPrimBinAdd2, rtASTTPrimBinAdd2CT, rtASTTPrimBinAdd2RT, rtASTTPrimBinAdd2CTRT, rtASTTPrimBinAdd2DL, rtASTTPrimBinAdd2UL ,rtASTTPrimBinAdd2ULDL, "ttASTTPrimBinAdd2")
-- eASTTPrimBinAdd2 = AST [TagExpr TApp, (App (Lam "x" (PrimBin Mul (Var "x") (Var "x"))) (PrimBin Add (LitN 3) (LitN 2))), (Var "x")]
-- rASTTPrimBinAdd2CT = AST [TagExpr TApp, (App (Lam "x" (PrimBin Mul (Var "x") (Var "x"))) (PrimBin Add (LitN 3) (LitN 2))), (Var "x")]

-- tASTTPrimBinAdd3 =(eASTTPrimBinAdd3, rtASTTPrimBinAdd3CT, rtASTTPrimBinAdd3RT, rtASTTPrimBinAdd3CTRT, rtASTTPrimBinAdd3DL, rtASTTPrimBinAdd3UL ,rtASTTPrimBinAdd3ULDL, "ttASTTPrimBinAdd3")
-- eASTTPrimBinAdd3 = AST [TagExpr TApp, (LitB True), (VarRep "x")]
-- rASTTPrimBinAdd3CT = AST [TagExpr TApp, (LitB True), (VarRep "x")]

-- tASTTPrimBinAdd4 =(eASTTPrimBinAdd4, rtASTTPrimBinAdd4CT, rtASTTPrimBinAdd4RT, rtASTTPrimBinAdd4CTRT, rtASTTPrimBinAdd4DL, rtASTTPrimBinAdd4UL ,rtASTTPrimBinAdd4ULDL, "ttASTTPrimBinAdd4")
-- eASTTPrimBinAdd4 = AST [TagExpr TApp, (LitB True), (AST [TagExpr TVar, (LitN 3)])]
-- rASTTPrimBinAdd4CT = AST [TagExpr TApp, (LitB True), (AST [TagExpr TVar, (LitN 3)])]

-- tASTTPrimBinAdd5 =(eASTTPrimBinAdd5, rtASTTPrimBinAdd5CT, rtASTTPrimBinAdd5RT, rtASTTPrimBinAdd5CTRT, rtASTTPrimBinAdd5DL, rtASTTPrimBinAdd5UL ,rtASTTPrimBinAdd5ULDL, "ttASTTPrimBinAdd5")
-- eASTTPrimBinAdd5 = AST [TagExpr TApp, (LitB True), (AST [TagExpr TVar, (LitN 3)])]
-- rASTTPrimBinAdd5CT = AST [TagExpr TApp, (LitB True), (AST [TagExpr TVar, (LitN 3)])]

    
-- tASTTIf :: [TestList]
-- tASTTIf = [tASTTIf1] --, tASTTIf2, tASTTIf3, tASTTIf4, tASTTIf5]

-- tASTTIf1 =(eASTTIfCT1, rtASTTIf1CT, rtASTTIf1RT, rtASTTIf1CTRT, rtASTTIf1DL, rtASTTIf1UL ,rtASTTIf1ULDL, "ttASTTIf1")
-- eASTTIfCT1 = DownA ( AST [TagExpr TIf,AST [TagExpr TLitB,LitB True],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]])
-- rASTTIfCT1CT = (If (LitB True) (Var "x") (Var "y"))
    
-- -- tApp = (eApp, rApp, "tApp")
-- -- eApp = App 
-- -- rApp = 

tEval = [tEval1, tEval2, tEval3]
tyEval = [tyEval1, tyEval2, tyEval3]

tEval1 =(eEval1, rtEval1CT, rtEval1RT, rtEval1CTRT, rtEval1DL, rtEval1UL ,rtEval1ULDL, "ttEval1")
eEval1 = DownA ( Eval (AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]],AST [TagExpr TPromote,TagExpr TVar,AST [TagExpr TPromote,TagExpr TVarRep,AST [TagExpr TVarRep,VarRep "x"]]]]))
rtEval1CT = Var "x"
rtEval1RT = Error (eEval1)
rtEval1CTRT = Var "x"
rtEval1DL =Error (DownA (Error (Error eEval1)))
rtEval1UL = Error (UpA eEval1)
-- DownA (Eval (AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]],AST [TagExpr TPromote,TagExpr TVar,AST [TagExpr TPromote,TagExpr TVarRep,AST [TagExpr TVarRep,VarRep "x"]]]])))
rtEval1ULDL = Error (Error (UpA eEval1))
tyEval1 = TyVar "Type1{x}"

tEval2 =(eEval2, rtEval2CT, rtEval2RT, rtEval2CTRT, rtEval2DL, rtEval2UL ,rtEval2ULDL, "ttEval2")
eEval2 =  Eval (App (Lam "x" (UpA (Var "x")))  (LetRec "y"  ( PrimBin Min (Var "y") (Var "y")) (Var "x")))
rtEval2CT = Eval (App (Lam "x" (AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]])) (LetRec "y" (PrimBin Min (Var "y") (Var "y")) (Var "x")))
rtEval2RT = Error (Error (Error (UpA (LetRec "y" (PrimBin Min (Var "y") (Var "y")) (Var "x")))))
rtEval2CTRT = Var "x"
rtEval2DL = Error (DownA eEval2)
rtEval2UL = AST [TagExpr TEval,AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TPromote,TagExpr TVar,AST [TagExpr TPromote,TagExpr TVarRep,AST [TagExpr TVarRep,VarRep "x"]]]],AST [TagExpr TLetRec,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr (TPrimBin Min),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]]]]
rtEval2ULDL = Eval (App (Lam "x" (AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]])) (LetRec "y" (PrimBin Min (Var "y") (Var "y")) (Var "x")))
tyEval2 = TyCodeUnP

tEval3 =(eEval3, rtEval3CT, rtEval3RT, rtEval3CTRT, rtEval3DL, rtEval3UL ,rtEval3ULDL, "ttEval3")
eEval3 =  Eval (Lam "x" (App (Lam "y" (Var "t") )  (UpA (Var "o"))))
rtEval3CT = Eval (Lam "x" (App (Lam "y" (Var "t")) (AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "o"]])))
rtEval3RT = Error (Error (DownA (Lam "x" (Var "t"))))
rtEval3CTRT = Error (Error (DownA (Lam "x" (Var "t"))))
rtEval3DL = Error (DownA (Eval (Lam "x" (App (Lam "y" (Var "t")) (UpA (Var "o"))))))
rtEval3UL =  AST [TagExpr TEval,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "t"]]],AST [TagExpr TPromote,TagExpr TVar,AST [TagExpr TPromote,TagExpr TVarRep,AST [TagExpr TVarRep,VarRep "o"]]]]]]
rtEval3ULDL = Eval (Lam "x" (App (Lam "y" (Var "t")) (AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "o"]])))
tyEval3 = TyErrorEq


tLetDA :: [TestList]
tLetDA = [tLetDA1, tLetDA2, tLetDA3, tLetDA4] --, tASTTIf2, tASTTIf3, tASTTIf4, tASTTIf5]
tyLetDA = [tyLetDA1, tyLetDA2, tyLetDA3, tyLetDA4] --, tASTTIf2, tASTTIf3, tASTTIf4, tASTTIf5]



tLetDA1 =(eLetDA1, rtLetDA1CT, rtLetDA1RT, rtLetDA1CTRT, rtLetDA1DL, rtLetDA1UL ,rtLetDA1ULDL, "ttLetDA1")
eLetDA1 = LetDA "x" (AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]],AST [TagExpr (TPrimUni Not),AST [TagExpr TLitB,LitB True]]]) (Eval ((Var "x")))
rtLetDA1CT = Eval (AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]],AST [TagExpr (TPrimUni Not),AST [TagExpr TLitB,LitB True]]])
rtLetDA1RT = Error eLetDA1
rtLetDA1CTRT = LitB False
rtLetDA1DL = Error (DownA eLetDA1)
rtLetDA1UL = Error (UpA eLetDA1)
rtLetDA1ULDL = Error (Error (UpA eLetDA1))
tyLetDA1 = TyCodeUnP

tLetDA2 =(eLetDA2, rtLetDA2CT, rtLetDA2RT, rtLetDA2CTRT, rtLetDA2DL, rtLetDA2UL ,rtLetDA2ULDL, "ttLetDA2")
eLetDA2 = LetDA "x" (Let "x" (AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "t"]]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "o"]]]]) (Var "x")) (Eval (AST [TagExpr TApp, (Var "x"), UpA (LitN 3)]))
rtLetDA2CT = Eval (AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TApp,AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "y"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "t"]]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "o"]]]],AST [TagExpr TLitN,LitN 3]])
rtLetDA2RT = Error eLetDA2
rtLetDA2CTRT =  Var "t"
rtLetDA2DL = Error (DownA eLetDA2)
rtLetDA2UL = Error (UpA eLetDA2)
rtLetDA2ULDL = Error (Error (UpA eLetDA2))
tyLetDA2 = TyCodeUnP

tLetDA3 =(eLetDA3, rtLetDA3CT, rtLetDA3RT, rtLetDA3CTRT, rtLetDA3DL, rtLetDA3UL ,rtLetDA3ULDL, "ttLetDA3")
eLetDA3 = LetDA "y" (Let "x" (LitN 3) (UpA (PrimBin Add (Var "x") (Var "x")))) (Var "y")
rtLetDA3CT = AST [TagExpr (TPrimBin Add),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]]
rtLetDA3RT = Error (LetDA "y" (Let "x" (LitN 3) (UpA (PrimBin Add (Var "x") (Var "x")))) (Var "y"))
rtLetDA3CTRT =rtLetDA3CT
rtLetDA3DL = Error (DownA eLetDA3)
rtLetDA3UL = Error (UpA eLetDA3)
rtLetDA3ULDL = Error (Error (UpA eLetDA3))
tyLetDA3 = TyCodeUnP

tLetDA4 =(eLetDA4, rtLetDA4CT, rtLetDA4RT, rtLetDA4CTRT, rtLetDA4DL, rtLetDA4UL ,rtLetDA4ULDL, "ttLetDA4")
eLetDA4 = (LetDA "y" (Let "z" (UpA (Let "x" (LitN 3) (PrimBin Add (Var "x") (Var "x")))) (Var "z")) (Var "y")) 
rtLetDA4CT = AST [TagExpr TLet,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TLitN,LitN 3],AST [TagExpr (TPrimBin Add),AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x"]]]]
rtLetDA4RT = Error eLetDA4
rtLetDA4CTRT = rtLetDA4CT
rtLetDA4DL = Error (DownA eLetDA4)
rtLetDA4UL = Error (UpA eLetDA4)
rtLetDA4ULDL = Error (Error (UpA eLetDA4))
tyLetDA4 = TyCodeUnP




stgPwr = (Lam "powX"         (AST [TagExpr TLam,     AST [TagExpr TVar,  AST [TagExpr TVarRep,VarRep "x"]],    (LetRec "pow" (Lam "count" (If (PrimBin Gt (Var "count") (LitN 1)) (AST [TagExpr (TPrimBin Mul),AST [TagExpr TVar, AST [TagExpr TVarRep, VarRep "x"]], App (Var "pow") (PrimBin Min (Var "count") (LitN 1))]) (AST [TagExpr TVar, AST [TagExpr TVarRep,VarRep "x"]]))) (App (Var "pow") (Var "powX")))])          )


-- ithnaryproj2 = 
-- Lam "i" 
-- (Lam "n"
    -- (LetDA         
        -- "ith"         
        -- (AST [TagExpr TVar,  AST [TagExpr TVarRep, GenSym]])          
        -- (LetRec "N"
            -- (Lam "count"                             
                -- (If                                     
                    -- (PrimBin Lt (Var "count") (PrimBin Add (Var "n") (LitN 1)))                    
                    -- (If                                             
                        -- (PrimBin Eq (Var "count") (Var "i"))                        
                        -- (AST [                            
                            -- TagExpr TLam,
                            -- (Var "ith"),                             
                            -- App (Var "N") (PrimBin Add (Var "count") (LitN 1))                       
                            -- ])                                            
                        -- (AST [                            
                            -- TagExpr TLam,
                            -- AST [
                                -- TagExpr TVar,                                 
                                -- AST [                                    
                                    -- TagExpr TVarRep,
                                    -- GenSym                                    
                                    -- ]                                
                                -- ],                             
                            -- App (Var "N") (PrimBin Add (Var "count") (LitN 1)) 
                            -- ]
                        -- )
                    -- )
                    -- (Var "ith")                            
                -- )                     
            -- )                    
            -- (App (Var "N") (LitN 0))            
        -- )
    -- )
-- )


inproj = Lam "i" (Lam "n"    (LetDA                 "ith"                 (AST [TagExpr TVar,  AST [TagExpr TVarRep, GenSym]])                  (LetRec "N"            (Lam "count"                                             (If                                                         (PrimBin Lt (Var "count") (Var "n") )                                        (If                                                                     (PrimBin Eq (Var "count") (PrimBin Min (Var "i") (LitN 1)))                                                (AST [                                                        TagExpr TLam,                            (Var "ith"),                                                         App (Var "N") (PrimBin Add (Var "count") (LitN 1))                                                   ])                                                                    (AST [                                                        TagExpr TLam,                            AST [                                TagExpr TVar,                                                                 AST [                                                                        TagExpr TVarRep,                                    GenSym                                                                        ]                                                                ],                                                        App (Var "N") (PrimBin Add (Var "count") (LitN 1))                             ]                        ))(Var "ith")                            )                     )                    (App (Var "N") (LitN 0))            )))
inprojtest = Lam "ip" (Lam "np" (LetDA "i" (Var "ip") (LetDA "n" (Var "np") (LetDA "ith" (Var "iTH") (LetRec "N" (Lam "count" (If (PrimBin Eq (LitN 3) (Var "n")) (Var "ith") (App (Var "N") (LitN 3)))) (App (Var "N") (LitN 5)))))))
-- ithnaryproj = 
-- Lam "i" 
-- (Lam "n"
    -- (LetDA         
        -- "ith"         
        -- (AST [TagExpr TVar,  AST [TagExpr TVarRep, GenSym]])          
        -- (LetRec "N"
            -- (Lam "count"                             
                -- (If                                     
                    -- (PrimBin Gt (Var "count") (LitN 0))                    
                    -- (If                                             
                        -- (PrimBin Eq (Var "count") (Var "i"))                        
                        -- (AST [                            
                            -- TagExpr TLam,
                            -- (Var "ith"),                             
                            -- App (Var "N") (PrimBin Min (Var "count") (LitN 1))                       
                            -- ])                                            
                        -- (AST [                            
                            -- TagExpr TLam,
                            -- AST [
                                -- TagExpr TVar,                                 
                                -- AST [                                    
                                    -- TagExpr TVarRep,
                                    -- GenSym                                    
                                    -- ]                                
                                -- ],                             
                            -- App (Var "N") (PrimBin Min (Var "count") (LitN 1)) 
                            -- ]
                        -- )
                    -- )
                    -- (Var "ith")                            
                -- )                     
            -- )                    
            -- (App (Var "N") (Var "n"))            
        -- )
    -- )
-- )


oo12 = Lam "x1" (Lam "x2" (Var "x1"))

-- oo12UL = AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x1"]],AST [TagExpr TLam,AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x2"]],AST [TagExpr TVar,AST [TagExpr TVarRep,VarRep "x1"]]]]



oo14 = Lam "x1" (Lam "x2" (Lam "x3" (Lam "x4" (Var "x1"))))
-- 4oo8 = Lam "x1" (Lam "x2" (Lam "x3" (Lam "x4" (Lam "x5" (Lam "x6" (Lam "x7" (Lam "x8" (Var "x4"))))))))




ithnaryproj' = Lam "i" (Lam "n"     (LetRec "N"         (Lam "count"             (If                 (PrimBin Gt (Var "count") (LitN 1))                 (If                     (PrimBin Eq (Var "count") (Var "i"))                    (LetDA "ith" GenSym (AST [TagExpr TLam, AST [TagExpr TVar,  AST [TagExpr TVarRep, (Var "ith")]], App (Var "N") (PrimBin Min (Var "count") (LitN 1))]))                    (AST [TagExpr TLam, AST [TagExpr TVar, AST [TagExpr TVarRep, GenSym]], App (Var "N") (PrimBin Min (Var "count") (LitN 1))])                )                (AST [TagExpr TVar, AST [TagExpr TVarRep, (Var "ith")]])            )         )        (App (Var "N") (Var "n"))    ))



