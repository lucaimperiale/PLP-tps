module TypeInference (TypingJudgment, Result(..), inferType, inferNormal, normalize)

where

import Data.List(intersect, union, nub, sort)
import Exp
import Type
import Unification

------------
-- Errors --
------------
data Result a = OK a | Error String


--------------------
-- Type Inference --
--------------------
type TypingJudgment = (Context, AnnotExp, Type)

typeVarsT :: Type -> [Int]
typeVarsT = foldType (:[]) [] [] union id

typeVarsE :: Exp Type -> [Int]
typeVarsE = foldExp (const []) [] id id id [] [] (\r1 r2 r3 ->nub(r1++r2++r3)) (const setAdd) union typeVarsT union (\r1 r2 _ _ r3->nub(r1++r2++r3))
  where setAdd t r = union (typeVarsT t) r

typeVarsC :: Context -> [Int]
typeVarsC c = nub (concatMap (typeVarsT . evalC c) (domainC c))

typeVars :: TypingJudgment -> [Int]
typeVars (c, e, t) = sort $ union (typeVarsC c) (union (typeVarsE e) (typeVarsT t))

normalization :: [Int] -> [Subst]
normalization ns = foldr (\n rec (y:ys) -> extendS n (TVar  y) emptySubst : (rec ys)) (const []) ns [0..]

normalize :: TypingJudgment -> TypingJudgment
normalize j@(c, e, t) = let ss = normalization $ typeVars j in foldl (\(rc, re, rt) s ->(s <.> rc, s <.> re, s <.> rt)) j ss
  
inferType :: PlainExp -> Result TypingJudgment
inferType e = case infer' e 0 of
    OK (_, tj) -> OK tj
    Error s -> Error s
    
inferNormal :: PlainExp -> Result TypingJudgment
inferNormal e = case infer' e 0 of
    OK (_, tj) -> OK $ normalize tj
    Error s -> Error s


infer' :: PlainExp -> Int -> Result (Int, TypingJudgment)

infer' (SuccExp e)    n = case infer' e n of
                            OK (n', (c', e', t')) ->
                              case mgu [(t', TNat)] of
                                UOK subst -> OK (n',
                                                    (
                                                     subst <.> c',
                                                     subst <.> SuccExp e',
                                                     TNat
                                                    )
                                                )
                                UError u1 u2 -> uError u1 u2
                            res@(Error _) -> res

-- COMPLETAR DESDE AQUI

infer' ZeroExp                n = OK (n,(emptyContext,ZeroExp,TNat))
infer' (VarExp x)             n = OK (n+1,(extendC emptyContext x (TVar n),VarExp x ,TVar n))
infer' (AppExp u v)           n = case infer' u n of 
                                    OK(n_u,(c1,exprM,tau)) ->
                                      case infer' v n_u of
                                        OK (n_v,(c2,exprN,rho)) ->
                                          case mgu ( [(tau,TFun rho (TVar (n_v)))] ++ map (\x -> (evalC c1 x,evalC c2 x))(filter (\y -> elem y (domainC c2)) (domainC c1))) of
                                            UOK subst -> OK (n_v+1,
                                                    (
                                                     joinC [subst <.> c1,subst <.> c2],
                                                     subst <.> AppExp exprM exprN,
                                                     subst <.> TVar (n_v)
                                                    )
                                                )
                                            UError u1 u2 -> uError u1 u2
                                        res@(Error _) -> res
                                    res@(Error _) -> res
infer' (LamExp x _ e)         n = case infer' e n of
                                    OK (n', (c', e', t')) ->
                                      if elem x (domainC c') then OK (n',
                                                    (
                                                     removeC c' x,
                                                     LamExp x (evalC c' x) e',
                                                     TFun (evalC c' x) t'
                                                    ))
                                                            else OK (n'+1,
                                                    (
                                                     removeC c' x,
                                                     LamExp x (TVar n') e',
                                                     TFun (TVar n') t'
                                                    ))
                                      
                                    res@(Error _) -> res

-- OPCIONALES

infer' (PredExp e)            n = case infer' e n of
                                    OK (n', (c', e', t')) ->
                                      case mgu [(t', TNat)] of
                                        UOK subst -> OK (n',
                                                            (
                                                             subst <.> c',
                                                             subst <.> PredExp e',
                                                             TNat
                                                            )
                                                        )
                                        UError u1 u2 -> uError u1 u2
                                    res@(Error _) -> res
infer' (IsZeroExp e)          n = case infer' e n of
                                    OK (n', (c', e', t')) ->
                                      case mgu [(t', TNat)] of
                                        UOK subst -> OK (n',
                                                            (
                                                             subst <.> c',
                                                             subst <.> IsZeroExp e',
                                                             TBool
                                                            )
                                                        )
                                        UError u1 u2 -> uError u1 u2
                                    res@(Error _) -> res
infer' TrueExp                n = OK (n,(emptyContext,TrueExp,TBool))
infer' FalseExp               n = OK (n,(emptyContext,FalseExp,TBool))
infer' (IfExp u v w)          n = case infer' u n of 
                                    OK(n_u,(c1,exprM,rho)) ->
                                      case infer' v n_u of
                                        OK (n_v,(c2,exprP,sigma)) ->
                                          case infer' w n_v of
                                            OK (n_w,(c3,exprQ,tau)) ->
                                              case mgu ([(sigma,tau),(rho,TBool)] 
                                                          ++ map (\x -> (evalC c1 x,evalC c2 x))(filter (\y -> elem y (domainC c2)) (domainC c1))
                                                          ++ map (\x -> (evalC c1 x,evalC c3 x))(filter (\y -> elem y (domainC c3)) (domainC c1))
                                                          ++ map (\x -> (evalC c2 x,evalC c3 x))(filter (\y -> elem y (domainC c3)) (domainC c2))

                                                              ) of
                                                UOK subst -> OK (n_w,
                                                                        (
                                                                        joinC [subst <.> c1,subst <.> c2,subst <.> c3],
                                                                        subst <.> IfExp exprM exprP exprQ,
                                                                        subst <.> sigma
                                                                        )
                                                                    )
                                                UError u1 u2 -> uError u1 u2
                                            res@(Error _) -> res
                                        res@(Error _) -> res
                                    res@(Error _) -> res

-- EXTENSIÓN

infer' (EmptyListExp _)       n = OK (n+1,(emptyContext,EmptyListExp (TVar n),TList  (TVar n)))
infer' (ConsExp u v)          n = case infer' u n of 
                                    OK(n_u,(c1,exprM,rho)) ->
                                      case infer' v n_u of
                                        OK (n_v,(c2,exprN,tau)) ->
                                          case mgu ( [(tau,TList rho)] ++ map (\x -> (evalC c1 x,evalC c2 x))(filter (\y -> elem y (domainC c2)) (domainC c1))) of
                                            UOK subst -> OK (n_v,
                                                    (
                                                     joinC [subst <.> c1,subst <.> c2],
                                                     subst <.> ConsExp exprM exprN,
                                                     subst <.> TList rho
                                                    )
                                                )
                                            UError u1 u2 -> uError u1 u2
                                        res@(Error _) -> res
                                    res@(Error _) -> res
infer' (ZipWithExp u v x y w) n = case infer' u n of 
                                    OK(n_u,(c1,exprM,sigma)) ->
                                      case infer' v n_u of
                                        OK (n_v,(c2,exprN,rho)) ->
                                          case infer' w n_v of
                                            OK (n_w,(c3,exprO,tau)) ->
                                              let c3' = removeC (removeC c3 x) y 
                                                  tx = if elem x (domainC c3) then evalC c3 x else TVar n_w
                                                  ty = if elem y (domainC c3) then evalC c3 y else TVar (n_w+1)                                                  
 
                                                  in case mgu ([(TList tx,sigma),(TList ty,rho)] 
                                                              ++ map (\x -> (evalC c1 x,evalC c2 x))(filter (\y -> elem y (domainC c2)) (domainC c1))
                                                              ++ map (\x -> (evalC c1 x,evalC c3' x))(filter (\y -> elem y (domainC c3')) (domainC c1))
                                                              ++ map (\x -> (evalC c2 x,evalC c3' x))(filter (\y -> elem y (domainC c3')) (domainC c2))

                                                                  ) of
                                                      UOK subst -> OK (n_w+2,
                                                                              (
                                                                              joinC [subst <.> c1,subst <.> c2,subst <.> c3'],
                                                                              subst <.> ZipWithExp exprM exprN x y exprO,
                                                                              subst <.> TList tau
                                                                              )
                                                                          )
                                                      UError u1 u2 -> uError u1 u2
                                            res@(Error _) -> res
                                        res@(Error _) -> res
                                    res@(Error _) -> res

--------------------------------
-- YAPA: Error de unificacion --
--------------------------------
uError :: Type -> Type -> Result (Int, a)
uError t1 t2 = Error $ "Cannot unify " ++ show t1 ++ " and " ++ show t2
