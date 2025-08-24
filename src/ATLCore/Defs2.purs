module ATLCore.Defs2 where

import Prelude

import Data.Either (Either(..), either)
import Data.HashMap (HashMap)
import Data.HashMap as HMap
import Data.Hashable (class Hashable, hash)
import Data.List (List(..), all, (:))
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Obj

--------------------------------------------------------------------------------

-- | The assignment context.
-- | 
-- | Ctx.terms:
-- |   - bound variable substitution.
-- |   - data constructor.
-- | Ctx.types: 
-- |   - bound variable substitution.
-- |   - type constructor.
-- | Ctx.dcons:
-- |   - data constructors for each type constructor.
-- maybe classes and instances later on?
newtype Ctx = Ctx 
  { terms :: Object CTm
  , types :: Object CTy
  , dcons :: Object (Object Ty) }

-- | (term namespace) variable definition.
data CTm
  = CTmSubst Tm
  | CTmDCons Ty

-- | (type namespace) variable definition.
data CTy
  = CTySubst Ty
  | CTyTCons Kind

--------------------------------------------------------------------------------

data Tm
  = TmVar String
  | TmAbs String Ty Tm
  | TmApp Tm Tm
  | TmTyAbs String Kind Tm
  | TmTyApp Tm Ty
  | TmFold Ty Tm
  | TmUnfold Ty Tm
  | TmCons String (List (Either Tm Ty))
  | TmCase Tm (HashMap Pat Tm)
  | TmCls String Ty Tm Ctx
  | TmTyCls String Kind Tm Ctx

data Pat
  = PatVar String
  | PatCons String (List (Either Pat Ty))

data Ty
  = TyVar String
  | TyArr Ty Ty
  | TyRec String Ty
  | TyForall String Kind Ty
  | TyAbs String Kind Ty
  | TyApp Ty Ty
  | TyCls String Kind Ty Ctx
  | TyCons String (List Ty)

data Kind
  = KindTy
  | KindArr Kind Kind

--------------------------------------------------------------------------------

tmVal :: Tm -> Boolean
tmVal = case _ of
  TmCls _ _ _ _ -> true
  TmTyCls _ _ _ _ -> true
  TmFold _ v -> tmVal v
  TmCons _ vs -> all (either tmVal (const true)) vs
  _ -> false

tmWrapApp :: List Tm -> Tm -> Tm
tmWrapApp Nil t = t
tmWrapApp (x:xs) t = TmApp (tmWrapApp xs t) x

--------------------------------------------------------------------------------

-- | Base assignment context.
baseCtx :: Ctx
baseCtx = Ctx
  { terms : Obj.fromHomogeneous 
    { "Unit" : CTmDCons $ TyCons "Unit" Nil
    , "Zero" : CTmDCons $ TyCons "Nat" Nil
    , "Succ" : CTmDCons $ TyArr (TyCons "Nat" Nil) (TyCons "Nat" Nil) }
  , types : Obj.fromHomogeneous
    { "Unit" : CTyTCons KindTy
    , "Void" : CTyTCons KindTy 
    , "Nat" : CTyTCons KindTy }
  , dcons : Obj.fromHomogeneous
    { "Unit" : Obj.fromHomogeneous
      { "Unit" : TyCons "Unit" Nil }
    , "Void" : Obj.empty
    , "Nat" : Obj.fromHomogeneous
      { "Zero" : TyCons "Nat" Nil
      , "Succ" : TyArr (TyCons "Nat" Nil) (TyCons "Nat" Nil) }
    }
  }

--------------------------------------------------------------------------------

ex1 :: Tm
ex1 = 
  TmTyAbs "X" KindTy $
    TmTyAbs "Y" KindTy $
      TmTyAbs "Z" KindTy $
        TmAbs "x" (TyArr (TyVar "Y") (TyArr (TyVar "Z") (TyVar "X"))) $
          TmAbs "y" (TyVar "Y") $
            TmAbs "z" (TyVar "Z") $ TmApp 
              (TmApp
                (TmVar "x")
                (TmVar "y")
              )
              (TmVar "z")

ex2 :: Tm
ex2 =
  TmTyAbs "F" (KindArr (KindArr KindTy KindTy) (KindArr KindTy KindTy)) $
    TmTyAbs "G" (KindArr KindTy KindTy) $
      TmTyAbs "A" KindTy $
          TmAbs "x" (TyApp (TyApp (TyVar "F") (TyVar "G")) (TyVar "A")) $
            TmApp 
              (TmVar "x")
              (TmApp
                (TmVar "y")
                (TmVar "z")
              )

ex3 :: Tm
ex3 =
  TmCase (TmCons "Just" ((Right $ TyCons "Unit" Nil):(Left $ TmCons "Unit" Nil):Nil)) $ HMap.fromArray 
    [Tuple (PatCons "Just" (Left (PatVar "x"):Nil)) 
      (TmCase (TmVar "x") $ HMap.fromArray
      [Tuple (PatCons "Unit" Nil) (TmVar "x")])
    ,Tuple (PatVar "z") (TmVar "x")]

ex4 :: Tm
ex4 = TmCons "Just" (
    (Right $ TyCons "Maybe" ((TyCons "Unit" Nil):Nil))
    :(Left $ TmCons "Just" ((Right $ TyCons "Unit" Nil):(Left $ TmCons "Unit" Nil):Nil))
    :Nil)

--------------------------------------------------------------------------------

-- syntactic equality.
derive instance eqCtx :: Eq Ctx
derive instance eqCTm :: Eq CTm
derive instance eqCTy :: Eq CTy
derive instance eqTm :: Eq Tm
derive instance eqPat :: Eq Pat
derive instance eqTy :: Eq Ty
derive instance eqKind :: Eq Kind

--------------------------------------------------------------------------------

instance showCtx :: Show Ctx where
  show (Ctx ctx) = "Ctx "<>show ctx

instance showCTm :: Show CTm where
  show (CTmSubst a) = "CTmSubst "<>show a
  show (CTmDCons a) = "CTmDCons "<>show a

instance showCTy :: Show CTy where
  show (CTySubst a) = "CTySubst "<>show a
  show (CTyTCons a) = "CTyTCons "<>show a

instance showTm :: Show Tm where
  show (TmVar x) = "TmVar "<>show x
  show (TmAbs x ty t1) = "TmAbs "<>show x<>" "<>show ty<>" "<>show t1
  show (TmApp t1 t2) = "TmApp "<>show t1<>" "<>show t2
  show (TmTyAbs x k t1) = "TmTyAbs "<>show x<>" "<>show k<>" "<>show t1
  show (TmTyApp t1 t2) = "TmTyApp "<>show t1<>" "<>show t2
  show (TmFold ty t1) = "TmFold "<>show ty<>" "<>show t1
  show (TmUnfold ty t1) = "TmUnfold "<>show ty<>" "<>show t1
  show (TmCons c ps) = "TmCons "<>show c<>" "<>show ps
  show (TmCase t1 cs) = "TmCase "<>show t1<>" "<>show cs
  show (TmCls x ty t1 cls) = "TmCls "<>show x<>" "<>show ty<>" "<>show t1<>" "<>show cls
  show (TmTyCls x k t1 cls) = "TmTyCls "<>show x<>" "<>show k<>" "<>show t1<>" "<>show cls

instance showPat :: Show Pat where
  show (PatVar x) = "PatVar "<>show x
  show (PatCons c ps) = "PatCons "<>show c<>" "<>show ps

instance showTy :: Show Ty where
  show (TyVar x) = "TyVar "<>show x
  show (TyArr t1 t2) = "TyArr "<>show t1<>" "<>show t2
  show (TyRec x t1) = "TyRec "<>show x<>" "<>show t1
  show (TyForall x k t1) = "TyForall "<>show x<>" "<>show k<>" "<>show t1
  show (TyAbs x k t1) = "TyAbs "<>show x<>" "<>show k<>" "<>show t1
  show (TyApp t1 t2) = "TyApp "<>show t1<>" "<>show t2
  show (TyCls x k t1 cls) = "TyCls "<>show x<>" "<>show k<>" "<>show t1<>" "<>show cls
  show (TyCons c ps) = "TyCons "<>show c<>" "<>show ps

instance showKind :: Show Kind where
  show KindTy = "*"
  show (KindArr k1 k2) = show k1<>" => "<>show k2

--------------------------------------------------------------------------------

instance tmHashable :: Hashable Tm where
  hash = show >>> hash

instance patHashable :: Hashable Pat where
  hash = show >>> hash

instance tyHashable :: Hashable Ty where
  hash = show >>> hash

instance kindHashable :: Hashable Kind where
  hash = show >>> hash

--------------------------------------------------------------------------------

-- dCons :: String -> Ctx -> Object Ty
-- dCons x (Ctx ctx) = Obj.fold toty Obj.empty ctx.terms
--   where
--     toty :: Object Ty -> String -> CTm -> Object Ty
--     toty ys s (CTmDCons t) | t `returns` x = Obj.insert s t ys
--     toty ys _ _ = ys

--     returns :: Ty -> String -> Boolean
--     returns (TyCons c _) s = s==c
--     returns _ _ = false -- missing cases! need to basically interpret
