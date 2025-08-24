module ATLCore.Pretty
  ( pkd
  , ppt
  , ptm
  , pty
  )
  where

import ATLCore.Defs2
import Prelude

import Data.Either (either)
import Data.Foldable (foldMap)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.List (List(..), (:))

{-------------------------------------------------------------------------------
Tm0 = Tm1
    | "case " Tm0 " of " ("| " Pt0 " => " Tm1)*
Tm1 = Tm2
    | "λ" var ":" Ty2 "." Tm1
    | "λ" var ":" Kd1 "." Tm1
    | Tm1 " " Tm2
    | Tm1 " " Ty2
    | "fold " Ty2 " " Tm1
    | "unfold " Ty2 " " Tm1
Tm2 = "(" Tm0 ")"
    | var

Pt0 = Pt1
    | var (" " Pt1)*
Pt1 = "(" Pt0 ")"
    | var
    | Ty2

Ty0 = Ty1
    | "μ" var "." Ty0
    | "∀" var ":" Kd1 "." Ty0
    | "λ" var ":" Kd1 "." Ty0
    | Ty0 " " Ty1
Ty1 = Ty2
    | Ty2 "->" Ty1
Ty2 = "(" Ty0 ")"
    | var
  
Kd0 = Kd1
    | Kd1 "=>" Kd0
Kd1 = "(" Kd0 ")"
    | "*"

λx.λy.λz.x y z
case case x of | a => a of | a => (case a of | b => b) | b => b

-------------------------------------------------------------------------------}

ptm0 :: Tm -> String
ptm0 (TmCase t1 cs) = "case "<>ptm0 t1<>" of "<>foldMapWithIndex (\p t -> "| "<>ppt0 p<>" => "<>ptm1 t<>" ") cs
ptm0 tm = ptm1 tm

ptm1 :: Tm -> String
ptm1 (TmAbs x ty t1) = "λ"<>x<>":"<>pty1 ty<>". "<>ptm1 t1
ptm1 (TmTyAbs x k t1) = "λ"<>x<>":"<>pkd1 k<>". "<>ptm1 t1
ptm1 (TmCls x ty t1 _) = "%λ"<>x<>":"<>pty1 ty<>". "<>ptm1 t1
ptm1 (TmTyCls x k t1 _) = "%λ"<>x<>":"<>pkd1 k<>". "<>ptm1 t1
ptm1 (TmApp t1 t2) = ptm1 t1<>" "<>ptm2 t2
ptm1 (TmTyApp t1 ty) = ptm1 t1<>" "<>pty1 ty
ptm1 (TmFold ty t2) = "fold "<>pty1 ty<>" "<>ptm1 t2
ptm1 (TmUnfold ty t2) = "unfold "<>pty1 ty<>" "<>ptm1 t2
ptm1 (TmCons c (p:ps)) = c<>foldMap ((<>) " " <<< either ptm2 pty2) (p:ps)
ptm1 tm = ptm2 tm

ptm2 :: Tm -> String
ptm2 (TmVar x) = x
ptm2 (TmCons c Nil) = c
ptm2 tm = "("<>ptm0 tm<>")"

ppt0 :: Pat -> String
ppt0 (PatCons c (p:ps)) = c<>foldMap ((<>) " " <<< either ppt1 pty2) (p:ps)
ppt0 pt = ppt1 pt

ppt1 :: Pat -> String
ppt1 (PatVar x) = x
ppt1 (PatCons c Nil) = c
ppt1 pt = "("<>ppt0 pt<>")"

pty0 :: Ty -> String
pty0 (TyRec x t1) = "μ"<>x<>". "<>pty0 t1
pty0 (TyForall x k t1) = "∀"<>x<>":"<>pkd1 k<>". "<>pty0 t1
pty0 (TyAbs x k t1) = "λ"<>x<>":"<>pkd1 k<>". "<>pty0 t1
pty0 (TyApp t1 t2) = pty0 t1<>" "<>pty1 t2
pty0 (TyCls x k t1 _) = "%λ"<>x<>":"<>pkd1 k<>". "<>pty0 t1
pty0 (TyCons c (p:ps)) = c<>foldMap ((<>) " " <<< pty2) (p:ps)
pty0 ty = pty1 ty

pty1 :: Ty -> String
pty1 (TyArr t1 t2) = pty2 t1<>" -> "<>pty1 t2
pty1 ty = pty2 ty

pty2 :: Ty -> String
pty2 (TyVar x) = x
pty2 (TyCons c Nil) = c
pty2 ty = "("<>pty0 ty<>")"

pkd0 :: Kind -> String
pkd0 (KindArr k1 k2) = pkd1 k1<>"=>"<>pkd0 k2
pkd0 kd = pkd1 kd

pkd1 :: Kind -> String
pkd1 KindTy = "*"
pkd1 kd = "("<>pkd0 kd<>")"

--------------------------------------------------------------------------------

ptm :: Tm -> String
ptm = ptm0
ppt :: Pat -> String
ppt = ppt0
pty :: Ty -> String
pty = pty0
pkd :: Kind -> String
pkd = pkd0

--------------------------------------------------------------------------------
