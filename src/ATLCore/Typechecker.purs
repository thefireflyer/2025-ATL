module ATLCore.Typechecker where

import ATLCore.Defs2
import Prelude

import Data.Fallible (Fallible(..))
import Data.List (List(..))
import Foreign.Object (Object)
import Foreign.Object as Obj

--------------------------------------------------------------------------------

-- | The typing constraint context.
type TyCtx = 
  { terms :: Object TTm
  , types :: Object TTy
  , dcons :: Object (Object Ty) }

-- | (term namespace) variable constraint.
data TTm
  = TTmSubst Ty
  | TTmDCons Ty

-- | (type namespace) variable constraint.
data TTy
  = TTySubst Ty
  | TTyTCons Kind

--------------------------------------------------------------------------------

-- | Type equivalence
equiv :: TyCtx -> Ty -> Ty -> Boolean
equiv tctx t1 t2 = false -- todo

--------------------------------------------------------------------------------

-- | Check that the assignment context is consistent.
-- | If it is, return it's typing constraint context.
tyCtx :: Ctx -> Fallible String TyCtx
tyCtx _ = Failed Nil

--------------------------------------------------------------------------------

tyOf :: TyCtx -> Ctx -> Tm -> Fallible String Ty
tyOf _ _ _ = Failed Nil

--------------------------------------------------------------------------------
