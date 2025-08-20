module ATLCore.Interpreter where

import ATLCore.Defs
import Prelude
import Data.Foldable (findMap, foldM, foldl)
import Data.HashMap as HashMap
import Data.List (List(..), zip, (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (catchException, throw)
import Foreign.Object (insert)
import Foreign.Object as Object

--------------------------------------------------------------------------------

run :: Module -> List String -> Effect Unit
run mod _ = eval baseCtx mod "main" (TmCons "Unit" Nil:Nil) -- todo

eval :: Ctx -> Module -> String -> List Tm -> Effect Unit
eval ctx mod name args = do
  log $ pr mod
  ctx' <- evalM ctx mod
  let tar = tmWrapApp args (TmVar name)
  log $ "\n"<>show ctx'
  log "[[Target Term]]"
  log $ pr tar
  log "\n\x1B[1;32m[[Running]]\x1B[0m"
  catchException (log <<< show) $ do
    tar' <- evalTm ctx' tar
    log $ pr tar'

evalM :: Ctx -> Module -> Effect Ctx
evalM ctx mod = foldM evalD ctx mod

evalD :: Ctx -> Decl -> Effect Ctx
evalD (Ctx ctx) decl = case decl of
  DeclTerm x _ b ->
    (\b' -> Ctx $ ctx { terms = insert x (CTm b') ctx.terms }) 
    <$> evalTm (Ctx ctx) b
  DeclType x _ t ->
    pure $ Ctx $ ctx { types = insert x (CTy t) ctx.types }
  DeclGADT x (GADT ps k cs) ->
    -- let tctx = foldl (\acc (Bind px pk) -> ) ctx ps in
    let terms' = foldl (\acc (Bind cx ct) -> insert cx (CTmCons ct) acc) ctx.terms cs in
    let types' = insert x (CTyGADT (GADT ps k cs)) ctx.types in
    pure $ Ctx $ ctx { terms = terms', types = types' }
  DeclClass x c ->
    pure $ Ctx $ ctx { classes = insert x c ctx.classes }
  DeclInstance c ps i ->
    pure $ Ctx $ ctx { instances = HashMap.insert (Tuple c ps) i ctx.instances}

evalTm :: Ctx -> Tm -> Effect Tm
evalTm (Ctx ctx) tm = case tm of
  TmVar x -> case Object.lookup x ctx.terms of
    Just (CTm t1) -> pure t1
    Just (CTmCons t) -> throw $ "expected term."
    Nothing -> throw $ "undefined variable: "<>x<>"."
  TmAbs x t t1 -> pure $ TmClosure x t t1 (Ctx ctx)
  TmApp t1 t2 -> do 
    t1' <- evalTm (Ctx ctx) t1 
    case t1' of
      TmClosure x _ t11 (Ctx cls) -> do
        t2' <- evalTm (Ctx ctx) t2
        let ctx' = cls { terms = insert x (CTm t2') ctx.terms }
        evalTm (Ctx ctx') t11
      _ -> throw $ "expected closure: "<>pr t1'
  TmTyAbs x k t -> pure $ TmTyClosure x k t (Ctx ctx)
  TmTyApp t1 t2 -> do 
    t1' <- evalTm (Ctx ctx) t1 
    case t1' of
      TmClosure x _ t11 (Ctx cls) -> do
        let cls' = cls { types = insert x (CTy t2) ctx.types }
        evalTm (Ctx cls') t11
      _ -> throw $ "expected closure: "<>pr t1'
  TmFold t b -> throw "TODO!" -- todo
  TmUnfold t b -> throw "TODO!" -- todo
  TmCons s ps -> TmCons s <$> 
    foldM (\acc x -> flip Cons acc <$> evalTmTy (Ctx ctx) x) Nil ps
  TmCase t1 cs -> do
    t1' <- evalTm (Ctx ctx) t1
    case t1' of
      TmCons x ps -> do
        maybe (throw $ "no case for: "<>pr t1') identity $
          findMap (\c -> case c of
            Case cx cps t2 -> 
              if cx == x 
                then flip evalTm t2 <<< Ctx <$>
                  foldM (\acc (Tuple p cp) -> case p of
                      Tm (TmVar px) -> case cp of
                        Tm cpt -> 
                          Just $ acc { terms = insert px (CTm cpt) acc.terms }
                        _ -> Nothing
                      Ty (TyVar px) -> case cp of
                        Ty cpt -> 
                          Just $ acc { types = insert px (CTy cpt) acc.types }
                        _ -> Nothing
                      _ -> if p == cp then Just acc else Nothing
                    ) ctx (zip ps cps)
                else Nothing
            Wild t2 -> Just $ evalTm (Ctx ctx) t2
          ) cs
      _ -> throw $ "expected data construct but got: "<>pr t1'
  TmClosure x t t1 cls -> pure $ TmClosure x t t1 cls
  TmTyClosure x k t cls -> pure $ TmTyClosure x k t cls
  -- todo: handle classes.

evalTmTy :: Ctx -> TmTy -> Effect TmTy
evalTmTy ctx (Tm tm) = Tm <$> evalTm ctx tm
evalTmTy _ (Ty ty) = pure $ Ty ty


--------------------------------------------------------------------------------
