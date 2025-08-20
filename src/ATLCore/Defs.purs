module ATLCore.Defs where

import Prelude

import Data.Foldable (class Foldable, findMap, fold, foldMap, intercalate, surroundMap)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Hashable (class Hashable, hash)
import Data.List (List(..), all, foldMap, fromFoldable, (:))
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object

--------------------------------------------------------------------------------

type Module = List Decl
data Decl
  = DeclTerm String Ty Tm
  -- ^ top-level term definition.
  | DeclType String Kind Ty
  -- ^ top-level type definition.
  | DeclGADT String GADT
  -- ^ GADT definition.
  | DeclClass String Class
  -- ^ Class definition.
  | DeclInstance String (List Ty) Instance
  -- ^ Instance definition.

data GADT = GADT (Binds Tm Kind) Kind (Binds Decl Ty)
data Class = Class (Binds Tm Kind) (Binds Decl Kind) (Binds Decl Ty)
data Instance = Instance (Assigns Ty) (Assigns Tm)

type Binds :: forall k. k -> Type -> Type
type Binds ctx a = List (Bind ctx a)
data Bind :: forall k. k -> Type -> Type
data Bind ctx a = Bind String a

type Assigns a = List (Assign a)
data Assign a = Assign String a

--------------------------------------------------------------------------------

newtype Ctx = Ctx
  { terms :: Object CTm -- term namespace
  , types :: Object CTy -- type namespace
  , classes :: Object Class -- class namespace
  , instances :: HashMap (Tuple String (List Ty)) Instance }

data CTm
  = CTm Tm -- bound to term
  | CTmCons Ty -- bound to type constructor

data CTy
  = CTy Ty -- bound to type
  | CTyGADT GADT -- bound to GADT

--------------------------------------------------------------------------------

data Tm
  = TmVar String
  | TmAbs String Ty Tm
  | TmApp Tm Tm
  | TmTyAbs String Kind Tm
  | TmTyApp Tm Ty
  | TmFold Ty Tm
  | TmUnfold Ty Tm
  | TmCons String (List TmTy)
  | TmCase Tm (NonEmptyList Case)
  | TmClosure String Ty Tm Ctx
  | TmTyClosure String Kind Tm Ctx

data Ty
  = TyVar String
  | TyArr Ty Ty
  | TyRec String Ty
  | TyForall String Kind Ty
  | TyAbs String Kind Ty
  | TyApp Ty Ty
  | TyCons String (List Ty)
  | TyCnst String (NonEmptyList Ty) Ty

data Kind
  = KindTy
  | KindArr Kind Kind

data TmTy = Tm Tm | Ty Ty

data Case 
  = Case String (List TmTy) Tm
  | Wild Tm

--------------------------------------------------------------------------------

tmVal :: Tm -> Boolean
tmVal tm = case tm of
  TmClosure _ _ _ _ -> true
  TmTyClosure _ _ _ _ -> true
  TmFold _ v -> tmVal v
  TmCons _ vs -> all tmtyVal vs
  _ -> false
  where
    tmtyVal (Tm x) = tmVal x
    tmtyVal _ = true

tmWrapApp :: List Tm -> Tm -> Tm
tmWrapApp Nil t = t
tmWrapApp (x:xs) t = TmApp (tmWrapApp xs t) x

modLookupTerm :: String -> Module -> Maybe (Tuple Ty Tm)
modLookupTerm x = findMap $ \d -> case d of
  DeclTerm s t b | s == x -> Just $ Tuple t b
  _ -> Nothing

modLookupType :: String -> Module -> Maybe (Tuple Kind Ty)
modLookupType x = findMap $ \d -> case d of
  DeclType s k b | s == x -> Just $ Tuple k b
  _ -> Nothing

modLookupGADT :: String -> Module -> Maybe GADT
modLookupGADT x = findMap $ \d -> case d of
  DeclGADT s g | s == x -> Just $ g
  _ -> Nothing

--------------------------------------------------------------------------------

class Pr a where
  pr :: a -> String

prSepBy ∷ ∀ a f. Pr a ⇒ Foldable f ⇒ Functor f ⇒ String → f a → String
prSepBy s = intercalate s <<< map pr

prPrefix :: ∀ a f. Pr a => Foldable f => String -> f a -> String
prPrefix s = foldMap ((<>) s <<< pr)

prPostfix :: ∀ a f. Pr a => Foldable f => String -> f a -> String
prPostfix s = foldMap (flip (<>) s <<< pr)

prSurround :: ∀ f a. Pr a => Foldable f => String -> f a -> String
prSurround s = surroundMap s pr

prBetween :: ∀ a. Pr a => String -> String -> a -> String
prBetween pre post = flip (<>) post <<< (<>) pre <<< pr


--------------------------------------------------------------------------------

instance prModule :: Pr Module where
  pr ds = prSepBy "\n" ds

instance prDecl :: Pr Decl where
  pr (DeclTerm x t b) = "let "<>x<>" : "<>pr t<>" := "<>pr b
  pr (DeclType x k b) = "let "<>x<>" : "<>pr k<>" := "<>pr b
  pr (DeclGADT x (GADT ps k cs)) = 
    "data "<>x<>prSurround " " ps<>": "<>pr k<>" where\n"
      <>foldMap (prBetween "  " "\n") cs
  pr (DeclClass x (Class ps ks ts)) =
    "class "<>x<>prPrefix " " ps<>" where\n"
      <>foldMap (prBetween "  " "\n") ks
      <>foldMap (prBetween "  " "\n") ts
  pr (DeclInstance c ps (Instance ks ts)) =
    "instance "<>c<>prPrefix " " ps<>" where\n"
      <>foldMap (prBetween "  " "\n") ks
      <>foldMap (prBetween "  " "\n") ts

instance prBindTm :: Pr a => Pr (Bind Tm a) where
  pr (Bind x a) = "("<>x<>":"<>pr a<>")"

instance prBindDecl :: Pr a => Pr (Bind Decl a) where
  pr (Bind x a) = x<>":"<>pr a

instance prAssign :: Pr a => Pr (Assign a) where
  pr (Assign x a) = x<>" := "<>pr a

instance prTm :: Pr Tm where
  pr tm = case tm of
    TmVar x -> x
    TmAbs x ty t1 -> "(λ"<>x<>":"<>pr ty<>". "<>pr t1<>")"
    TmApp t1 t2 -> "("<>pr t1<>" "<>pr t2<>")"
    TmTyAbs tx k t1 -> "(λ"<>tx<>":"<>pr k<>". "<>pr t1<>")"
    TmTyApp t1 ty -> "("<>pr t1<>" "<>pr ty<>")"
    TmFold ty t1 -> "(fold "<>pr ty<>" "<>pr t1<>")"
    TmUnfold ty t1 -> "(unfold "<>pr ty<>" "<>pr t1<>")"
    TmCons c ps -> "(#"<>c<>prPrefix " " ps<>")"
    TmCase t1 cs -> 
      "(case "<>pr t1<>" of\n"
      <>foldMap (prBetween "  " "\n") cs
    TmClosure x ty t1 _ -> "(%λ"<>x<>":"<>pr ty<>". "<>pr t1<>")"
    TmTyClosure tx k t1 _ -> "(%λ"<>tx<>":"<>pr k<>". "<>pr t1<>")"

instance prTy :: Pr Ty where
  pr ty = case ty of
    TyVar x -> x
    TyArr ty1 ty2 -> "("<>pr ty1<>" -> "<>pr ty2<>")"
    TyRec x ty1 -> "(μ"<>x<>". "<>pr ty1<>")"
    TyForall x k ty1 -> "(∀"<>x<>":"<>pr k<>". "<>pr ty1<>")"
    TyAbs x k ty1 -> "(λ"<>x<>":"<>pr k<>". "<>pr ty1<>")"
    TyApp ty1 ty2 -> "("<>pr ty1<>" "<>pr ty2<>")"
    TyCons x ps -> "(#"<>x<>prPrefix " " ps<>")"
    TyCnst c tys ty2 -> 
      "("<>c<>prSurround " " tys<>"=> "<>pr ty2<>")"

instance prKind :: Pr Kind where
  pr KindTy = "*"
  pr (KindArr k1 k2) = "("<>pr k1<>" => "<>pr k2<>")"

instance prTmTy :: Pr TmTy where
  pr (Tm t) = pr t
  pr (Ty t) = pr t

instance prCase :: Pr Case where
  pr (Case c ps t2) = "| #"<>c<>prSurround " " ps<>"=> "<>pr t2
  pr (Wild t2) = "| _ => "<>pr t2

--------------------------------------------------------------------------------

derive instance eqTmTy :: Eq TmTy
derive instance eqCase :: Eq Case
derive instance eqClass :: Eq Class
derive instance eqInstance :: Eq Instance
derive instance eqBind :: Eq a => Eq (Bind ctx a)
derive instance eqAssign :: Eq a => Eq (Assign a)
derive instance eqCTm :: Eq CTm
derive instance eqCTy :: Eq CTy
derive instance eqGADT :: Eq GADT
derive instance eqCtx :: Eq Ctx
derive instance eqTm :: Eq Tm
derive instance eqTy :: Eq Ty
derive instance eqKind :: Eq Kind

instance showTy :: Show Ty where
  show = pr

instance showCTm :: Show CTm where
  show (CTm tm) = pr tm
  show (CTmCons ty) = ":"<>pr ty

instance showCTy :: Show CTy where
  show (CTy ty) = pr ty
  show (CTyGADT _) = "\\\\\\"

instance showClass :: Show Class where
  show (Class ps k ts) = 
      prPrefix " " ps<>" where\n"
      <>foldMap (prBetween "  " "\n") k
      <>foldMap (prBetween "  " "\n") ts

instance showInstance :: Show Instance where
  show (Instance ks ts) = 
      foldMap (prBetween "  " "\n") ks
      <>foldMap (prBetween "  " "\n") ts

instance showCtx :: Show Ctx where
  show (Ctx ctx) = 
          "[[Context.terms]]\n"<>Object.foldMap sCtm ctx.terms
    <>"\n[[Context.types]]\n"<>Object.foldMap sCty ctx.types
    <>"\n[[Context.classes]]\n"<>Object.foldMap sCls ctx.classes
    <>"\n[[Context.instances]]\n"<>fold (HashMap.toArrayBy sIst ctx.instances)
    where
      sCtm x (CTm b) = x<>" = "<>pr b<>"\n"
      sCtm x (CTmCons b) = x<>":"<>pr b<>"\n"
      sCty x (CTy b) = x<>" = "<>pr b<>"\n"
      sCty x (CTyGADT b) = pr $ DeclGADT x b
      sCls x b = pr $ DeclClass x b
      sIst (Tuple c t) b = pr $ DeclInstance c t b

instance hashableTy :: Hashable Ty where
  hash = hash <<< show

--------------------------------------------------------------------------------

baseCtx :: Ctx
baseCtx = Ctx
  { terms : Object.fromHomogeneous 
    { "Unit" : CTmCons $ TyCons "Unit" Nil }
  , types : Object.fromHomogeneous
    { "Unit" : CTyGADT $ 
        GADT Nil KindTy $ pure $ Bind "Unit" (TyCons "Unit" Nil)
    , "Void" : CTyGADT $ GADT Nil KindTy Nil }
  , classes : Object.empty
  , instances : HashMap.empty
  }

--------------------------------------------------------------------------------

ex1 :: Module
ex1 = fromFoldable $ [maybe, main]
  where
    maybe = DeclGADT "Maybe" $ GADT (pure $ Bind "a" KindTy) (KindArr KindTy KindTy) $
      fromFoldable
      [ Bind "Just" (TyArr (TyVar "a") (TyCons "Maybe" (pure $ TyVar "a"))),
        Bind "None" (TyCons "Maybe" (pure $ TyVar "a"))]
    main = DeclTerm "main"
      (TyArr (TyCons "Unit" Nil) (TyCons "Maybe" (pure $ TyCons "String" Nil)))
      $ TmAbs "args" (TyCons "Unit" Nil) $
          TmCons "None" Nil

ex2 :: Module
ex2 = fromFoldable [maybe, functor, maybeFunctor, main]
  where
    maybe = DeclGADT "Maybe" $ GADT (pure $ Bind "a" KindTy) (KindArr KindTy KindTy) $
      fromFoldable
      [ Bind "Just" (TyArr (TyVar "a") (TyCons "Maybe" (pure $ TyVar "a"))),
        Bind "None" (TyCons "Maybe" (pure $ TyVar "a"))]
    functor = DeclClass "Functor" $ Class (pure $ Bind "f" (KindArr KindTy KindTy))
      Nil
      (fromFoldable [
        Bind "map" $ TyForall "a" KindTy $ TyForall "b" KindTy $ 
          TyArr 
            (TyArr (TyVar "a") (TyVar "b")) 
            $ TyArr 
              (TyApp (TyVar "f") (TyVar "a"))
              (TyApp (TyVar "f") (TyVar "b"))
      ])
    maybeFunctor = DeclInstance "Functor" (pure $ TyVar "Maybe")
      $ Instance
      Nil (fromFoldable $ [
        Assign "map" $ TmTyAbs "a" KindTy $ TmTyAbs "b" KindTy $ 
          TmAbs "f" (TyArr (TyVar "a") (TyVar "b")) $
            TmAbs "x" (TyCons "Maybe" (pure $ TyVar "a")) $
              TmCase (TmVar "x") (NonEmptyList $ NonEmpty 
                (Case "Just" (pure $ Tm $ TmVar ("a")) 
                  $ TmCons "Just" (pure $ Tm $ TmApp (TmVar "f") (TmVar "a")))
                ((Case "None" Nil $ TmCons "None" Nil):Nil))
      ])
    main = DeclTerm "main"
      (TyCnst "Functor" (pure $ TyVar "Maybe")
      $ TyArr 
          (TyCons "Maybe" (pure $ TyCons "String" Nil)) 
          (TyCons "Maybe" (pure $ TyCons "Unit" Nil)))
      $ TmAbs "args" (TyCons "Maybe" (pure $ TyCons "String" Nil))  $
          TmApp 
            (TmApp 
              (TmVar "map") 
              (TmAbs "x" (TyCons "String" Nil) (TmCons "Unit" Nil)))
            (TmVar "args")

--------------------------------------------------------------------------------