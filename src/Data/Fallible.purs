module Data.Fallible (Fallible(..)) where

import Prelude
import Data.List (List)

--------------------------------------------------------------------------------

data Fallible e a = Okay a | Failed (List e)

instance functorFallible :: Functor (Fallible e) where
  map f (Okay a) = Okay (f a)
  map _ (Failed es) = Failed es

instance applyFallible :: Apply (Fallible e) where
  apply (Okay f) (Okay a) = Okay (f a)
  apply (Okay _) (Failed ys) = Failed ys
  apply (Failed xs) (Okay _) = Failed xs
  apply (Failed xs) (Failed ys) = Failed (xs<>ys)

instance applicativeFallible :: Applicative (Fallible e) where
  pure = Okay

instance semigroupFallible :: Semigroup (Fallible e a) where
  append (Okay _) (Okay y) = Okay y
  append (Okay _) (Failed ys) = Failed ys
  append (Failed xs) (Okay _) = Failed xs
  append (Failed xs) (Failed ys) = Failed (xs<>ys)

instance monoidFallible :: Monoid a => Monoid (Fallible e a) where
  mempty = Okay mempty

instance showFallible :: Show a => Show (Fallible String a) where
  show (Okay a) = "Okay "<>show a
  show (Failed es) = "Failed "<>show es

-- sequence, you're looking for sequence!!

-- raise :: forall f m a a1. Foldable f => Monad m => Monoid (m a1) => Applicative a => f (a a1) -> a (m a1)
-- raise = foldl 
--   (\ys x -> (flip append <$> ys) <*> (pure <$> x)) 
--   (pure mempty)

-- distribute :: forall f m e a. Foldable f => Monoid (m a) => Monad m => f (Fallible e a) -> Fallible e (m a)
-- distribute = raise

-- distList :: forall e a. List (Fallible e a) -> Fallible e (List a)
-- distList = distribute

--------------------------------------------------------------------------------
