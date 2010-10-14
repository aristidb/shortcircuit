{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts #-}

module Data.ExclusiveMonoid
where
  
import Control.Monad
import MonadLib
import Data.Maybe
import Data.Monoid

class ExclusiveZero a where
    zero :: a

class Monad m => ExclusiveMonoid a m | a -> m where
    isValidM :: a -> m Bool
    orElse :: a -> a -> a

isValid :: ExclusiveMonoid a Id => a -> Bool
isValid = runId . isValidM

firstOf :: (ExclusiveZero a, ExclusiveMonoid a m) => [a] -> a
firstOf = foldr orElse zero

instance ExclusiveZero Bool where
    zero = False

instance ExclusiveMonoid Bool Id where
    isValidM = return . (== True)
    orElse = (||)

instance ExclusiveZero (Maybe a) where
    zero = Nothing

instance ExclusiveMonoid (Maybe a) Id where
    isValidM = return . isJust
    orElse = mplus

instance ExclusiveMonoid (Either a b) Id where
    isValidM (Right _) = return True
    isValidM (Left _)  = return False
    orElse a@(Left _)    (Left _)  = a
    orElse   (Left _)  b@(Right _) = b
    orElse a@(Right _)   _         = a

newtype EitherSum a b = EitherSum { unEitherSum :: Either a b }

instance Monoid a => ExclusiveZero (EitherSum a b) where
    zero = EitherSum $ Left mempty

instance Monoid a => ExclusiveMonoid (EitherSum a b) Id where
    isValidM (EitherSum x) = isValidM x
    orElse (EitherSum x) (EitherSum y) = EitherSum $ orElse' x y
        where
          orElse'   (Left a)    (Left b)  = Left (a `mappend` b)
          orElse'   (Left _)  b@(Right _) = b
          orElse' a@(Right _)   _         = a

newtype WrappedMonad m a = WrapMonad { unwrapMonad :: m a }

instance (Monad m, ExclusiveZero a) => ExclusiveZero (WrappedMonad m a) where
    zero = WrapMonad $ return zero

instance (Monad m, ExclusiveMonoid a Id) => ExclusiveMonoid (WrappedMonad m a) m where
    isValidM = liftM isValid . unwrapMonad
    orElse a b = WrapMonad $ do
      x <- unwrapMonad a
      case isValid x of
        False -> unwrapMonad b
        True  -> return x

monadZero :: (Monad m, ExclusiveZero (WrappedMonad m a)) => m a
monadZero = unwrapMonad zero

monadIsValid :: (Monad m, ExclusiveMonoid (WrappedMonad m a) m) => m a -> m Bool
monadIsValid = isValidM . WrapMonad

monadOrElse :: (Monad m, ExclusiveMonoid (WrappedMonad m a) m) => m a -> m a -> m a
monadOrElse a b = unwrapMonad $ orElse (WrapMonad a) (WrapMonad b)

monadFirstOf :: (Monad m
                , ExclusiveZero (WrappedMonad m a)
                , ExclusiveMonoid (WrappedMonad m a) m) 
                => [m a] -> m a
monadFirstOf = unwrapMonad . firstOf . map WrapMonad
