{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts #-}

module Data.ExclusiveMonoid
where
  
import Control.Monad
import MonadLib
import Data.Maybe

class Monad m => ExclusiveMonoid a m | a -> m where
    invalid :: a
    isValidM :: a -> m Bool
    orElse :: a -> a -> a
    firstOf :: [a] -> a
    firstOf = foldr orElse invalid

instance ExclusiveMonoid Bool Id where
    invalid = False
    isValidM = return . (== True)
    orElse = (||)

instance ExclusiveMonoid (Maybe a) Id where
    invalid = Nothing
    isValidM = return . isJust
    orElse = mplus
    
isValid :: ExclusiveMonoid a Id => a -> Bool
isValid = runId . isValidM

newtype WrappedMonad m a = WrapMonad { unwrapMonad :: m a }

instance (Monad m, ExclusiveMonoid a Id) => ExclusiveMonoid (WrappedMonad m a) m where
    invalid = WrapMonad $ return invalid
    isValidM = liftM isValid . unwrapMonad
    orElse a b = WrapMonad $ do
      x <- unwrapMonad a
      case isValid x of
        False -> unwrapMonad b
        True  -> return x

monadInvalid :: (Monad m, ExclusiveMonoid (WrappedMonad m a) m) => m a
monadInvalid = unwrapMonad invalid

monadIsValid :: (Monad m, ExclusiveMonoid (WrappedMonad m a) m) => m a -> m Bool
monadIsValid = isValidM . WrapMonad

monadOrElse :: (Monad m, ExclusiveMonoid (WrappedMonad m a) m) => m a -> m a -> m a
monadOrElse a b = unwrapMonad $ orElse (WrapMonad a) (WrapMonad b)

monadFirstOf :: (Monad m, ExclusiveMonoid (WrappedMonad m a) m) => [m a] -> m a
monadFirstOf = unwrapMonad . firstOf . map WrapMonad
