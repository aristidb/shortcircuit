module Control.Shortcircuit
(
  ExclusiveZero(..)
, ExclusiveMonoid(..)
, firstOf
, EitherSum(..)
, monadZero
, monadIsValid
, monadOrElse
, monadFirstOf
)
where
  
import Control.Monad
import Data.Maybe
import Data.Monoid

class ExclusiveZero a where
    zero :: a

class ExclusiveMonoid a where
    isValid :: a -> Bool
    orElse :: a -> a -> a

firstOf :: (ExclusiveZero a, ExclusiveMonoid a) => [a] -> a
firstOf = foldr orElse zero

instance ExclusiveZero Bool where
    zero = False

instance ExclusiveMonoid Bool where
    isValid = (== True)
    orElse = (||)

instance ExclusiveZero (Maybe a) where
    zero = Nothing

instance ExclusiveMonoid (Maybe a) where
    isValid = isJust
    orElse = mplus

instance ExclusiveMonoid (Either a b) where
    isValid (Right _) = True
    isValid (Left _)  = False
    orElse a@(Left _)    (Left _)  = a
    orElse   (Left _)  b@(Right _) = b
    orElse a@(Right _)   _         = a

newtype EitherSum a b = EitherSum { unEitherSum :: Either a b }

instance Monoid a => ExclusiveZero (EitherSum a b) where
    zero = EitherSum $ Left mempty

instance Monoid a => ExclusiveMonoid (EitherSum a b) where
    isValid (EitherSum x) = isValid x
    orElse (EitherSum x) (EitherSum y) = EitherSum $ orElse' x y
        where
          orElse'   (Left a)    (Left b)  = Left (a `mappend` b)
          orElse'   (Left _)  b@(Right _) = b
          orElse' a@(Right _)   _         = a

monadZero :: (Monad m, ExclusiveZero a) => m a
monadZero = return zero

monadIsValid :: (Monad m, ExclusiveMonoid a) => m a -> m Bool
monadIsValid = liftM isValid

monadOrElse :: (Monad m, ExclusiveMonoid a) => m a -> m a -> m a
monadOrElse a b = a >>= \x -> if isValid x then return x else b

monadFirstOf :: (Monad m
                , ExclusiveZero a
                , ExclusiveMonoid a) 
                => [m a] -> m a
monadFirstOf = foldr monadOrElse monadZero
