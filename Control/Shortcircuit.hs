module Control.Shortcircuit
(
  HasFalse(..)
, HasTrue(..)
, Shortcircuit(..)
, isFalse
, if'
, unless'
, (??)
, (||)
, (&&)
, firstTrueOf
, lastFalseOf
, orM
, andM
, firstTrueOfM
, lastFalseOfM
)
where
  
import Control.Monad
import Control.Monad.Instances ()
import Data.Maybe
import Prelude hiding ((||), (&&))

class HasFalse a where
    false :: a

class HasTrue a where
    true :: a

class Shortcircuit a where
    isTrue :: a -> Bool

isFalse :: (Shortcircuit a) => a -> Bool
isFalse = not . isTrue

if' :: (Shortcircuit a) => a -> b -> b -> b
if' x a b | isTrue x  = a
          | otherwise = b

unless' :: (Shortcircuit a) => a -> b -> b -> b
unless' x a b | isFalse x = a
              | otherwise = b

(??) :: (Shortcircuit a) => b -> b -> a -> b
a ?? b = \x -> if' x a b

(||) :: (Shortcircuit a) => a -> a -> a
(||) = join if'

(&&) :: (Shortcircuit a) => a -> a -> a
(&&) = join unless'

firstTrueOf :: (Shortcircuit a, HasFalse a) => [a] -> a
firstTrueOf = foldr (||) false

lastFalseOf :: (Shortcircuit a, HasTrue a) => [a] -> a
lastFalseOf = foldr (&&) true

orM :: (Monad m, Shortcircuit a) => m a -> m a -> m a
orM a b = a >>= \x -> (return x ?? b) x

andM :: (Monad m, Shortcircuit a) => m a -> m a -> m a
andM a b = a >>= \x -> (b ?? return x) x

firstTrueOfM :: (Monad m, Shortcircuit a, HasFalse a) => [m a] -> m a
firstTrueOfM = foldr orM (return false)

lastFalseOfM :: (Monad m, Shortcircuit a, HasTrue a) => [m a] -> m a
lastFalseOfM = foldr andM (return true)

instance HasTrue Bool where
    true = True

instance HasFalse Bool where
    false = False

instance Shortcircuit Bool where
    isTrue = (== True)

instance HasFalse (Maybe a) where
    false = Nothing

instance Shortcircuit (Maybe a) where
    isTrue = isJust

instance Shortcircuit (Either a b) where
    isTrue (Left _)  = False
    isTrue (Right _) = True
