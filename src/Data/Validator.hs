{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Validator
    ( -- * core monad and runners
      ValidationM, ValidationT, ValidationRule, ValidationRuleT
    , runValidator, runValidatorT
      -- * combinators
    , (>=>), (<=<)
      -- * checks
    , minLength, maxLength, lengthBetween, notEmpty
    , largerThan, smallerThan, valueBetween
    , matchesRegex
    , conformsPred, conformsPredM
      -- * helper classes and types
    , HasLength(..), Stringable(..)
    , Int64
      -- * reexports
    , re, mkRegexQQ, Regex
    )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.Int
import Data.Stringable hiding (length)
import Text.Regex.PCRE.Heavy
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

-- | The validation monad
type ValidationM e = ValidationT e Identity

-- | The validation monad transformer
newtype ValidationT e m a
    = ValidationT { unValidationT :: EitherT e m a }
      deriving (Monad, Functor, Applicative, Alternative, MonadPlus, MonadTrans)

-- | Run a validation on a type 'a'
runValidator :: ValidationRule e a -> a -> Either e a
runValidator a b = runIdentity $ runValidatorT a b
{-# INLINE runValidator #-}

-- | Run a validation on a type 'a'
runValidatorT :: Monad m => ValidationRuleT e m a -> a -> m (Either e a)
runValidatorT validationSteps input =
    runEitherT $ unValidationT (validationSteps input)
{-# INLINE runValidatorT #-}

-- | A validation rule. Combine using >=> or <=< from Control.Monad
type ValidationRule e a = ValidationRuleT e Identity a

-- | A validation rule. Combine using >=> or <=< from Control.Monad
type ValidationRuleT e m a = a -> ValidationT e m a

-- | All types that have a length, eg. 'String', '[a]', 'Vector a', etc.
class HasLength a where
    getLength :: a -> Int64

instance HasLength [a] where
    getLength = fromIntegral . length
    {-# INLINE getLength #-}

instance HasLength T.Text where
    getLength = fromIntegral . T.length
    {-# INLINE getLength #-}

instance HasLength TL.Text where
    getLength = TL.length
    {-# INLINE getLength #-}

instance HasLength BS.ByteString where
    getLength = fromIntegral . BS.length
    {-# INLINE getLength #-}

instance HasLength BSL.ByteString where
    getLength = BSL.length
    {-# INLINE getLength #-}

-- | Mark a custom check as failed
checkFailed :: Monad m => e -> ValidationT e m a
checkFailed = ValidationT . left
{-# INLINE checkFailed #-}

-- | Check that the value is at least N elements long
minLength :: (Monad m, HasLength a) => Int64 -> e -> ValidationRuleT e m a
minLength lowerBound e obj = largerThan lowerBound e (getLength obj) >> return obj
{-# INLINE minLength #-}

-- | Check that the value is at maxium N elements long
maxLength :: (Monad m, HasLength a) => Int64 -> e -> ValidationRuleT e m a
maxLength upperBound e obj =
    smallerThan upperBound e (getLength obj) >> return obj
{-# INLINE maxLength #-}

-- | Check that the value's length is between N and M
lengthBetween :: (Monad m, HasLength a) => Int64 -> Int64 -> e -> ValidationRuleT e m a
lengthBetween lowerBound upperBound e obj = valueBetween lowerBound upperBound e (getLength obj) >> return obj
{-# INLINE lengthBetween #-}

-- | Specialized minLength with N = 1
notEmpty :: (Monad m, HasLength a) => e -> ValidationRuleT e m a
notEmpty = minLength 1
{-# INLINE notEmpty #-}

-- | Check that a value is larger than N
largerThan :: (Monad m, Ord a) => a -> e -> ValidationRuleT e m a
largerThan lowerBound = conformsPred (>= lowerBound)
{-# INLINE largerThan #-}

-- | Check that a value is smaller than N
smallerThan :: (Monad m, Ord a) => a -> e -> ValidationRuleT e m a
smallerThan upperBound = conformsPred (<= upperBound)
{-# INLINE smallerThan #-}

-- | Check that a value is between M and N
valueBetween :: (Monad m, Ord a) => a -> a -> e -> ValidationRuleT e m a
valueBetween lowerBound upperBound e = largerThan lowerBound e >=> smallerThan upperBound e
{-# INLINE valueBetween #-}

-- | Check that a value conforms a predicate
conformsPred :: Monad m => (a -> Bool) -> e -> ValidationRuleT e m a
conformsPred predicate e obj = unless (predicate obj) (checkFailed e) >> return obj
{-# INLINE conformsPred #-}

-- | Check that a value conforms a predicate
conformsPredM :: Monad m => (a -> m Bool) -> e -> ValidationRuleT e m a
conformsPredM predicate e obj =
    do res <- lift $ predicate obj
       unless res (checkFailed e) >> return obj
{-# INLINE conformsPredM #-}

-- | Checks that a value matches a regular expression
matchesRegex :: (Stringable a, Monad m) => Regex -> e -> ValidationRuleT e m a
matchesRegex r = conformsPred (=~ r)
{-# INLINE matchesRegex #-}