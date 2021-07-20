-- | Common input check functions
module Mlabs.Control.Check(
    isNonNegative
  , isPositive
  , isPositiveRational
  , isUnitRange
  , isPositiveRay
  , isUnitRangeRay
) where

import PlutusTx.Prelude
import Control.Monad.Except (MonadError(..))
import qualified PlutusTx.Ratio as R

import Mlabs.Data.Ray (Ray)
import qualified Mlabs.Data.Ray as Ray

{-# INLINABLE isNonNegative #-}
-- | Execute further if `val` is not negative integer, otherwise throw error using `msg`.
isNonNegative :: (Applicative m, MonadError String m) => String -> Integer -> m ()
isNonNegative msg val
  | val >= 0  = pure ()
  | otherwise = throwError $ msg <> " should be non-negative"

{-# INLINABLE isPositive #-}
-- | Execute further if `val` is positive integer, otherwise throw error using `msg`.
isPositive :: (Applicative m, MonadError String m) => String -> Integer -> m ()
isPositive msg val
  | val > 0   = pure ()
  | otherwise = throwError $ msg <> " should be positive"

{-# INLINABLE isPositiveRational #-}
-- | Execute further if `val` is positive rational, otherwise throw error using `msg`.
isPositiveRational :: (Applicative m, MonadError String m) => String -> Rational -> m ()
isPositiveRational msg val
  | val > R.fromInteger 0 = pure ()
  | otherwise             = throwError $ msg <> " should be positive"

{-# INLINABLE isUnitRange #-}
-- | Execute further if `val` is in range [0, 1], otherwise throw error using `msg`.
isUnitRange :: (Applicative m, MonadError String m) => String -> Rational -> m ()
isUnitRange msg val
  | val >= R.fromInteger 0 && val <= R.fromInteger 1 = pure ()
  | otherwise                                        = throwError $ msg <> " should have unit range [0, 1]"

{-# INLINABLE isPositiveRay #-}
-- | Execute further if `val` is positive `Ray`, otherwise throw error using `msg`.
isPositiveRay :: (Applicative m, MonadError String m) => String -> Ray -> m ()
isPositiveRay msg val
  | val > Ray.fromInteger 0 = pure ()
  | otherwise               = throwError $ msg <> " should be positive"

{-# INLINABLE isUnitRangeRay #-}
-- | Execute further if `val` is `Ray` in range [0, 1], otherwise throw error using `msg`.
isUnitRangeRay :: (Applicative m, MonadError String m) => String -> Ray -> m ()
isUnitRangeRay msg val
  | val >= Ray.fromInteger 0 && val <= Ray.fromInteger 1 = pure ()
  | otherwise                                            = throwError $ msg <> " should have unit range [0, 1]"

