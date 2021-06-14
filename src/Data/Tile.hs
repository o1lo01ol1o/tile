{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Implemened from
-- Daisuke Dobashi
-- A CHARACTERIZATION OF TILING GROUPS
-- TSUKUBA J. MATH.
-- Vol. 32 No. 2 (2008), 323–334
module Data.Tile where

import Control.Lens
import Data.Bifunctor (Bifunctor (bimap))
import Data.Distributive
import Data.Finitary
import Data.Functor.Rep hiding (gindex, gtabulate)
import Data.List (findIndex)
import Data.Vec.DataFamily.SpineStrict.Pigeonhole (gindex, gitraverse, gtabulate)
import GHC.Generics hiding (Rep)
import GHC.Natural (Natural)
import Numeric.Interval

data Substitute = Prime | Prime' | Prime''
  deriving stock (Eq, Ord, Show, Bounded, Generic)
  deriving anyclass (Finitary)

data Substitution a = Substitution
  {prime :: !a, doublePrime :: !a, triplePrime :: !a}
  deriving stock (Eq, Ord, Show, Functor)

data Sigma a = Sigma a a a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Generic1)
  deriving anyclass (FunctorWithIndex Substitute, FoldableWithIndex Substitute)

instance TraversableWithIndex Substitute Sigma where
  itraverse = gitraverse

instance Representable Sigma where
  type Rep Sigma = Substitute
  index = gindex
  tabulate = gtabulate

instance Distributive Sigma where
  distribute = distributeRep
  collect = collectRep

-- |
-- >>> pure True :: Sigma Bool
-- Sigma True True True
instance Applicative Sigma where
  pure = pureRep
  (<*>) = apRep

newtype VStar t a = VStar {unVStar :: t (Sigma a)}
  deriving stock (Functor, Foldable, Traversable, Generic, Generic1)

deriving instance Eq (t (Sigma a)) => Eq (VStar t a)

deriving instance Ord (t (Sigma a)) => Ord (VStar t a)

deriving instance Show (t (Sigma a)) => Show (VStar t a)

-- deriving anyclass instance Representable VStar

-- | Class for tilings
-- >>> magnitude <$> interval (tileLength (minBound :: Bool)) (tileLength (maxBound :: Bool))
-- Just 1.0
class (Bounded a, Finitary a, Ord a) => Tiling a where
  tileLength :: Fractional b => a -> b

  sigma :: a -> Substitution a
  sigma a = Substitution a a a

data DoublyPointedLabel a = One a | Two a | OneTwo a | None a
  deriving stock (Eq, Ord, Show, Functor)

label :: Natural -> Natural -> [Substitution a] -> Maybe [DoublyPointedLabel (Substitution a)]
label 0 0 [] = Just []
label 1 1 [x] = Just [OneTwo x]
label i j xs
  | isValid && i == j = Just $ labelAt i OneTwo <$> zip [0 ..] xs
  | isValid = Just $ labelsAt i j One Two <$> zip [0 ..] xs
  | otherwise = Nothing
  where
    isValid = 1 <= i && j <= fromIntegral (length xs)

    labelAt ix l (i', a)
      | ix == i' = l a
      | otherwise = None a

    labelsAt ix jx la lb (i', a)
      | ix == i' = la a
      | jx == i' = lb a
      | otherwise = None a

isLabledOne :: DoublyPointedLabel a -> Bool
isLabledOne (OneTwo _) = True
isLabledOne (One _) = True
isLabledOne _ = False

isLabledTwo :: DoublyPointedLabel a -> Bool
isLabledTwo (OneTwo _) = True
isLabledTwo (Two _) = True
isLabledTwo _ = False

doublyMap :: Bifunctor p => (a -> d) -> p a a -> p d d
doublyMap f = bimap f f

jk :: [DoublyPointedLabel (Substitution a)] -> (Natural, Natural)
jk xs = doublyMap (fromIntegral . fromJust) (findIndex isLabledOne xs, findIndex isLabledTwo xs)

fromJust :: Maybe Int -> Int
fromJust (Just i) = i
fromJust _ = error "From jsut!"

starHolds :: Eq a => [Int] -> [Int] -> [DoublyPointedLabel (Substitution a)] -> [DoublyPointedLabel (Substitution a)] -> Bool
starHolds js ks xs ys = and $ zipWith (\j k -> (xs ^? ix j) == (ys ^? ix k)) js ks

-- newZ m q n j k a b xs ys p
--     | zp && j > k = xs ^? ix p
--     | zp && j < k = ys ^? ix p
--     | zp && j == k = error "j /= k!"
--     | zmp = xs ^? (j - a + p)
--     | zmpq && r - j > s - k = xs ^? ix (j + b + p)
--     | zmpq && r - j < s - k = ys ^? ix (j + b + p)
--     | zmpq && r - j == s - k = error "r - j == s - k!"
--     | otherwise = error "newZ: Unaccounted for!"
--     where
--         zp = 1 <= p && p <= m
--         zmp = 1 <= p && p <= q
--         zmpq = 1 <= p && p <= n

-- newWordZ m q n j k a b xs ys =  newZ m q n j k a b xs ys <$> [1 .. m + 1 + n]

-- binOp :: [DoublyPointedLabel (Substitution a)] -> [DoublyPointedLabel (Substitution a)] -> [DoublyPointedLabel (Substitution a)]
-- binOp x y
--     | starHolds
--     |
--     where
--         r = fromIntegral $ length x
--         s = fromIntegral $ length y
--         (i,j) = jk x
--         (k,l) = jk y
--         a = min j k
--         b = min (r - j) (s -k)
--         m = max j k
--         n = max (r - j) (s -k)
--         q = a + b
--         j' = [j - a + 1 .. j + b]
--         k' = [k-a + 1 .. k + b]
--         new = newWordZ m q n j k a b xs ys

instance Tiling Bool where
  tileLength False = 1 / 2
  tileLength True = 1
