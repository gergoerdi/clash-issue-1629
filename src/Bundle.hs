{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Bundle where

import Clash.Prelude hiding (Bundle(..), mealyB)

class Bundle f a res | f a -> res, f res -> a where
    bundle :: (Applicative f) => res -> f a

    default bundle :: (res ~ f a) => res -> f a
    bundle = id

    unbundle :: (Traversable f) => f a -> res

    default unbundle :: (res ~ f a) => f a -> res
    unbundle = id

instance Bundle f Bit (f Bit) where

instance Bundle f (a, b) (f a, f b) where
    bundle (x, y) = (,) <$> x <*> y
    unbundle xy = (fst <$> xy, snd <$> xy)

instance (KnownNat n) => Bundle f (Vec n a) (Vec n (f a)) where
    bundle = traverse# id
    unbundle = sequenceA . fmap lazyV

instance Bundle f (Maybe a) (f (Maybe a)) where

mealyB
  :: (HiddenClockResetEnable dom)
  => (Bundle (Signal dom) i i', Bundle (Signal dom) o o')
  => (NFDataX s)
  => (s -> i -> (s,o))
  -> s
  -> (i' -> o')
mealyB = hideClockResetEnable mealyBE

mealyBE
  :: (Bundle (Signal dom) i i', Bundle (Signal dom) o o')
  => (NFDataX s)
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> (s -> i -> (s,o))
  -> s
  -> (i' -> o')
mealyBE clk rst en f s0 = unbundle . mealyE clk rst en f s0 . bundle

mealyE
  :: (NFDataX s)
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> (s -> i -> (s,o))
  -> s
  -> (Signal dom i -> Signal dom o)
mealyE = undefined

foo :: (HiddenClockResetEnable dom) => (Signal dom Int, Signal dom Int) -> (Signal dom Int, Signal dom Int)
foo = mealyB minMax (0, 0)
  where
    minMax :: (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
    minMax (x, y) (minX, maxY) = ((x + maxY, y + minX), (min x minX, max y maxY))
