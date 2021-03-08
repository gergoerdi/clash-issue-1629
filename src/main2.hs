{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
import Clash.Prelude hiding (Bundle(..), mealyB)

class Bundle a f res | f a -> res, f res -> a where
    bundle :: (Applicative f) => res -> f a

    default bundle :: (Applicative f, res ~ f a) => res -> f a
    bundle = id

    unbundle :: (Traversable f) => f a -> res

    default unbundle :: (Traversable f, res ~ f a) => f a -> res
    unbundle = id

instance Bundle Bit f (f Bit) where

instance Bundle (a, b) f (f a, f b) where
    bundle (x, y) = (,) <$> x <*> y
    unbundle xy = (fst <$> xy, snd <$> xy)

instance (KnownNat n) => Bundle (Vec n a) f (Vec n (f a)) where
    bundle = traverse# id
    unbundle = sequenceA . fmap lazyV

instance Bundle (Maybe a) f (f (Maybe a)) where

mealyB
  :: (HiddenClockResetEnable dom)
  => (Bundle i (Signal dom) i', Bundle o (Signal dom) o')
  => (NFDataX s)
  => (s -> i -> (s,o))
  -> s
  -> (i' -> o')
mealyB = hideClockResetEnable mealyBE

mealyBE
  :: (Bundle i (Signal dom) i', Bundle o (Signal dom) o')
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

main :: IO ()
main = pure ()
