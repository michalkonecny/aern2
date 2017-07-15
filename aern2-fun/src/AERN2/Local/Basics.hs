module AERN2.Local.Basics where

import AERN2.MP.Accuracy
import AERN2.MP.Dyadic

type Local a = Dyadic -> Dyadic -> Accuracy -> a

constant :: a -> Local a
constant x = \_l _r _ac -> x

liftLocal1 :: (a -> b) -> Local a -> Local b
liftLocal1 f locX =
  \l r ac -> f (locX l r ac)

liftLocal2 :: (a -> b -> c) -> Local a -> Local b -> Local c
liftLocal2 op locX locY =
  \l r ac -> (locX l r ac) `op` (locY l r ac)

instance HasAccuracyGuide (Local a) where
  getAccuracyGuide _ = NoInformation

instance CanSetAccuracyGuide (Local a) where
  setAccuracyGuide _ f = f
