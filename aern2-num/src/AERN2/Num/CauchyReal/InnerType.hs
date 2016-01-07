{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}
module AERN2.Num.CauchyReal.InnerType where

import Control.Arrow

import AERN2.Num.Operations
import AERN2.Num.MPBall
import AERN2.Num.Accuracy

data CauchyReal_ = 
    CauchyReal_ { cr_name :: Maybe String, cr_seq :: Accuracy -> MPBall } 

instance (Arrow to) => CanAddMulScalarA to CauchyReal_ CauchyReal_
instance (Arrow to) => CanAddMulDivScalarA to CauchyReal_ CauchyReal_

instance (Arrow to) => CanNegA to CauchyReal_
instance (Arrow to) => CanRecipA to CauchyReal_

instance (Arrow to) => CanAddA to CauchyReal_ CauchyReal_

instance (Arrow to) => CanAddThisA to CauchyReal_ CauchyReal_
instance (Arrow to) => CanAddSameTypeA to CauchyReal_

instance (Arrow to) => CanSubA to CauchyReal_ CauchyReal_

instance (Arrow to) => CanSubThisA to CauchyReal_ CauchyReal_
instance (Arrow to) => CanSubSameTypeA to CauchyReal_

instance (Arrow to) => CanMulA to CauchyReal_ CauchyReal_

instance (Arrow to) => CanMulByA to CauchyReal_ CauchyReal_
instance (Arrow to) => CanMulSameTypeA to CauchyReal_

instance (Arrow to) => CanDivA to CauchyReal_ CauchyReal_

instance (Arrow to) => CanDivByA to CauchyReal_ CauchyReal_
instance (Arrow to) => CanDivSameTypeA to CauchyReal_

