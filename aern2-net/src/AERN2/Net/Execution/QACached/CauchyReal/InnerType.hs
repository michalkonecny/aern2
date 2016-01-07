{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}
module AERN2.Net.Execution.QACached.CauchyReal.InnerType where

import Control.Arrow

import AERN2.Num

import AERN2.Net.Execution.QACached.Basics

data QACached_CauchyReal_ = 
    QACached_CauchyReal_ { cachedCR_name :: Maybe String, cachedCR_id :: ValueId }

instance (Arrow to) => CanAddMulScalarA to QACached_CauchyReal_ QACached_CauchyReal_
instance (Arrow to) => CanAddMulDivScalarA to QACached_CauchyReal_ QACached_CauchyReal_

instance (Arrow to) => CanNegA to QACached_CauchyReal_
instance (Arrow to) => CanRecipA to QACached_CauchyReal_

instance (Arrow to) => CanAddA to QACached_CauchyReal_ QACached_CauchyReal_

instance (Arrow to) => CanAddThisA to QACached_CauchyReal_ QACached_CauchyReal_
instance (Arrow to) => CanAddSameTypeA to QACached_CauchyReal_

instance (Arrow to) => CanSubA to QACached_CauchyReal_ QACached_CauchyReal_

instance (Arrow to) => CanSubThisA to QACached_CauchyReal_ QACached_CauchyReal_
instance (Arrow to) => CanSubSameTypeA to QACached_CauchyReal_

instance (Arrow to) => CanMulA to QACached_CauchyReal_ QACached_CauchyReal_

instance (Arrow to) => CanMulByA to QACached_CauchyReal_ QACached_CauchyReal_
instance (Arrow to) => CanMulSameTypeA to QACached_CauchyReal_

instance (Arrow to) => CanDivA to QACached_CauchyReal_ QACached_CauchyReal_

instance (Arrow to) => CanDivByA to QACached_CauchyReal_ QACached_CauchyReal_
instance (Arrow to) => CanDivSameTypeA to QACached_CauchyReal_


instance (Arrow to) => CanAddMulScalarA to QACached_CauchyReal_ CauchyReal_
instance (Arrow to) => CanAddMulDivScalarA to QACached_CauchyReal_ CauchyReal_

instance (Arrow to) => CanAddA to QACached_CauchyReal_ CauchyReal_ where
    type AddTypeA to QACached_CauchyReal_ CauchyReal_ = QACached_CauchyReal_
instance (Arrow to) => CanAddA to CauchyReal_ QACached_CauchyReal_ where
    type AddTypeA to CauchyReal_ QACached_CauchyReal_ = QACached_CauchyReal_
instance (Arrow to) => CanAddThisA to QACached_CauchyReal_ CauchyReal_

instance (Arrow to) => CanSubA to QACached_CauchyReal_ CauchyReal_
instance (Arrow to) => CanSubThisA to QACached_CauchyReal_ CauchyReal_

instance (Arrow to) => CanMulA to QACached_CauchyReal_ CauchyReal_ where
    type MulTypeA to QACached_CauchyReal_ CauchyReal_ = QACached_CauchyReal_
instance (Arrow to) => CanMulA to CauchyReal_ QACached_CauchyReal_ where
    type MulTypeA to CauchyReal_ QACached_CauchyReal_ = QACached_CauchyReal_
instance (Arrow to) => CanMulByA to QACached_CauchyReal_ CauchyReal_ where

instance (Arrow to) => CanDivA to QACached_CauchyReal_ CauchyReal_ where
    type DivTypeA to QACached_CauchyReal_ CauchyReal_ = QACached_CauchyReal_
instance (Arrow to) => CanDivA to CauchyReal_ QACached_CauchyReal_ where
    type DivTypeA to CauchyReal_ QACached_CauchyReal_ = QACached_CauchyReal_
instance (Arrow to) => CanDivByA to QACached_CauchyReal_ CauchyReal_

