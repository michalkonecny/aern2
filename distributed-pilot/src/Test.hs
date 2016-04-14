{-# LANGUAGE Arrows, ExistentialQuantification, TypeOperators, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test where

import Control.Arrow
import Unsafe.Coerce

class C c where
    type T1 c
    type T2 c

data AnyC to = 
    forall c.(C c) => AnyC c ((T1 c) `to` (T2 c))

{- GHC is buggy when dealing with this sort of code 
test :: 
    (ArrowApply to, ArrowChoice to) 
    =>
    c -> ((AnyC to, T1 c) `to` (T2 c))
test (_::c) =
    proc (ac, t1c) ->
        do
        case ac of
            (AnyC (_::c') f) ->
                do
                t2c <- app -< (f, (unsafeCoerce t1c) :: T1 c')
                returnA -< ((unsafeCoerce t2c) :: T2 c)
        
-}      