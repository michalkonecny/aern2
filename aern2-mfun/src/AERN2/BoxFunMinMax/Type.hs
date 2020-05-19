module AERN2.BoxFunMinMax.Type where

import AERN2.MP.Ball
import MixedTypesNumPrelude
import AERN2.BoxFun.Type
import AERN2.BoxFun.Box
import AERN2.BoxFun.TestFunctions
import AERN2.BoxFun.Optimisation

import Debug.Trace

data MinMaxTree = Leaf {tree_f :: BoxFun} | Min {tree_l :: MinMaxTree, tree_r :: MinMaxTree} | Max {tree_l :: MinMaxTree, tree_r :: MinMaxTree}

heronTree :: MinMaxTree
heronTree = Max 
            (Min heron1 heron2)
            (Min heron3 heron4)
            
heron1 :: MinMaxTree
heron1 = Max (Leaf heron1p) (Leaf heron1q)

heron2 :: MinMaxTree
heron2 = Max (Leaf heron2p) (Leaf heron2q)

heron3 :: MinMaxTree
heron3 = Max (Leaf heron3p) (Leaf heron3q)

heron4 :: MinMaxTree
heron4 = Max (Leaf heron4p) (Leaf heron4q)

checkTree :: MinMaxTree -> Bool
checkTree (Leaf f) = globalMinimumGreaterThanN f (bits 100) (0/1) (prec 1000)
checkTree (Max (Leaf f) (Leaf g))
  | fGTg f g fbox' = checkTree (Leaf f)
  | fGTg g f fbox' = checkTree (Leaf g)
  | otherwise      = 
    let
      newBoxes = fullBisect fbox'
      updateDomain z = BoxFun (dimension z) (bf_eval z)
      
      newTree [] = undefined
      newTree [box]         = Max (Leaf (updateDomain f box)) (Leaf (updateDomain g box))
      newTree (box : boxes) = Min
                                (Max (Leaf (updateDomain f box)) (Leaf (updateDomain g box)))
                                (newTree boxes)
    in
      ((getMinimumOnBox f fbox' !>! 0 || getMinimumOnBox g fbox' !>! 0)
      ||
      (if AERN2.BoxFun.Box.width fbox' !>! 1 / (2 ^ 5) then
          checkTree $ newTree newBoxes
        else
          checkTree (Leaf f) || checkTree (Leaf g)))
  where
    fbox = domain f
    fbox' = setPrecision (prec 1000) fbox

checkTree (Min (Leaf f) (Leaf g))
  | fLTg f g fbox' = checkTree (Leaf f)
  | fLTg g f fbox' = checkTree (Leaf g)
  | otherwise      = 
    let
      newBoxes = fullBisect (domain f)
      updateDomain z = BoxFun (dimension z) (bf_eval z)

      newTree [] = undefined
      newTree [box]         = Min (Leaf (updateDomain f box)) (Leaf (updateDomain g box))
      newTree (box : boxes) = Min
                                (Min (Leaf (updateDomain f box)) (Leaf (updateDomain g box)))
                                (newTree boxes)
    in
      ((getMinimumOnBox f fbox' !>! 0 && getMinimumOnBox g fbox' !>! 0)
      ||
      (if AERN2.BoxFun.Box.width fbox' !>! 1 / (2 ^ 5) then
        checkTree $ newTree newBoxes
      else
        checkTree (Leaf f) && checkTree (Leaf g)))
  where
    fbox = domain f
    fbox' = setPrecision (prec 1000) fbox
checkTree (Max l r) = checkTree l || checkTree r
checkTree (Min l r) = checkTree l && checkTree r

fGTg :: BoxFun -> BoxFun -> Box -> Bool
fGTg f g box = fst (endpointsAsIntervals (apply f box)) !>! snd (endpointsAsIntervals (apply g box))

fLTg :: BoxFun -> BoxFun -> Box -> Bool
fLTg f g box = snd (endpointsAsIntervals (apply f box)) !<! fst (endpointsAsIntervals (apply g box))

size :: MinMaxTree -> Integer
size (Leaf _)  = 
  trace "Leaf, +1"
  1
size (Min l r) = 
  trace "Min, 1+l+r"
  1 + size l + size r
size (Max l r) =  
  trace "Max, 1+l+r"
  1 + size l + size r