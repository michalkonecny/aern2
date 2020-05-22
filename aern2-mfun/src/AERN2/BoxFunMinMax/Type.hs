module AERN2.BoxFunMinMax.Type where

import AERN2.MP.Ball
import MixedTypesNumPrelude
import AERN2.BoxFun.Type
import AERN2.BoxFun.Box
import AERN2.BoxFun.TestFunctions
import AERN2.BoxFunMinMax.Optimisation

import Debug.Trace

-- All leaves in the tree must have the same domain
data MinMaxTree = Leaf {tree_f :: (BoxFun, String)} | Min {tree_l :: MinMaxTree, tree_r :: MinMaxTree} | Max {tree_l :: MinMaxTree, tree_r :: MinMaxTree}

heronTree :: MinMaxTree
heronTree = Max 
            (Min heron1 heron2)
            (Min heron3 heron4)
            
heron1 :: MinMaxTree
heron1 = Max (Leaf (heron1p, "heron1p")) (Leaf (heron1q,"heron1q"))

heron2 :: MinMaxTree
heron2 = Max (Leaf (heron2p, "heron2p")) (Leaf (heron2q, "heron2q"))

heron3 :: MinMaxTree
heron3 = Max (Leaf (heron3p, "heron3p")) (Leaf (heron3q, "heron3q"))

heron4 :: MinMaxTree
heron4 = Max (Leaf (heron4p, "heron4p")) (Leaf (heron4q, "heron4p"))

checkTree :: MinMaxTree -> Box -> Accuracy -> Precision -> Bool
checkTree (Leaf f) box ac p = 
  trace ("Checking " ++ snd f ++ "on domain: " ++ show (getEndpoints box))
  trace ("rough bound gives: " ++ show ((applyMinimumOnBox (fst f) box')))
  (applyMinimumOnBox (fst f) box' !>! 0)
  ||
  case globalMinimumGreaterThanN (BoxFun (dimension (fst f)) (bf_eval (fst f)) box') ac p (cn 0.0) of
    Just b  -> b
    Nothing -> False
  where
    box' = setPrecision p box
checkTree (Max l r) box ac p
  | lRange !>! rRange = 
    trace "Left branch greater than right branch"
    trace ("Left  range: " ++ show (endpointsAsIntervals lRange))
    trace ("Right range: " ++ show (endpointsAsIntervals rRange))
    checkTree l box ac p

  | rRange !>! lRange = 
    trace "Right branch greater than left branch"
    trace ("Left  range: " ++ show (endpointsAsIntervals lRange))
    trace ("Right range: " ++ show (endpointsAsIntervals rRange))
    checkTree r box ac p

  | otherwise      =
    (lRange !>! 0 || rRange !>! 0)
    ||
    (if AERN2.BoxFun.Box.width box' !>! 1 / (fromAccuracy ac ^ 2) then
        trace ("Bisected boxes: " ++ show newBoxes)
        checkBoxes newBoxes
      else
        trace "Stopping bisections (Box too small)"
        checkTree l box' ac p || checkTree r box' ac p)
    where
      lRange = applyTree l box'
      rRange = applyTree r box'
 
      box' = setPrecision p box

      newBoxes = fullBisect box'

      checkBoxes []           = True
      checkBoxes [b]          = checkTree (Max l r) b ac p
      checkBoxes (b : boxes)  = checkBoxes [b]
                                &&
                                checkBoxes boxes
checkTree (Min l r) box ac p = checkTree l box ac p && checkTree r box ac p

size :: MinMaxTree -> Integer
size (Leaf _)  = 1
size (Min l r) = 1 + size l + size r
size (Max l r) = 1 + size l + size r

applyTree :: MinMaxTree -> Box -> CN MPBall
applyTree (Leaf f)  box = apply (fst f) box
applyTree (Max l r) box = max (applyTree l box) (applyTree r box)
applyTree (Min l r) box = min (applyTree l box) (applyTree r box)

domainTree :: MinMaxTree -> Box
domainTree (Leaf f) = domain (fst f) 
domainTree (Min l _) = domainTree l 
domainTree (Max l _) = domainTree l
