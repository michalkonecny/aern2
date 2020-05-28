module AERN2.BoxFunMinMax.Type where

import AERN2.MP.Ball
import MixedTypesNumPrelude
import AERN2.BoxFun.Type
import AERN2.BoxFun.Box
import AERN2.BoxFun.TestFunctions
import AERN2.BoxFunMinMax.Optimisation

import Data.Maybe

import Debug.Trace

-- All leaves in the tree must have the same domain
data MinMaxTree = Leaf {tree_f :: BoxFun} | Min {tree_l :: MinMaxTree, tree_r :: MinMaxTree} | Max {tree_l :: MinMaxTree, tree_r :: MinMaxTree}

heronTree2 :: MinMaxTree
heronTree2 = Max 
            (Min heron1 heron2)
            (Min heron3 heron4)
            
heron1 :: MinMaxTree
heron1 = Max (Leaf (heron1p)) (Leaf (heron1q))

heron2 :: MinMaxTree
heron2 = Max (Leaf (heron2p)) (Leaf (heron2q))

heron3 :: MinMaxTree
heron3 = Max (Leaf (heron3p)) (Leaf (heron3q))

heron4 :: MinMaxTree
heron4 = Max (Leaf (heron4p)) (Leaf (heron4q))

checkTree :: MinMaxTree -> Box -> Accuracy -> Precision -> Maybe Bool
checkTree (Leaf f) box ac p = 
  trace ("Checking on domain: " ++ show (getEndpoints box))
  trace ("rough bound gives: " ++ show ((applyMinimumOnBox (f) box')))
  Just (applyMinimumOnBox (f) box' !>! 0)
  ||
  globalMinimumGreaterThanN (BoxFun (dimension (f)) (bf_eval (f)) box') ac p (cn 0.0)
  where
    box' = setPrecision p box
checkTree (Max l r) box ac p
  | rRange !<! 0 =
    trace "Right branch below 0"
    trace ("Left  range: " ++ show (endpointsAsIntervals lRange))
    trace ("Right range: " ++ show (endpointsAsIntervals rRange))
    checkTree l box ac p

  | lRange !<! 0 =
    trace "Left branch below 0"
    trace ("Left  range: " ++ show (endpointsAsIntervals lRange))
    trace ("Right range: " ++ show (endpointsAsIntervals rRange))
    checkTree r box ac p
  | otherwise      =
    (lRange !>! 0 || rRange !>! 0) -- || (lOptAboveZero || rOptAboveZero)
    ||
    (if AERN2.BoxFun.Box.width box' !>! 1 / (200) then
        trace ("Bisected boxes: " ++ show newBoxes)
        checkBoxes newBoxes
      else
        trace "Stopping bisections (Box too small)" $
        case (checkTree l box' ac p, checkTree r box' ac p) of
          (Just True, _) -> Just True
          (_, Just True) -> Just True
          (_, _)         -> trace "indeterminate" 
                            Nothing)
    where
      lRange = applyTree l box'
      rRange = applyTree r box'
      
      lOptAboveZero = fromMaybe False (globalMinimumAbove0Tree l box')
      rOptAboveZero = fromMaybe False (globalMinimumAbove0Tree r box')
 
      box' = setPrecision p box

      newBoxes = fullBisect box'

      checkBoxes []           = Just True
      checkBoxes [b]          = checkTree (Max l r) b ac p
      checkBoxes (b : boxes)  = (&&) <$> checkBoxes [b]
                                <*>
                                checkBoxes boxes
checkTree (Min l r) box ac p = checkTree l box ac p && checkTree r box ac p

size :: MinMaxTree -> Integer
size (Leaf _)  = 1
size (Min l r) = 1 + size l + size r
size (Max l r) = 1 + size l + size r

applyTree :: MinMaxTree -> Box -> CN MPBall

-- applyTree (Leaf f)  box = globalMinimumAboveN (BoxFun (dimension (fst f)) (bf_eval (fst f)) box) (bits 100) (prec 1000) (cn 0.0)
applyTree (Leaf f)  box = apply (f) box
applyTree (Max l r) box = max (applyTree l box) (applyTree r box)
applyTree (Min l r) box = min (applyTree l box) (applyTree r box)

-- TODO: globalMinimum and globalMaximum with low accuracy/precision to compute more accurate r and lRanges

domainTree :: MinMaxTree -> Box
domainTree (Leaf f) = domain (f) 
domainTree (Min l _) = domainTree l 
domainTree (Max l _) = domainTree l

globalMinimumAbove0Tree :: MinMaxTree -> Box -> Maybe Bool
globalMinimumAbove0Tree (Leaf f) box  = globalMinimumGreaterThanN (BoxFun (dimension (f)) (bf_eval (f)) box) (bits 10) (prec 10) (cn 0.0)
globalMinimumAbove0Tree (Max l r) box = (||) <$> globalMinimumAbove0Tree l box <*> globalMinimumAbove0Tree r box
globalMinimumAbove0Tree (Min l r) box = (&&) <$> globalMinimumAbove0Tree l box <*> globalMinimumAbove0Tree r box
