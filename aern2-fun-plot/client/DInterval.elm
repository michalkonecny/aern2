module DInterval exposing (..)

-- import Set exposing (Set)
-- import Dict exposing (Dict)
-- import List

import Api exposing (..)

type alias DInterval = DyadicIntervalAPI

intersect : DyadicIntervalAPI -> DyadicIntervalAPI -> Maybe DyadicIntervalAPI
intersect di1 di2 =
  let
    l1 = di1.dyadic_endpointL
    r1 = di1.dyadic_endpointR
    l2 = di2.dyadic_endpointL
    r2 = di2.dyadic_endpointR
    l = maxD l1 l2
    r = minD r1 r2
    lF = dToFloat l
    rF = dToFloat r
  in
  if lF <= rF then Just (DyadicIntervalAPI l r) else Nothing

union : DyadicIntervalAPI -> DyadicIntervalAPI -> DyadicIntervalAPI
union di1 di2 =
  let
    l1 = di1.dyadic_endpointL
    r1 = di1.dyadic_endpointR
    l2 = di2.dyadic_endpointL
    r2 = di2.dyadic_endpointR
  in
  DyadicIntervalAPI (minD l1 l2) (maxD r1 r2)

unions : List DyadicIntervalAPI -> DyadicIntervalAPI
unions list =
  case list of
    [] -> unit
    (h :: t) -> List.foldl union h t

unit : DyadicIntervalAPI
unit = DyadicIntervalAPI (DyadicS (-1) 0) (DyadicS 1 0)

width : DyadicIntervalAPI -> DyadicS
width di =
  let
    l = di.dyadic_endpointL
    r = di.dyadic_endpointR
  in
  r `subD` l

minD : DyadicS -> DyadicS -> DyadicS
minD d1 d2 =
  let
    x1 = dToFloat d1
    x2 = dToFloat d2
  in
  if x1 < x2 then d1 else d2

maxD : DyadicS -> DyadicS -> DyadicS
maxD d1 d2 =
  let
    x1 = dToFloat d1
    x2 = dToFloat d2
  in
  if x1 < x2 then d2 else d1

subD : DyadicS -> DyadicS -> DyadicS
subD d1 d2 =
  let
    (d1',d2') = dUnifyExp d1 d2
    e = d1'.dyadic_exp
    v1 = d1'.dyadic_value
    v2 = d2'.dyadic_value
  in
  DyadicS (v1 - v2) e

mulDi : DyadicS -> Int -> DyadicS
mulDi d n =
  let
    v = d.dyadic_value
    e = d.dyadic_exp
    aux v e n =
      if n % 2 == 0 then aux v (e + 1) (n // 2)
      else DyadicS (n*v) e
  in
  if n == 0 then DyadicS 0 0 else aux v e n

divDi : DyadicS -> Int -> DyadicS
divDi d n =
  let
    v = d.dyadic_value
    e = d.dyadic_exp
    aux v e n =
      if n % 2 == 0 then aux v (e - 1) (n // 2)
      else
        let
          nE = round <| logBase 2 <| toFloat <| abs n
        in
        DyadicS ((2^nE*v)//n) (e-nE)
  in
  if n == 0
    then Debug.crash "division by 0"
    else aux v e n

dUnifyExp : DyadicS -> DyadicS -> (DyadicS, DyadicS)
dUnifyExp d1 d2 =
  let
    v1 = d1.dyadic_value
    e1 = d1.dyadic_exp
    v2 = d2.dyadic_value
    e2 = d2.dyadic_exp
    e = e1 `min` e2
    v1' = v1 * (2^(e1-e))
    v2' = v2 * (2^(e2-e))
  in
  ( DyadicS v1' e, DyadicS v2' e )

dToFloat : DyadicS -> Float
dToFloat d =
  let
    v = toFloat (d.dyadic_value)
    e = toFloat (d.dyadic_exp)
  in
  v * (2.0^e)
