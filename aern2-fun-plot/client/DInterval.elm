module DInterval exposing (..)

-- import Set exposing (Set)
-- import Dict exposing (Dict)
-- import List

import Api exposing (..)

type alias DInterval = DyadicIntervalAPI

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

minD : DyadicS -> DyadicS -> DyadicS
minD d1 d2 =
  let
    v1 = toFloat (d1.dyadic_value)
    e1 = toFloat (d1.dyadic_exp)
    v2 = toFloat (d2.dyadic_value)
    e2 = toFloat (d2.dyadic_exp)
    x1 = v1 * (2.0^e1)
    x2 = v2 * (2.0^e2)
  in
  if x1 < x2 then d1 else d2

maxD : DyadicS -> DyadicS -> DyadicS
maxD d1 d2 =
  let
    v1 = toFloat (d1.dyadic_value)
    e1 = toFloat (d1.dyadic_exp)
    v2 = toFloat (d2.dyadic_value)
    e2 = toFloat (d2.dyadic_exp)
    x1 = v1 * (2.0^e1)
    x2 = v2 * (2.0^e2)
  in
  if x1 < x2 then d2 else d1
