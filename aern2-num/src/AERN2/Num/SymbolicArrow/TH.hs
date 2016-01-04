{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module AERN2.Num.SymbolicArrow.TH where

import Language.Haskell.TH
import Control.Arrow
import qualified Data.Map as Map

import AERN2.Num.Operations
import AERN2.Num.CauchyReal
import AERN2.Num.SymbolicArrow.Expression

exprA :: (Q Exp) -> (Q Exp)
exprA expM = 
    do
    e <- expM
    let (eWithVars, varNames) = getVarsAndFillInExpr e []
    eA <- [| (arr (\ $(varTupleP varNames) -> Map.fromList $(varMapList varNames)))  >>> (realExpr2arrow ($(return eWithVars))) |]
    eT <- [t| (FieldA to r, ConvertibleA to CauchyReal r) => to $(inputType varNames [t|r|]) r  |]
    return $ SigE eA eT
    where
    varMapList varNames =
        do
        elems <- mapM mkVarPair varNames
        return $ ListE elems
        where
        mkVarPair varNameS =
            [| ($(return $ LitE $ StringL varNameS), $(varE (mkName varNameS)) ) |]
    varTupleP varNames =
        do
        return $ TupP $ map mkVarP varNames
        where
        mkVarP varNameS =
            VarP (mkName varNameS)
    inputType varNames rM =
        do
        r <- rM
        return $ foldr (addR r) (TupleT (length varNames)) varNames
        where
        addR r _ prevType = AppT prevType r
            

getVarsAndFillInExpr :: Exp -> [String] -> (Exp, [String])
getVarsAndFillInExpr e prevVars =
    case e of
        LetE decls body ->
            (LetE decls' body', vars')
            where
            (body', vars') = getVarsAndFillInExpr body vars1
            (decls', vars1) = getVarsAndFillInDecs decls prevVars
        _ -> (e, prevVars)

getVarsAndFillInDecs :: [Dec] -> [String] -> ([Dec], [String])
getVarsAndFillInDecs decls prevVars =
    case decls of
        [] -> (decls, prevVars)
        (decl : rest) ->
            (decl' : rest', vars')
            where
            (decl', vars1) = getVarsAndFillInDec decl prevVars
            (rest', vars') = getVarsAndFillInDecs rest vars1

getVarsAndFillInDec :: Dec -> [String] -> (Dec, [String])
getVarsAndFillInDec dec prevVars =
    case dec of
        ValD pat body [] ->
            (ValD pat' body' [], vars')
            where
            (pat', body', vars') = getVarsAndFillInValD pat body prevVars
        _ ->
            (dec, prevVars) -- shall we throw an error instead?

getVarsAndFillInValD :: Pat -> Body -> [String] -> (Pat, Body, [String])
getVarsAndFillInValD pat body prevVars =
    case (pat, body) of
        (ListP pats, NormalB (VarE realsName))
            | (show realsName) == ("AERN2.Num.SymbolicArrow.TH.vars" :: String) ->
                (ListP pats, (NormalB (ListE reVars)), vars')
            where
            (reVars, vars') = foldr processPat ([], prevVars) pats
            processPat (VarP varName) (prevREVars, vars1) =
                (reVar : prevREVars, (show varName) : vars1)
                where
                reVar = 
                    AppE (VarE (mkName "AERN2.Num.SymbolicArrow.Expression.var")) (LitE (StringL (show varName)))
        _ ->
            (pat, body, prevVars)

vars :: [R]
vars = []

data R = R
instance Ring R
instance Field R
instance HasIntegers R
instance HasRationals R
instance HasCauchyReals R
instance HasEqA (->) R R where
    equalToA = error "R"
instance HasOrderA (->) R R where
    lessThanA = error "R"
    leqA = error "R"
instance CanNeg R
instance CanNegSameType R
instance CanRecip R
instance CanRecipSameType R
instance CanAdd R R
instance CanAddThis R R
instance CanAddSameType R
instance CanSub R R
instance CanSubThis R R
instance CanSubSameType R
instance CanMul R R
instance CanMulBy R R
instance CanMulSameType R
instance CanDiv R R
instance CanDivBy R R
instance CanDivSameType R

instance CanAddA (->) R Integer where
    type AddTypeA (->) R Integer = R
instance CanAddA (->) Integer R where
    type AddTypeA (->) Integer R = R
instance CanAddThis R Integer

instance CanSubA (->) R Integer where
    type SubTypeA (->) R Integer = R
instance CanSubA (->) Integer R where
    type SubTypeA (->) Integer R = R
instance CanSubThis R Integer

instance CanMulA (->) R Integer where
    type MulTypeA (->) R Integer = R
instance CanMulA (->) Integer R where
    type MulTypeA (->) Integer R = R
instance CanMulBy R Integer

instance CanDivA (->) R Integer where
    type DivTypeA (->) R Integer = R
instance CanDivA (->) Integer R where
    type DivTypeA (->) Integer R = R
instance CanDivBy R Integer

instance CanAddA (->) R Rational where
    type AddTypeA (->) R Rational = R
instance CanAddA (->) Rational R where
    type AddTypeA (->) Rational R = R
instance CanAddThis R Rational

instance CanSubA (->) R Rational where
    type SubTypeA (->) R Rational = R
instance CanSubA (->) Rational R where
    type SubTypeA (->) Rational R = R
instance CanSubThis R Rational

instance CanMulA (->) R Rational where
    type MulTypeA (->) R Rational = R
instance CanMulA (->) Rational R where
    type MulTypeA (->) Rational R = R
instance CanMulBy R Rational

instance CanDivA (->) R Rational where
    type DivTypeA (->) R Rational = R
instance CanDivA (->) Rational R where
    type DivTypeA (->) Rational R = R
instance CanDivBy R Rational

instance CanAddA (->) R CauchyReal where
    type AddTypeA (->) R CauchyReal = R
instance CanAddA (->) CauchyReal R where
    type AddTypeA (->) CauchyReal R = R
instance CanAddThis R CauchyReal

instance CanSubA (->) R CauchyReal where
    type SubTypeA (->) R CauchyReal = R
instance CanSubA (->) CauchyReal R where
    type SubTypeA (->) CauchyReal R = R
instance CanSubThis R CauchyReal

instance CanMulA (->) R CauchyReal where
    type MulTypeA (->) R CauchyReal = R
instance CanMulA (->) CauchyReal R where
    type MulTypeA (->) CauchyReal R = R
instance CanMulBy R CauchyReal

instance CanDivA (->) R CauchyReal where
    type DivTypeA (->) R CauchyReal = R
instance CanDivA (->) CauchyReal R where
    type DivTypeA (->) CauchyReal R = R
instance CanDivBy R CauchyReal
    