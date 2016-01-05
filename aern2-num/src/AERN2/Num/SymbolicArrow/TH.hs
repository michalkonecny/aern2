{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module AERN2.Num.SymbolicArrow.TH 
(exprA, predA, vars)
where

import Language.Haskell.TH
import Control.Arrow
import qualified Data.Map as Map

import AERN2.Num.Operations
import AERN2.Num.SymbolicArrow.Expression

exprA :: (Q Exp) -> (Q Exp)
exprA expM = 
    do
    e <- expM
    let (eWithVars, varNames) = getVarsAndFillInExpr e []
    eA <- [| (arr (\ $(varTupleP varNames) -> Map.fromList $(varMapList varNames)))  >>> (realExpr2arrow ($(return eWithVars))) |]
    _ <- varTupleP [] -- useless, only here to avoid an erroneous unused warning
    eT <- [t| (RealExprA to r) => to $(inputType varNames [t|r|]) r  |]
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
            
predA :: (Q Exp) -> (Q Exp)
predA expM = 
    do
    e <- expM
    let (eWithVars, varNames) = getVarsAndFillInExpr e []
    eA <- [| (arr (\ $(varTupleP varNames) -> Map.fromList $(varMapList varNames)))  >>> (realPred2arrow ($(return eWithVars))) |]
    _ <- varTupleP [] -- useless, only here to avoid an erroneous unused warning
    eT <- [t| (RealPredA to r) => to $(inputType varNames [t|r|]) (EqCompareTypeA to r r)  |]
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
                    AppE (VarE (mkName "AERN2.Num.SymbolicArrow.var")) (LitE (StringL (show varName)))
            processPat _ _ = error "AERN2.Num.SymbolicArrow.TH.getVarsAndFillInValD: internal error"
        _ ->
            (pat, body, prevVars)

vars :: [RealExpr]
vars = []
