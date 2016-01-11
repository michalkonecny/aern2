{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module AERN2.Num.SymbolicArrow.TH 
(exprA, predA, vars, exprAinternal, predAinternal)
where

import Language.Haskell.TH
import Control.Arrow
import qualified Data.Map as Map

import AERN2.Num.Operations
import AERN2.Num.SymbolicArrow.Expression

exprA :: Q Exp -> Q Exp
exprA = exprA_ "AERN2.Num.exprAvar"
predA :: Q Exp -> Q Exp
predA = predA_ "AERN2.Num.exprAvar"

exprAinternal :: Q Exp -> Q Exp
exprAinternal = exprA_ "AERN2.Num.SymbolicArrow.Expression.var"
predAinternal :: Q Exp -> Q Exp
predAinternal = predA_ "AERN2.Num.SymbolicArrow.Expression.var"

exprA_ :: String -> (Q Exp) -> (Q Exp)
exprA_ varQualifiedName expM = 
    do
    e <- expM
    let (eWithVars, varNames) = getVarsAndFillInExpr varQualifiedName e []
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
            
predA_ :: String -> (Q Exp) -> (Q Exp)
predA_ varQualifiedName expM = 
    do
    e <- expM
    let (eWithVars, varNames) = getVarsAndFillInExpr varQualifiedName e []
    eA <- [| (arr (\ $(varTupleP varNames) -> Map.fromList $(varMapList varNames)))  >>> (realPred2arrow ($(return eWithVars))) |]
    _ <- varTupleP [] -- useless, only here to avoid an erroneous unused warning
    eT <- [t| (RealExprA to r) => to $(inputType varNames [t|r|]) (EqCompareTypeA to r r)  |]
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
            

getVarsAndFillInExpr :: String -> Exp -> [String] -> (Exp, [String])
getVarsAndFillInExpr varQualifiedName e prevVars =
    case e of
        LetE decls body ->
            (LetE decls' body', vars')
            where
            (body', vars') = getVarsAndFillInExpr varQualifiedName body vars1
            (decls', vars1) = getVarsAndFillInDecs varQualifiedName decls prevVars
        _ -> (e, prevVars)

getVarsAndFillInDecs :: String -> [Dec] -> [String] -> ([Dec], [String])
getVarsAndFillInDecs varQualifiedName decls prevVars =
    case decls of
        [] -> (decls, prevVars)
        (decl : rest) ->
            (decl' : rest', vars')
            where
            (decl', vars1) = getVarsAndFillInDec varQualifiedName decl prevVars
            (rest', vars') = getVarsAndFillInDecs varQualifiedName rest vars1

getVarsAndFillInDec :: String -> Dec -> [String] -> (Dec, [String])
getVarsAndFillInDec varQualifiedName dec prevVars =
    case dec of
        ValD pat body [] ->
            (ValD pat' body' [], vars')
            where
            (pat', body', vars') = getVarsAndFillInValD varQualifiedName pat body prevVars
        _ ->
            (dec, prevVars) -- shall we throw an error instead?

getVarsAndFillInValD :: String -> Pat -> Body -> [String] -> (Pat, Body, [String])
getVarsAndFillInValD varQualifiedName pat body prevVars =
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
                    AppE (VarE (mkName varQualifiedName)) (LitE (StringL (show varName)))
            processPat _ _ = error "AERN2.Num.SymbolicArrow.TH.getVarsAndFillInValD: internal error"
        _ ->
            (pat, body, prevVars)

vars :: [RealExpr]
vars = []
