apihs=../server/src/AERN2/RealFun/PlotService/API.hs
ghcextensions=-XRebindableSyntax -XDataKinds -XTypeOperators -XTypeSynonymInstances -XDeriveGeneric

../assets/index.html: Api.elm Main.elm
	elm make Main.elm
	mv index.html ../assets/

Api.elm: GenerateElm.hs $(apihs)
	(cd ../ ; stack exec -- runhaskell $(ghcextensions) -iserver/src client/GenerateElm.hs)
