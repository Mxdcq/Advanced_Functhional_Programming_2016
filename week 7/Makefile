all:
	happy -gca ParE.y
	alex -g LexE.x
	ghc --make TestE.hs -o TestE

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocE.* LexE.* ParE.* LayoutE.* SkelE.* PrintE.* TestE.* AbsE.* TestE ErrM.* SharedString.* ComposOp.* e3.dtd XMLE.* Makefile*
	

