OBJECTScasttex = Main.hs TexParser.hs StyleSvg.hs Config.hs
OBJECTSupdatesvg = UpdateSvg.hs StyleSvg.hs
COMPILE= ghc

all: casttex updatesvg

casttex: $(OBJECTScasttex)
	$(COMPILE) -o $@ $<

updatesvg: $(OBJECTSupdatesvg)
	$(COMPILE) -o $@ $<


clean:
	rm -f casttex updatesvg *.hi *.o

