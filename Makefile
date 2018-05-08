# Makefile

all: Main

Main: Main.hs PPM.hs PPMIO.hs
	ghc -o Main Main.hs
	rm *.hi *.o

clean:
	rm Main *.hi *.o