GHC=ghc6

all:
	if [ ! -d out ]; then mkdir out; fi
	$(GHC) -odir out -hidir out -iout -i../../.. --make Main.hs
	$(GHC) -odir out -hidir out -iout -c Page.hs -o Page.o
	$(GHC) -odir out -hidir out -iout -c Sub/Page.hs -o Sub/Page.o

clean:
	rm -rf out
	rm -f Page.o
	rm -f Sub/Page.o
	rm -f a.out
