GHC=ghc

all:
	if [ ! -d out ]; then mkdir out; fi
	if [ ! -d out/DynamicLoader ]; then mkdir out/DynamicLoader; fi
	$(GHC) -odir out -hidir out -iout -i../../.. --make Main.hs
	$(GHC) -hidir out -iout -c plugins/Upper.hs -o plugins/Upper.plugin

clean:
	rm -rf out
	rm -f plugins/Upper.plugin
	rm -f a.out
