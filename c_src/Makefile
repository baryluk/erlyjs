SPIDERMONKEY_DIR=spidermonkey-1.6


all: spidermonkey erlyjs

spidermonkey:
	(cd $(SPIDERMONKEY_DIR) && make -f Makefile.ref)
	
erlyjs:
	(cd erlyjs && make)
	
clean:
	(cd $(SPIDERMONKEY_DIR) && make clean )
	(cd erlyjs && make clean)