ERL=/Users/rsaccon/R11B/start.sh
#ERL=/usr/local/erlware/bin/erl
#ERL=erl
APP_NAME=erlyjs_demo
NODE_NAME=erlyjs

all:
	$(ERL) -make
	
cnode:
	( cd c_src && $(MAKE) )

run:
	$(ERL) -pa `pwd`/ebin \
	-s $(APP_NAME) \
	-sname $(NODE_NAME)
	
clean:
	rm -fv ebin/*
	rm -fv erl_crash.dump
	
clean-cnode:
	( cd c_src && $(MAKE) clean )