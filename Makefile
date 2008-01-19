ERL=/Users/rsaccon/R11B/start.sh
#ERL=/usr/local/erlware/bin/erl
#ERL=erl

all:
	$(ERL) -make
	
cnode:
	( cd c_src && $(MAKE) )

run:
	$(ERL) -pa `pwd`/ebin \
	-s erlyjs \
	-sname erlyjs
	
clean:
	rm -fv ebin/*.beam
	rm -fv erl_crash.dump
	
clean-cnode:
	( cd c_src && $(MAKE) clean )