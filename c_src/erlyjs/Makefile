PRIV=../../priv
PLATFORM_UNAME = $(shell uname)

NAME=erlyjs
SPIDERMONKEY_DIR=../spidermonkey-1.6
PRIV_LIB_DIR=$(PRIV)/lib
PRIV_BIN_DIR=$(PRIV)/bin

ifeq ($(PLATFORM_UNAME), Darwin)
 	PLATFORM=XP_UNIX
	OTP=/Users/rsaccon/R11B
	ERL_INTERFACE=$(OTP)/erlang/lib/erl_interface-3.5.5.3
	COMPILER=gcc
	LIBS=-ljs -lerl_interface -lei -lpthread -lm
	INCLUDEDIRS=-I$(SPIDERMONKEY_DIR) -I$(SPIDERMONKEY_DIR)/Darwin_DBG.OBJ -I$(ERL_INTERFACE)/include
 	CC=$(COMPILER) $(INCLUDEDIRS) 
	LIBDIRS=-L/usr/lib -L$(SPIDERMONKEY_DIR)/Darwin_DBG.OBJ -L$(ERL_INTERFACE)/darwin-x86/lib
else
  ifeq ($(PLATFORM_UNAME), Linux)
    # todo: everything for Linux
    OTP=/opt/cean
    ERL_INTERFACE=$(OTP)/erlang/lib/erl_interface-3.5.5.3
    COMPILER=gcc
    LIBS=-lcairo -lpng -lerl_interface -lei -lnsl -lpthread  
    INCLUDEDIRS=-I/usr/include/cairo -I$(OTP)/erlang/include/erl_interface
    CC=$(COMPILER) $(INCLUDEDIRS)
    LIBDIRS=-L/usr/lib  -L$(ERL_INTERFACE)/linux-x86/lib -L/usr/lib/happycoders
  endif
endif


all:
	$(CC) $(NAME).c -D$(PLATFORM) -o $(PRIV_BIN_DIR)/$(NAME) $(LIBDIRS) $(LIBS)
	rm -f *.o
			
clean:
	rm -f $(PRIV_BIN_DIR)/$(NAME)