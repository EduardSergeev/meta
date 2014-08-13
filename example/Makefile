
BEAMS := *.beam

.PHONY: all clean meta_lib

all: $(BEAMS)

%.beam: %.erl meta_lib
	@echo Compiling $<
	@erlc -pz ../ebin -pa . -I ../include $<

meta_lib:
	@echo Building meta library
	@cd .. && rebar compile

clean:
	@echo Cleaning
	@rm -f *.beam
