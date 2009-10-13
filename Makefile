VSN		:= 0.01
ERL		?= erl
APP		:= ewgi_examples

all: erl ebin/$(APP).app

erl: ebin
	@./support/compile.erl ebin src/$(APP).app $(EBIN_DIRS)

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

clean:
	@echo "removing:"
	@rm -fv ebin/*.beam ebin/*.app

ebin/$(APP).app: src/$(APP).app Makefile
	@./support/gen_app_file.erl src/$(APP).app $@ $(VSN)

ebin:
	@mkdir ebin