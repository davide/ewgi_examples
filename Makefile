VSN		:= 0.01
ERL		?= erl
APP		:= ewgi_examples

all: erl ebin/$(APP).app

erl: ebin
	(cd src;$(MAKE))
	@touch src/$(APP).app

clean:
	(cd src;$(MAKE) clean)

ebin/$(APP).app: src/$(APP).app Makefile
	@./support/gen_app_file.erl src/$(APP).app $@ $(VSN)

ebin:
	@mkdir ebin