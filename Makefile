
compile: clean
	@mkdir ebin
	@erlc -o ebin src/*.erl
	@$(MAKE) script

script:
	@echo "erl -noshell -pa ebin -eval \"rev_erlang:start().\" -s init stop" > rev-erlang.sh
	@chmod +x rev-erlang.sh

docs: clean-docs
	@erl -noshell -run edoc_run files '["src/rev_erlang.erl", \
	                                    "src/fwd_sem.erl", \
	                                    "src/bwd_sem.erl", \
	                                    "src/utils.erl"]' '[{dir, "docs"}]'

clean: clean-docs
	@rm -Rf ebin

clean-docs:
	@rm -Rf docs

debug: clean
	@mkdir ebin
	@erlc -Ddebug -o ebin src/*.erl
	@$(MAKE) script
