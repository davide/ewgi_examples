#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -eval 'application:start(crypto)' -eval 'application:start(inets)' \
-eval 'application:set_env(ewgi, app_module, ewgi_examples_web)' \
-eval 'application:set_env(ewgi, app_function, dispatcher)' \
-eval 'inets:start(httpd, [{server_name, "ewgi"}, {server_root, "."}, {document_root, "."}, {modules, [ewgi_inets]}, {port, 8000}])'
