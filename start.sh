#!/bin/sh
cd `dirname $0`
if [ -n "$1" ]; then
	export EWGI_WEBSERVER=$1
	exec erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s ewgi_examples
else
	echo "Missing webserver name as parameter (mochiweb|inets|yaws)!"
fi
