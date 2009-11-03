#!/bin/sh
cd `dirname $0`
ERL="erl"
if [ -n "$1" ]; then
	export EWGI_WEBSERVER=$1
	if [ -n "$2" ]; then
		export EWGI_WEBSERVER_PORT=$2
		OS=`uname`;
		if [ "$OS" = "Linux" ] &&  [ "0$EWGI_WEBSERVER_PORT" -lt 1024 ]
		then
			ERL="authbind --deep $ERL"
		fi
	fi
	exec $ERL -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s ewgi_examples
else
	echo "Missing webserver name as parameter (mochiweb|inets|yaws)!"
fi
