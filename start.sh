#!/bin/sh
cd `dirname $0`
ERL="erl"
if [ -n "$1" ]; then
	export EWGI_WEBSERVER=$1
	shift
	if [ -n "$1" ]; then
		export EWGI_WEBSERVER_PORT=$1
		OS=`uname`;
		if [ "$OS" = "Linux" ] &&  [ "0$EWGI_WEBSERVER_PORT" -lt 1024 ]
		then
			ERL="authbind --deep $ERL"
		fi
	fi
	shift
	until [ -z "$1" ]
	do
		ERL="$ERL $1"
		shift
	done
	exec $ERL -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s ewgi_examples
else
	echo "Missing webserver name as parameter (mochiweb|inets|yaws)!"
fi
