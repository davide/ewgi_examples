#!/bin/sh
cd `dirname $0`
ERL="erl"
if [ -n "$1" ]; then
	CONFIG=$1
	OS=`uname`;
	if [ "$OS" = "Linux" ]
	then
		ERL="authbind --deep $ERL"
	fi
	shift
	until [ -z "$1" ]
	do
		ERL="$ERL $1"
		shift
	done
	exec $ERL -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s ewgi_examples -config config/$CONFIG.config
else
	echo "Missing config_name as parameter!"
fi
