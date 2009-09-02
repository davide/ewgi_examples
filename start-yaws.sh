#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin/ $PWD/deps/*/ebin -boot start_sasl -eval 'filelib:ensure_dir("./log/").' -yaws debug -run yaws -yaws id ewgi_examples -conf priv/yaws.conf
	
