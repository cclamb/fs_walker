#!/bin/bash
/bin/rm -f client*.out fs_walker_info.log fs_walker_log.log 
erlc *.erl
erl -noshell -boot start_sasl -config elog -run fs_walker init "$1" "$2" -run init stop 
