#!/bin/bash
# $1 is the directory
# clients is still specified in ebin/fswalker.app
#

/bin/rm -f client*.out fsw.log fsw_sasl.log
erlc -o ../ebin *.erl

#erl -pa ../ebin/ -noshell -boot start_sasl -config sasl_log \
#    -s fsw_toplevel start  -s init stop -fswalker logfile "$1" -fswalker directory \"$2\"

erl -pa ../ebin/ -noshell -boot start_sasl -config sasl_log -s fsw_toplevel start  -fswalker directory \"$1\"

