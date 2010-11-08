#!/bin/bash
# $1 is the directory
# clients is still specified in ebin/fswalker.app
#

/bin/rm -f *.out fsw.log fsw_sasl.log
#erlc -o ../ebin *.erl

#erl -pa ../ebin/ -noshell -boot start_sasl -config sasl_log \
#    -s fsw_toplevel start  -s init stop -fswalker logfile "$1" -fswalker directory \"$2\"
#erl -pa ../ebin/ -sname worker

erl -pa ../ebin/  -sname master -boot start_sasl -config fsw -s fsw_toplevel start  # -fs_server directory \"$1\" 

