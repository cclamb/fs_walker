#!/bin/bash
erl -pa ../ebin/  -boot start_sasl -config sasl_log \
    -s fsw_toplevel start_test -s init stop  -fswtest logfile "$1" -fswtest directory "$2"




