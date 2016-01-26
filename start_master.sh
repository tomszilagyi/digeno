#!/bin/sh
H=`hostname --fqdn`
erl -smp -env ERL_LIBS .. -name "digeno_master@$H" -setcookie digeno_secret -noshell -config digeno.config -eval 'application:start(digeno)'
