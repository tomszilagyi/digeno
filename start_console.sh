#! /bin/bash

H=`hostname --fqdn`
erl -name "digeno-console@$H" -hidden -setcookie digeno_secret -config digeno.config -remsh digeno_master@$H
