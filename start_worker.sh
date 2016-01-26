#!/bin/bash

# Start a new digeno worker node on a machine with this script.
# The node will be named 'digeno_worker@HOSTNAME' by default. This can be
# changed by passing a single argument to be used instead of 'digeno_worker'.
# You do not want to do this normally, it's just for testing purposes.

renice 19 $$
HOSTNAME=`hostname --fqdn`

# Default value for the digeno master node.
if [[ -z $MASTER_NODE ]]; then
	MASTER_NODE=digeno_master@`hostname --fqdn`
fi
export MASTER_NODE

# OPTIONAL setting to limit the number of CPU cores used.
# You may also export this in the shell from where this script is launched.
#export CORES=2


# basic sanity check to get an intelligible message instead of an erlang startup error
if [[ `hostname` == $HOSTNAME ]] ; then
    if [[ $HOSTNAME == *.* ]] ; then
	echo "'hostname' returns" `hostname`
	echo "'hostname --fqdn' returns" $HOSTNAME
    else
	echo "Error: 'hostname --fqdn' returns" $HOSTNAME
	echo "Please fix your name resolution!"
	exit 1
    fi
fi


if [[ $# -gt 0 ]] ; then
	NODENAME=$1\@$HOSTNAME
else
	NODENAME=digeno_worker@$HOSTNAME
fi
echo Node name: $NODENAME

erl -smp -env ERL_LIBS .. -name $NODENAME -noshell -setcookie digeno_secret -eval 'digeno_worker:start()'
