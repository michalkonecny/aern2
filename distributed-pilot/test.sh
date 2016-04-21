#!/bin/bash

host0=localhost
host1=localhost
host2=localhost
#host0=dev
#host1=192.168.1.125
#host2=192.168.1.125

ssh $host0 killall distributed-pilot >& /dev/null
ssh $host1 killall distributed-pilot >& /dev/null
ssh $host2 killall distributed-pilot >& /dev/null

rsync --copy-links distributed-pilot $host0:/tmp/distributed-pilot
rsync --copy-links distributed-pilot $host1:/tmp/distributed-pilot
rsync --copy-links distributed-pilot $host2:/tmp/distributed-pilot

ssh $host0 /tmp/distributed-pilot $host1 10600 >& log.0 &
ssh $host1 /tmp/distributed-pilot $host1 10601 >& log.1 &
ssh $host2 /tmp/distributed-pilot $host1 10602 >& log.2 &

multitail log.0 log.1 log.2
