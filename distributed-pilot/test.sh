./distributed-pilot localhost 10601 >& log.1 &
./distributed-pilot localhost 10602 >& log.2 &
./distributed-pilot localhost 10603 >& log.3 &
multitail log.1 log.2 log.3
