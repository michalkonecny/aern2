./distributed-pilot localhost 10501 >& log.10501 &
./distributed-pilot localhost 10502 >& log.10502 &
./distributed-pilot localhost 10503 >& log.10503 &
multitail log.10501 log.10502 log.10503
