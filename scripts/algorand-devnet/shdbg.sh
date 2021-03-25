#!/bin/sh

cat >./shdbg1.sh <<END
#!/bin/bash
printf "HTTP/1.1 200 OK\r\nConnection: close\r\nContent-Length: 4\r\n\r\nOK\r\n"
exec cat <&0 2>&1 >\$EPOCHREALTIME.json
END
chmod +x ./shdbg1.sh

exec nc -p 9392 -s 0.0.0.0 -lk -e ./shdbg1.sh
