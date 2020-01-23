test:
	php -d xdebug.remote_enable=on \
	-d xdebug.remote_host=127.0.0.1 \
	-d xdebug.remote_port=9000 \
	-d xdebug.remote_handler=dbgp \
	-d xdebug.idekey=geben \
	-d xdebug.remote_autostart=On \
	./manual.php

test-uni:
	php -d xdebug.remote_enable=on \
	-d xdebug.remote_host=127.0.0.1 \
	-d xdebug.remote_port=9000 \
	-d xdebug.remote_handler=dbgp \
	-d xdebug.idekey=geben \
	-d xdebug.remote_autostart=On \
	./manual-bad-unicode.php
