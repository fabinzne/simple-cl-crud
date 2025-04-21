# Makefile
.PHONY: all build run clean

run: down up
	sbcl --load run-server.lisp

up:
	podman compose -f compose.yml up -d --force-recreate

down:
	podman compose -f compose.yml down -v --remove-orphans

clean: down
	podman rmi -f lisp-crud
	rm -rf podman/postgres_data