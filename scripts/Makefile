ifndef DOCKER_TAG
override DOCKER_TAG=v`date '+%Y%m%d'`
endif

docker:
	docker -D --log-level debug build --progress plain -f Dockerfile -t ghcr.io/jingtaozf/literate-lisp/cl-base:$(DOCKER_TAG) .
