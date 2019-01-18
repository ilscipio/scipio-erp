# Docker Image build
NAME		:= lscipio/scipio-erp-devspaces
TAG			:= 0.1
IMG			:= ${NAME}:${TAG}
LATEST		:= ${NAME}:latest

build:
	docker build -t ${IMG} docker/
	docker tag ${IMG} ${LATEST}

rebuild:
	docker build --no-cache -t ${IMG} docker/
	docker tag ${IMG} ${LATEST}

exec:
	docker run -it ${IMG} /bin/bash

.DEFAULT_GOAL := build
.PHONY : build
