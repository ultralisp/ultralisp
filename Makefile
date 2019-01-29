TAG := '40ants/ultralisp:0.1.7'

build: qlfile.lock
	docker build -t $(TAG) .

push:
	docker push $(TAG)

qlfile.lock: qlfile
	qlot update

deps: qlfile.lock
	# Here you need to install gen-deps-system using this command
	# ros install 40ants/gen-deps-system
	qlot exec gen-deps-system --except ultralisp app


.PHONY: build push
