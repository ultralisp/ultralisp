TAG := '40ants/ultralisp'

all:
	docker build -t $(TAG) .

qlfile.lock: qlfile
	qlot update

deps: qlfile.lock
	# Here you need to install gen-deps-system using this command
	# ros install 40ants/gen-deps-system
	qlot exec gen-deps-system --except ultralisp app

