.PHONY: all clean test run build-image
all:
	stack build

test:
	stack test

clean:
	stack clean

run:
	stack exec healths

# Multi stage build
build-image:
	@docker build -t utky/healths:latest .
