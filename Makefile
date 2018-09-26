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
	@stack --docker --docker-stack-exe=image --no-nix build
	@stack --docker --docker-stack-exe=image --no-nix install --local-bin-path .
	@docker build -t utky/healths:dev -f Dockerfile.local .
	@rm ./healths
