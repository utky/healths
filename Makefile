.PHONY: all clean test run
all:
	stack build

test:
	stack test

clean:
	stack clean

run:
	stack exec healths
