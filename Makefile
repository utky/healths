.PHONY: all clean test
all:
	stack build

test:
	stack test

clean:
	stack clean
