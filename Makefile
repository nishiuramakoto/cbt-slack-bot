.PHONY: build
build :
	stack build

.PHONY: test
test : build
	stack test

.PHONY: exec
exec : test
	stack exec slack-cbt
