all : cbt-bot.out.md

.PHONY: build
build :
	stack build

.PHONY: test
test : build
	stack test

.PHONY: exec
exec : test
	stack exec slack-cbt


mybib.out.md : mybib.md mybib.bib Makefile
	pandoc -t markdown-raw_html-citations-native_divs-native_spans --filter=pandoc-citeproc --standalone mybib.md -o $@

%.out.md : %.md %.bib Makefile
	pandoc -t markdown-raw_html-citations-native_divs-native_spans --filter=pandoc-citeproc --standalone $< -o $@
