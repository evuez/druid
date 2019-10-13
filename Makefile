SOURCES := $(wildcard */*.hs)

.PHONY: format
format: $(SOURCES)

.PHONY: $(SOURCES)
$(SOURCES):
	hindent $@

.PHONY: test
test:
	stack test --fast

.PHONY: test.watch
test.watch:
	stack test druid:druid-test --file-watch --fast
