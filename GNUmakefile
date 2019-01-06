all: bin/cctangle bin/ccweave
test: test_cctangle

BOOTSTRAP_TANGLER := bootstrap/cctangle

SOURCE_FILES := $(shell ${BOOTSTRAP_TANGLER} -Inq ccweb.org)
OUTPUT_FILES := $(shell ${BOOTSTRAP_TANGLER} -Onq ccweb.org)
inotify:
	while inotifywait -e modify ${SOURCE_FILES}; do \
	make -j test; \
	done
define STACK_PREAMBLE
#!/usr/bin/env stack
{- stack
  script
  --resolver lts-12.22
  --package containers
  --package hostname
  --package mtl
  --package filepath
  --package directory
  --package unix
  --package pretty
  --package optparse-applicative
  --package parsec
-}
endef

export STACK_PREAMBLE

bootstrap: ${BOOTSTRAP_TANGLER}

bootstrap/%: bin/%.hs test_%
	@mkdir -p bootstrap
	@rm -f $@
	@echo "$$STACK_PREAMBLE" > $@
	@cat $< >> $@
	@chmod 555 $@
clean:
	rm -f $(filter-out GNUmakefile,${OUTPUT_FILES})

${OUTPUT_FILES}: ccweb.org
	@mkdir -p bin
	@rm -f $@
	${BOOTSTRAP_TANGLER} $<
bin/%: bin/%.hs
	@rm -f $@
	stack build --trace $(subst bin/,ccweb:,$@)
	cp `stack exec -- which $(subst bin/,,$@)` $@
DIFF := git --no-pager diff --no-index

test_cctangle: bin/cctangle
	@rm -f tests/out/tangle*
	stack exec cctangle -- -nvvvv tests/tangle.org >/dev/null
	stack exec cctangle -- -IOnvvvv tests/tangle.org >/dev/null
	stack exec cctangle -- -IOnvvvv ccweb.org >/dev/null
	stack exec cctangle -- tests/tangle.org >/dev/null
	${DIFF} tests/ref/tangle001.hs tests/out/tangle001.hs
	${DIFF} tests/ref/tangle002.hs tests/out/tangle002.hs
	${DIFF} tests/ref/tangle003.hs tests/out/tangle003.hs
	${DIFF} tests/ref/tangle004.hs tests/out/tangle004.hs
	${DIFF} tests/ref/tangle005.hs tests/out/tangle005.hs
	@rm -f tests/out/tangle*

test_ccweave: bin/ccweave
	@rm -f tests/ref/weave*
	stack exec ccweave -- tests/weave001.org