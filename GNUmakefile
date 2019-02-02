all: bin/cctangle bin/ccweave
test: test_cctangle test_ccweave

clean:
	git clean -x -f
inotify:
	while inotifywait -e modify $$(bootstrap/cctangle -Inq ccweb.org) tests/*.org; do \
	make -j test; \
	done
bootstrap:
	@mkdir -p bootstrap
	@rm -f bootstrap/cctangle
	stack build --trace ccweb:cctangle
	cp `stack exec -- which cctangle` $@

.PHONY: bootstrap
TANGLED_FILES :=
-include ccweb.outs

ccweb.outs:
	@rm -f $@
	test -x bootstrap/cctangle && (bootstrap/cctangle -Onq ccweb.org | sed -e 's/^/TANGLED_FILES += /' > $@)
-include ccweb.deps

ccweb.deps:
	@rm -f $@
	test -x bootstrap/cctangle && (bootstrap/cctangle -Mnq ccweb.org | sed -e 's/^/ccweb.outs ccweb.deps /' > $@)
${TANGLED_FILES}:
	@mkdir -p $(dir $@)
	@rm -f $@
	bootstrap/cctangle ccweb.org
bin/%tangle bin/%weave: bin/cctangle.hs bin/ccweave.hs
	@rm -f $@
	stack build -j2 --trace
	cp `stack exec -- which ccweave` bin/ccweave
	cp `stack exec -- which cctangle` bin/cctangle
DIFF := git --no-pager diff --no-index
test_cctangle: bin/cctangle
	@rm -f tests/out/tangle*
	stack exec cctangle -- -n tests/tangle.org >/dev/null
	stack exec cctangle -- -IOn tests/tangle.org >/dev/null
	stack exec cctangle -- -IOn ccweb.org >/dev/null
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
