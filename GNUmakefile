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
	stack install ccweb --local-bin-path $@

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
	stack install ccweb --local-bin-path bin
DIFF := git --no-pager diff --no-index
test_cctangle: bin/cctangle
	@rm -f tests/out/tangle*
	stack exec cctangle -- -n tests/tangle.org >/dev/null
	stack exec cctangle -- -IOn tests/tangle.org >/dev/null
	stack exec cctangle -- -IOn ccweb.org >/dev/null
	stack exec cctangle -- tests/tangle.org >/dev/null
	for out in $(shell stack exec cctangle -- -Oq tests/tangle.org); do \
		echo "$$out..." && ${DIFF} $${out/out/ref} $$out ; \
	done
	@rm -f tests/out/tangle*

test_ccweave: bin/ccweave
	@rm -f tests/ref/weave*
	stack exec ccweave -- tests/weave001.org
