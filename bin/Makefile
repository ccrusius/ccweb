all: ccweb.pdf

cctangle.lua ccweave.lua: ccweb.ccw bootstrap.lua
	lua bootstrap.lua ccweb.ccw

ccweb.tex: ccweave.lua
	lua ccweave.lua ccweb.ccw > $@

ccweb.pdf: ccweb.tex
	pdftex ccweb
