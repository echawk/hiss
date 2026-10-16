# hiss: KISS package manager in Haskell.
#
# Built directly with ghc (no cabal).  Only packages bundled with GHC are
# used: base, directory, and process.

GHC      ?= ghc
GHCFLAGS ?= -O1 -Wall -Wno-name-shadowing -Wno-unused-do-bind
PACKAGES  = -hide-all-packages -package base -package directory -package process
BUILDDIR  = build
PREFIX   ?= /usr/local

SOURCES = Main.hs Kiss.hs $(wildcard Kiss/*.hs)

all: hiss

hiss: $(SOURCES)
	$(GHC) --make -XHaskell2010 $(PACKAGES) $(GHCFLAGS) \
		-outputdir $(BUILDDIR) -o $@ Main.hs

check: hiss
	../kiss-spec/conformance/run.sh ../kiss-spec/kiss/kiss ./hiss

install: hiss
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	cp hiss $(DESTDIR)$(PREFIX)/bin/hiss

clean:
	rm -rf $(BUILDDIR) hiss

.PHONY: all check install clean
