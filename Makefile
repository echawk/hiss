# hiss: KISS package manager in Haskell.
#
# Built directly with ghc (no cabal, no --make).  Only packages bundled with
# GHC are used: base, directory, and process.
#
# Each module is compiled separately with `ghc -c`; the executable is then
# produced by an explicit link step.  To debug linking:
#
#   make GHC_LDFLAGS=-v3          # show ghc's full linker invocation
#   make GHC_LDFLAGS='-pgml cc'   # choose the C compiler/linker ghc drives
#   make link-command         # print the link command without running it

GHC      ?= ghc
HCFLAGS  ?= -O1 -Wall -Wno-name-shadowing -Wno-unused-do-bind
GHC_LDFLAGS ?=
PACKAGES  = -hide-all-packages -package base -package directory -package process
BUILDDIR  = build
PREFIX   ?= /usr/local

HC = $(GHC) -XHaskell2010 $(PACKAGES) -i. -odir $(BUILDDIR) -hidir $(BUILDDIR)

MODULES = Kiss/Util Kiss/FS Kiss/Env Kiss/Glob Kiss/OS Kiss/File Kiss/Hook \
          Kiss/Search Kiss/Package Kiss/Manifest Kiss/Alternatives \
          Kiss/Install Kiss/Source Kiss/Build Kiss/Update Kiss Main
OBJECTS = $(MODULES:%=$(BUILDDIR)/%.o)

all: hiss

# Link.
hiss: $(OBJECTS)
	$(GHC) $(PACKAGES) $(GHC_LDFLAGS) -o $@ $(OBJECTS)

link-command:
	@echo $(GHC) $(PACKAGES) $(GHC_LDFLAGS) -o hiss $(OBJECTS)

# Compile one module.  Interface (.hi) files are written beside the objects;
# depend.mk orders modules by their imports.
$(BUILDDIR)/%.o: %.hs
	@mkdir -p $(@D)
	$(HC) $(HCFLAGS) -c $< -o $@

$(BUILDDIR)/%.hi: $(BUILDDIR)/%.o
	@:

-include depend.mk

# Regenerate depend.mk after changing imports.
depend:
	$(HC) -M -dep-makefile depend.mk Main.hs
	sed -i.bak -e '/^#/d' -e '/^$$/d' depend.mk && rm -f depend.mk.bak

# Differential conformance against the reference kiss 6.2.0, using the suite
# in the kiss-spec submodule.  REFERENCE may name an existing copy.
KISS_URL    = https://codeberg.org/kiss-community/kiss/raw/tag/6.2.0/kiss
KISS_SHA256 = 9874e2090290372156f5840c6b7b52d2101e3dae29e22b042831d1c6163633e4
REFERENCE  ?= $(BUILDDIR)/reference/kiss-ref

check: hiss $(REFERENCE) kiss-spec/conformance/run.sh
	LC_ALL=C ./kiss-spec/conformance/run.sh $(REFERENCE) ./hiss $(CASES)

kiss-spec/conformance/run.sh:
	git submodule update --init kiss-spec

$(BUILDDIR)/reference/kiss:
	@mkdir -p $(@D)
	curl -fsSL -o $@.tmp $(KISS_URL)
	echo '$(KISS_SHA256)  $@.tmp' | shasum -a 256 -c -
	chmod 755 $@.tmp && mv $@.tmp $@

# The reference expects GNU/busybox `wc`, which prints counts unpadded; BSD
# `wc` pads them, which breaks its removal check.  Wrap it when needed.
$(BUILDDIR)/reference/kiss-ref: $(BUILDDIR)/reference/kiss
	@mkdir -p $(BUILDDIR)/reference/bin
	printf '#!/bin/sh\n/usr/bin/wc "$$@" | sed "s/^ *//"\n' > $(BUILDDIR)/reference/bin/wc
	chmod 755 $(BUILDDIR)/reference/bin/wc
	printf '#!/bin/sh\nPATH="%s/bin:$$PATH" exec "%s/kiss" "$$@"\n' \
		"$(CURDIR)/$(BUILDDIR)/reference" "$(CURDIR)/$(BUILDDIR)/reference" > $@
	chmod 755 $@

install: hiss
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	cp hiss $(DESTDIR)$(PREFIX)/bin/hiss

clean:
	rm -rf $(BUILDDIR) hiss

.PHONY: all check clean depend install link-command
