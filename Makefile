# A basic Makefile for building and installing Batteries Included
# It punts to ocamlbuild for all the heavy lifting.

NAME = batteries

# This is also defined in the VERSION file
VERSION = 1.2.0pre

# Define variables and export them for mkconf.ml
DOCROOT ?= /usr/share/doc/ocaml-batteries
export DOCROOT
BROWSER_COMMAND ?= x-www-browser %s
export BROWSER_COMMAND

OCAMLBUILD ?= ocamlbuild

BATTERIES_NATIVE ?= yes
BATTERIES_NATIVE_SHLIB ?= yes

# What to build
TARGETS = syntax.otarget byte.otarget src/batteries_help.cmo META
TEST_TARGETS = testsuite/main.byte

ifeq ($(BATTERIES_NATIVE_SHLIB), yes)
  TARGETS += shared.otarget
  TEST_TARGETS += testsuite/main.native
else ifeq ($(BATTERIES_NATIVE), yes)
  TARGETS += native.otarget
  TEST_TARGETS += testsuite/main.native
endif

.PHONY: all clean doc install uninstall reinstall test

all:
	test ! -e src/batteries_config.ml || rm src/batteries_config.ml
	$(OCAMLBUILD) $(TARGETS)

clean:
	rm -f apidocs
	$(OCAMLBUILD) -clean

doc:
	$(OCAMLBUILD) batteries.docdir/index.html
	test -e apidocs || ln -s _build/batteries.docdir apidocs

install: all
	ocamlfind install estring libs/estring/META \
		_build/libs/estring/*.cmo \
		_build/libs/estring/*.cmi \
		_build/libs/estring/*.mli
	ocamlfind install $(NAME) _build/META \
		 _build/src/*.cma _build/src/*.cmxa _build/src/*.cmxs \
		 _build/src/*.a battop.ml \
		_build/src/*.cmx _build/src/*.cmi _build/src/*.mli \
		_build/src/batteries_help.cmo \
		_build/src/syntax/pa_comprehension/pa_comprehension.cmo \
		_build/src/syntax/pa_strings/pa_strings.cma

uninstall:
	ocamlfind remove estring
	ocamlfind remove $(NAME)
	rm -rf $(DOCROOT)

install-doc: doc
	mkdir -p $(DOCROOT)
	cp apidocs/* $(DOCROOT)

reinstall:
	$(MAKE) uninstall
	$(MAKE) install

test: 
	$(OCAMLBUILD) $(TARGETS) $(TEST_TARGETS)
	$(foreach TEST, $(TEST_TARGETS), _build/$(TEST); )