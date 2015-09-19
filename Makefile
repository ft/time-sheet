RUNTESTS = run-tests -strip-roots -dispatch-root "$$PWD/tests"

BYTECOMPILE = sh ./utils/compile
INSTALL = sh ./utils/install

all:
	@echo
	@echo " Available targets:"
	@echo
	@echo "           all: This help text"
	@echo "           doc: Generate library documentation"
	@echo "  byte-compile: Byte-compile the client library"
	@echo "    byte-clean: Remove byte-compiled scheme code"
	@echo "       install: Install the project to the host system"
	@echo "          test: Run test suite"
	@echo "  test-verbose: Run test suite (with verbose test harness)"
	@echo "    test-debug: Run test suite (With all debugging enabled)"
	@echo

compile:
	$(BYTECOMPILE)

clean:
	find scheme -name '*.go' -exec rm '{}' +
	(cd doc && $(MAKE) clean)

doc:
	(cd doc && $(MAKE))

install:
	$(INSTALL)

test:
	$(RUNTESTS)

test-verbose:
	$(RUNTESTS) -verbose

test-debug:
	$(RUNTESTS) -verbose -dispatch-verbose -debug

.PHONY: all compile clean doc install test test-debug test-verbose
