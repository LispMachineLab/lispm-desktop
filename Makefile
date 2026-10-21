EMACS ?= emacs
# lisp/ of lispm-browser, the browser the desktop draws: a checkout beside
# this one by default, as lispm-desktop-deps.el looks for it.
LISPM_BROWSER ?= ../lispm-browser/lisp

.PHONY: test compile i18n native native-test

# Tests stub a few primitives; the code they test runs uncompiled, so the
# native compiler need not build trampolines for them, which takes seconds.
test:
	$(EMACS) -Q --batch --eval '(setq native-comp-enable-subr-trampolines nil)' \
	  -L lisp -L $(LISPM_BROWSER) -l test/lispm-desktop-test.el -f ert-run-tests-batch-and-exit

# The messages each catalog of i18n/ lacks, and those it holds that the code
# no longer gives; the tests fail on either.
i18n:
	$(EMACS) -Q --batch --eval '(setq native-comp-enable-subr-trampolines nil)' \
	  -L lisp -L $(LISPM_BROWSER) -l test/lispm-desktop-test.el -f lispm-desktop-test-report-catalogs

# A check: the .elc files are removed again, as they would shadow the
# sources of a checkout Emacs loads from.
compile:
	$(EMACS) -Q --batch --eval '(package-initialize)' -L lisp -L $(LISPM_BROWSER) \
	  -f batch-byte-compile lisp/*.el; \
	status=$$?; rm -f lisp/*.elc $(LISPM_BROWSER)/*.elc; exit $$status

# lispm-desktop-term's module: terminals emulated off Emacs's thread, in Rust.
# lispm-desktop builds it itself at the first start where Cargo is, and again
# at a start that finds its sources changed.
native:
	cd native/lispm-desktop-term && cargo build --release

# The module's own tests, which need no Emacs.
native-test:
	cd native/lispm-desktop-term && cargo test --locked
