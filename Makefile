EMACS ?= emacs
# lisp/ of lispm-browser, the browser the desktop draws: a checkout beside
# this one by default, as lispm-desktop-deps.el looks for it.
LISPM_BROWSER ?= ../lispm-browser/lisp
# lisp/ of lispm-agent, whose editor's agents the desktop draws.
LISPM_AGENT ?= ../lispm-agent/lisp
# lispm-stickies, whose notes the desktop shows: with a checkout beside this
# one, the tests of Stickies on the desktop run too.
LISPM_STICKIES ?= ../lispm-stickies
STICKIES = $(if $(wildcard $(LISPM_STICKIES)/lispm-stickies.el),-L $(LISPM_STICKIES))

.PHONY: test compile i18n native native-test pin

# Tests stub a few primitives; the code they test runs uncompiled, so the
# native compiler need not build trampolines for them, which takes seconds.
# The terminals' tests use the module `make native' built, if any.
test:
	$(EMACS) -Q --batch --eval '(setq native-comp-enable-subr-trampolines nil)' \
	  --eval "(setq lispm-desktop-term-module-source 'build)" \
	  -L lisp -L $(LISPM_BROWSER) -L $(LISPM_AGENT) $(STICKIES) -l test/lispm-desktop-test.el -f ert-run-tests-batch-and-exit

# The messages each catalog of i18n/ lacks, and those it holds that the code
# no longer gives; the tests fail on either.
i18n:
	$(EMACS) -Q --batch --eval '(setq native-comp-enable-subr-trampolines nil)' \
	  -L lisp -L $(LISPM_BROWSER) -L $(LISPM_AGENT) -l test/lispm-desktop-test.el -f lispm-desktop-test-report-catalogs

# A check: the .elc files are removed again, as they would shadow the
# sources of a checkout Emacs loads from.
compile:
	$(EMACS) -Q --batch --eval '(package-initialize)' -L lisp -L $(LISPM_BROWSER) -L $(LISPM_AGENT) \
	  -f batch-byte-compile lisp/*.el; \
	status=$$?; rm -f lisp/*.elc $(LISPM_BROWSER)/*.elc $(LISPM_AGENT)/*.elc; exit $$status

# lispm-desktop-term's module: terminals emulated off Emacs's thread, in Rust.
# lispm-desktop downloads it, checked, unless lispm-desktop-term-module-source
# is `build': then it builds it itself at the first start, and again at a
# start that finds its sources changed.
native:
	cd native/lispm-desktop-term && cargo build --release

# The module's own tests, which need no Emacs.
native-test:
	cd native/lispm-desktop-term && cargo test --locked

# The SHA-256 of the modules the terminal module workflow published for the
# sources, written into lisp/lispm-desktop-term-sums.eld: a release of LispM
# pins its own sources, so its downloads are checked against them.
pin:
	EMACS=$(EMACS) python3 scripts/pin-term-module.py
