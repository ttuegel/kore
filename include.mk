# Settings

TOP ?= $(shell git rev-parse --show-toplevel)
export TOP  # so that sub-makes do not invoke git again
UPSTREAM_BRANCH = origin/master

BUILD_DIR = $(TOP)/.build
K_NIGHTLY = $(BUILD_DIR)/nightly.tar.gz
K_NIGHTLY_URL = https://github.com/kframework/k/releases/download/nightly-eb4711a9a/nightly.tar.gz
K_DIST_DEFAULT = $(BUILD_DIR)/k
K_DIST ?= $(K_DIST_DEFAULT)
K_DIST_BIN = $(K_DIST)/bin
K_DIST_LIB = $(K_DIST)/lib
K_REPO = 'https://github.com/kframework/k'

# The kernel JAR is used as a build timestamp.
K = $(K_DIST_LIB)/java/kernel-1.0-SNAPSHOT.jar
KOMPILE = $(K_DIST_BIN)/kompile
KRUN = $(K_DIST_BIN)/krun
KPROVE = $(K_DIST_BIN)/kprove

KOMPILE_OPTS = --backend haskell
KRUN_OPTS = --haskell-backend-command "$(KORE_EXEC) $(KORE_EXEC_OPTS)"
KPROVE_OPTS = --haskell-backend-command "$(KORE_EXEC) $(KORE_EXEC_OPTS)"
KPROVE_REPL_OPTS = --haskell-backend-command "$(KORE_REPL) $(KORE_EXEC_OPTS)"

HS_TOP = $(TOP)/kore
HS_SOURCE_DIRS = $(HS_TOP)/src $(HS_TOP)/app $(HS_TOP)/test $(HS_TOP)/bench
STACK_NO_PROFILE = --no-library-profiling --no-executable-profiling
STACK_FAST = --fast
STACK_COVERAGE = --coverage
STACK_BUILD = build --pedantic

STACK = stack
STACK_HADDOCK = $(STACK) --work-dir=.stack-work-haddock
STACK_TEST = $(STACK) --work-dir=.stack-work-test

STACK_LOCAL_INSTALL_ROOT ?= $(shell $(STACK) path --local-install-root)
STACK_LOCAL_DOC_ROOT ?= $(shell $(STACK_HADDOCK) path --local-doc-root)
STACK_LOCAL_HPC_ROOT ?= $(shell $(STACK_TEST) path --local-hpc-root)
export STACK_LOCAL_INSTALL_ROOT  # so that sub-makes do not invoke stack again
export STACK_LOCAL_DOC_ROOT  # so that sub-makes do not invoke stack again
export STACK_LOCAL_HPC_ROOT  # so that sub-makes do not invoke stack again

KORE_EXEC = $(STACK_LOCAL_INSTALL_ROOT)/bin/kore-exec
KORE_EXEC_OPTS =

KORE_REPL = $(STACK_LOCAL_INSTALL_ROOT)/bin/kore-repl

$(KORE_EXEC):
	$(STACK) $(STACK_BUILD) $(STACK_NO_PROFILE) kore:exe:kore-exec

$(KORE_REPL):
	$(STACK) $(STACK_BUILD) $(STACK_NO_PROFILE) kore:exe:kore-repl
