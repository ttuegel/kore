ifeq ($(origin TOP), undefined)
	TOP = $(shell git rev-parse --show-toplevel)
endif

include $(TOP)/include.mk

DEF_DIR ?= .
TEST_DIR ?= .

DEF ?= test
EXT ?= $(DEF)
KPROVE_MODULE ?= VERIFICATION

DIFF = diff -u
FAILED = ( mv $@ $@.fail && false )
FAILED_STORE_PROOFS = ( mv $(STORE_PROOFS) $(STORE_PROOFS).fail && mv $@ $@.fail && false )

KOMPILED := $(TEST_DIR)/$(DEF)-kompiled
DEF_KORE_DEFAULT = $(KOMPILED)/definition.kore
DEF_KORE ?= $(DEF_KORE_DEFAULT)
TEST_DEPS = $(K) $(DEF_KORE) $(KORE_EXEC)

TESTS = \
	$(wildcard $(TEST_DIR)/*.$(EXT)) \
	$(wildcard $(TEST_DIR)/*-spec.k) \
	$(wildcard $(TEST_DIR)/*.merge) \
	$(wildcard $(TEST_DIR)/test-*.sh)

OUTS = $(foreach TEST, $(TESTS), $(TEST).out)

KOMPILE_OPTS += -d $(DEF_DIR)
KRUN_OPTS += -d $(DEF_DIR)
KPROVE_OPTS += -d $(DEF_DIR) -m $(KPROVE_MODULE)
KORE_EXEC_OPTS += \
	$(if $(STORE_PROOFS),\
		--save-proofs $(STORE_PROOFS),\
		$(if $(RECALL_PROOFS),\
			--save-proofs $(@:.out=.save-proofs.kore)\
		)\
	)
KPROVE_REPL_OPTS += -d $(DEF_DIR) -m $(KPROVE_MODULE)
KPROVE_SPEC = $<

$(DEF_KORE_DEFAULT): $(DEF_DIR)/$(DEF).k $(K)
	@echo ">>>" $(CURDIR) "kompile" $<
	rm -fr $(KOMPILED)
	$(KOMPILE) $(KOMPILE_OPTS) $<

# From make 3.82 news: http://cvs.savannah.gnu.org/viewvc/*checkout*/make/make/NEWS?revision=2.120
# * WARNING: Backward-incompatibility!
#   The pattern-specific variables and pattern rules are now applied in the
#   shortest stem first order instead of the definition order (variables
#   and rules with the same stem length are still applied in the definition
#   order). This produces the usually-desired behavior where more specific
#   patterns are preferred. To detect this feature search for 'shortest-stem'
#   in the .FEATURES special variable.

%.golden: DIFF = true
%.golden: %
	cp $< $@

### RUN

%.$(EXT).out: $(TEST_DIR)/%.$(EXT) $(TEST_DEPS)
	@echo ">>>" $(CURDIR) "krun" $<
	rm -f $@
	$(KRUN) $(KRUN_OPTS) $< --output-file $@
	$(DIFF) $@.golden $@ || $(FAILED)

### SEARCH

%.search.final.$(EXT).out: KRUN_OPTS += --search-final

%.search.star.$(EXT).out: KRUN_OPTS += --search-all

%.search.one.$(EXT).out: KRUN_OPTS += --search-one-step

%.search.plus.$(EXT).out: KRUN_OPTS += --search-one-or-more-steps

PATTERN_OPTS = --pattern "$$(cat $*.k)"

%.search-pattern.final.$(EXT).out: KRUN_OPTS += --search-final $(PATTERN_OPTS)

%.search-pattern.star.$(EXT).out: KRUN_OPTS += --search-all $(PATTERN_OPTS)

%.search-pattern.one.$(EXT).out: KRUN_OPTS += --search-one-step $(PATTERN_OPTS)

%.search-pattern.plus.$(EXT).out: \
	KRUN_OPTS += --search-one-or-more-steps $(PATTERN_OPTS)

### PROVE

%-spec.k.out: $(TEST_DIR)/%-spec.k $(TEST_DEPS)
	@echo ">>>" $(CURDIR) "kprove" $<
	rm -f $@
	$(if $(STORE_PROOFS),rm -f $(STORE_PROOFS),$(if $(RECALL_PROOFS),cp $(RECALL_PROOFS) $(@:.out=.save-proofs.kore)))
	$(KPROVE) $(KPROVE_OPTS) $(KPROVE_SPEC) --output-file $@ || true
	$(DIFF) $@.golden $@ || $(FAILED)
	$(if $(STORE_PROOFS),$(DIFF) $(STORE_PROOFS).golden $(STORE_PROOFS) || $(FAILED_STORE_PROOFS))

%-save-proofs-spec.k.out: STORE_PROOFS = $(@:.out=.save-proofs.kore)

%.save-proofs.kore: %.out
	[ -f $@ ]

%-repl-spec.k.out: KPROVE_OPTS = $(KPROVE_REPL_OPTS)

%-repl-script-spec.k.out: %-repl-script-spec.k.repl
%-repl-script-spec.k.out: \
	HASKELL_BACKEND_COMMAND = $(KORE_REPL) -r --repl-script $<.repl

### BMC

%-bmc-spec.k.out: KPROVE = $(KBMC)
%-bmc-spec.k.out: KPROVE_SPEC = --raw-spec $<
%-bmc-spec.k.out: KPROVE_OPTS += --depth $(KBMC_DEPTH)

### MERGE

%.merge.out: $(TEST_DIR)/%.merge $(DEF_KORE) $(KORE_EXEC)
	@echo ">>>" $(CURDIR) "kore-exec --merge-rules" $<
	rm -f $@
	$(KORE_EXEC) $(DEF_KORE) $(KORE_EXEC_OPTS) --module $(KORE_MODULE) --merge-rules $< --output $@
	$(DIFF) $@.golden $@ || $(FAILED)

### SCRIPTS

test-%.sh.out: $(KORE_EXEC) $(TEST_DIR)/test-%.sh
	@echo ">>>" $(CURDIR) $(@:.out=)
	rm -f $@
	$(TEST_DIR)/$(@:.out=) > $@ || true
	$(DIFF) $@.golden $@ || $(FAILED)

### TARGETS

test: test-k

test-k: $(OUTS)

golden: $(foreach OUT, $(OUTS), $(OUT).golden)

clean:
	rm -fr $(KOMPILED) $(TEST_DIR)/*.out $(TEST_DIR)/*.save-proofs.kore

.PHONY: test-k test golden clean
