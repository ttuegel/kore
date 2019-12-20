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

KOMPILED := $(TEST_DIR)/$(DEF)-kompiled
DEF_KORE := $(KOMPILED)/definition.kore
TEST_DEPS = $(K) $(DEF_KORE) $(KORE_EXEC)

TESTS = $(wildcard $(TEST_DIR)/*.$(EXT)) $(wildcard $(TEST_DIR)/*-spec.k)

OUTS = $(foreach TEST, $(TESTS), $(TEST).out)

KOMPILE_OPTS += -d $(DEF_DIR)
KRUN_OPTS += -d $(DEF_DIR)
KPROVE_OPTS += -d $(DEF_DIR) -m $(KPROVE_MODULE)
KPROVE_REPL_OPTS += -d $(DEF_DIR) -m $(KPROVE_MODULE)

$(DEF_KORE): $(DEF_DIR)/$(DEF).k $(K)
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
	rm -f $(@:.out=.save-proofs)
	$(KPROVE) $(KPROVE_OPTS) $< --output-file $@ || true
	$(DIFF) $@.golden $@ || $(FAILED)
	! [[ -f $(@:.out=.save-proofs) ]] \
		|| $(DIFF) $(@:.out=.save-proofs).golden $(@:.out=.save-proofs) \
		|| $(FAILED)

%-repl-spec.k.out: KPROVE_OPTS = $(KPROVE_REPL_OPTS)

%-repl-script-spec.k.out: \
	KPROVE_OPTS = \
		-d $(DEF_DIR) -m $(KPROVE_MODULE) \
		--haskell-backend-command \
		"$(KORE_REPL) -r --repl-script $<.repl"

%-save-proofs-spec.k.out: \
	KPROVE_OPTS =\
		-d $(DEF_DIR) -m $(KPROVE_MODULE) \
		--haskell-backend-command \
		"$(KORE_EXEC) --save-proofs $(@:.out=.save-proofs)"

### BMC

%-bmc-spec.k.out: KPROVE = $(KBMC)
%-bmc-spec.k.out: KPROVE_OPTS += --debug --raw-spec --depth $(KBMC_DEPTH)

### MERGE

%.merge.out: $(TEST_DIR)/%.merge $(DEF_KORE) $(KORE_EXEC)
	$(KORE_EXEC) $(DEF_KORE) --module $(KORE_MODULE) --merge-rules $< --output $@
	$(DIFF) $@.golden $@ || $(FAILED)

### TARGETS

test: test-k

test-k: $(OUTS)

golden: $(foreach OUT, $(OUTS), $(OUT).golden)

clean:
	rm -fr $(KOMPILED) $(TEST_DIR)/*.out

.PHONY: test-k test golden clean
