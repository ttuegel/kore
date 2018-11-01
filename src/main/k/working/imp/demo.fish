#!/usr/bin/env fish

set TOP (git rev-parse --show-toplevel)
set KORE_EXEC (stack path --local-install-root)/bin/kore-exec
set K_BIN $TOP/.build/k/k-distribution/target/release/k/bin

function kompile
    eval $K_BIN/kompile --backend haskell \
        (string escape -- $argv)
end

function krun
    eval $K_BIN/krun --haskell-backend-command $KORE_EXEC \
        (string escape -- $argv)
end

stack build kore:exe:kore-exec

kompile imp.k --syntax-module IMP

krun tests/max-symbolic.imp --search-final \
  --output-file tests/max-symbolic.search.final.output

krun tests/impossible-branch.imp --search-final \
  --output-file tests/impossible-branch.search.final.output
