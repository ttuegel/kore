#!/usr/bin/env bash

# Generate standalone Kore tests from wasm-semantics.
# Usage:
#   1. Clone the `wasm-semantics` repository.
#   2. Follow the instructions to prepare the dependencies on your system.
#   3. Set the KORE environment variable in your shell to the location of the
#      `kore` repository.
#   4. Run this script in the root of the `wasm-semantics` repository.

set -exuo pipefail

kollect() {
    local name="$1"
    shift
    echo '#!/bin/sh' > "$name.sh"
    "$@" --debug --dry-run | xargs $KORE/scripts/kollect.sh "$name" >> "$name.sh"
    chmod +x "$name.sh"
}

make build-haskell

for spec in \
    simple-arithmetic \
    locals \
    loops \
    memory-concrete-type \
    memory-symbolic-type
do
    kollect "test-$spec" \
        ./kwasm prove --backend haskell \
            tests/proofs/"$spec"-spec.k \
            --def-module KWASM-LEMMAS
done
