#!/usr/bin/env bash

out() {
    while [[ "$#" -gt 0 ]]
    do
        if [[ -n "${HIE_BIOS_OUTPUT}" ]]
        then
            echo "$1" >> "${HIE_BIOS_OUTPUT}"
        else
            echo "$1"
        fi
        shift
    done
}

stack_dist_dir=$(stack path --dist-dir)

out -i

yq -r '. "packages" []' < ./stack.yaml | while read pkg
do
    for dir in src test app/share bench "$stack_dist_dir/build/autogen"
    do
        if [ -d "$pkg/$dir" ]
        then
            out -i"$pkg/$dir"
        fi
    done
done

out -clear-package-db
out -package-db $(stack path --local-pkg-db)
out -package-db $(stack path --snapshot-pkg-db)
out -package-db $(stack path --global-pkg-db)

yq -r '. "ghc-options" []' < ./package-haskell.yaml | while read opt
do
    out "${opt}"
done

yq -r '. "default-extensions" []' < ./package-haskell.yaml | while read ext
do
    out "-X${ext}"
done
