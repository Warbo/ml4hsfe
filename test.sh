#! /usr/bin/env nix-shell
#! nix-shell -i bash -p jq order-deps recurrent-clustering

function fail {
    echo -e "FAIL: $1" >> /dev/stderr
    exit 1
}

cabal build || fail "Couldn't build"

cabal test || fail "Tests failed"

for EX in ml4hsfe-loop-example-input*.json
do
    WIDTH=10 HEIGHT=10 cabal run ml4hsfe-loop < "$EX" > /dev/null ||
        fail "Failed sending '$EX' through ml4hsfe-loop"
done

for EX in ml4hsfe-outer-loop-example-input*.json
do
    WIDTH=10 HEIGHT=10 cabal run ml4hsfe-outer-loop < "$EX" > /dev/null ||
        fail "Failed sending '$EX' through ml4hsfe-outer-loop"
done
