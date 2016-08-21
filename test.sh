#! /usr/bin/env nix-shell
#! nix-shell --show-trace -i bash -p jq runWeka

function msg {
    echo -e "$1" 1>&2
}

function fail {
    msg "FAIL: $1"
    exit 1
}

cabal build || fail "Couldn't build"

cabal test || fail "Tests failed"

# Test ml4hsfe-outer-loop on examples

for EX in examples/ml4hsfe-outer-loop-example-input*.json
do
    export WIDTH=10
    export HEIGHT=10
    HASKELL_RESULT=$(cabal run -v0 ml4hsfe-outer-loop < "$EX") ||
        fail "Failed sending '$EX' through ml4hsfe-outer-loop"

    msg "Ensuring Haskell results contain clusters"
    RESULT=$(echo "$HASKELL_RESULT" | jq 'map(has("cluster")) | all')
    [[ "x$RESULT" = "xtrue" ]] || {
        msg "Haskell results missing clusters"
        exit 1
    }
done
