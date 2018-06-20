#!/usr/bin/env bash

function msg {
    echo -e "$1" 1>&2
}

function fail {
    msg "FAIL: $1"
    exit 1
}

command -v jq   || fail "No jq executable found"

cabal new-build || fail "Couldn't build"

cabal new-test  || fail "Tests failed"

# Test ml4hsfe-outer-loop on examples

for EX in examples/ml4hsfe-outer-loop-example-input*.json
do
    export WIDTH=10
    export HEIGHT=10
    HASKELL_RESULT=$(cabal new-run -v0 ml4hsfe-outer-loop < "$EX") ||
        fail "Failed sending '$EX' through ml4hsfe-outer-loop"

    msg "Ensuring Haskell results contain clusters"
    RESULT=$(echo "$HASKELL_RESULT" | jq 'map(has("cluster")) | all')
    [[ "x$RESULT" = "xtrue" ]] || {
        msg "Haskell results missing clusters"
        exit 1
    }
done
