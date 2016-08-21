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

for EX in examples/ml4hsfe-loop-example-input*.json
do
    WIDTH=10 HEIGHT=10 cabal run -v0 ml4hsfe-loop < "$EX" > /dev/null ||
        fail "Failed sending '$EX' through ml4hsfe-loop"
done

# Test ml4hsfe-outer-loop against slow bash alternative

function compareResults {
    # shellcheck disable=SC2016
    jq -n --slurpfile a "$1" --slurpfile b "$2" \
       '$a[0] | map((. + {"cluster":null}) as $elem | $b[0] | map((select((. + {"cluster":null}) == $elem))) | length | . == 1)'
}

function bashCluster {
  DEPS=$(cat)

  while read -r SCC
  do
    msg "Next SCC"

    # shellcheck disable=SC2016
    DEPS=$(echo "$DEPS" | jq --slurpfile scc <(echo "$SCC") \
             'map(. as $elem | if ($scc[0] | map(.name == $elem.name and .module == $elem.module and .package == $elem.package) | any) then . + {"tocluster":true} else . end)')

    # Update all features with the latest clusters

    # Look up an ID in $deps
    # shellcheck disable=SC2016
    COND2='.name == $this.name and .module == $this.module and .package == $this.package'

    # shellcheck disable=SC2016
    LOOKUP='(. as $this | $deps | map(select('"$COND2"') | .cluster) | . + [0] | .[0] | . + 300)'
    FEATURES="(.features | map(if type == \"object\" then ($LOOKUP) else . end))"

    # Cluster. We call runWeka directly since nix-shell adds a lot of
    # overhead, which we move outside the loop to our own invocation
    msg "Clustering..."
    # shellcheck disable=SC2016
    CLUSTERED=$(
      echo "$DEPS" |
      jq '. as $deps | $deps | map(. + {"features": '"$FEATURES"'})' |
      runWeka)

    # Add new clusters to DEPS
    msg "Collating..."
    # shellcheck disable=SC2016
    DEPS=$(echo "$DEPS" | jq --argfile clustered <(echo "$CLUSTERED") \
                             'map(. as $this | $clustered | map(select(.name == $this.name and .module == $this.module and .package == $this.package)) | map(.cluster) | if length == 1 then $this + {"cluster": .[0]} else $this end)')
  done < <(echo "$DEPS" | cabal run order-deps -v0 | jq -c '.[]')

  msg "Done"
  echo "$DEPS"
}

for EX in examples/ml4hsfe-outer-loop-example-input*.json
do
    export WIDTH=10
    export HEIGHT=10
    HASKELL_RESULT=$(cabal run -v0 ml4hsfe-outer-loop < "$EX") ||
        fail "Failed sending '$EX' through ml4hsfe-outer-loop"
    BASH_RESULT=$(cabal run -v0 ml4hsfe-loop < "$EX" | bashCluster)

    msg "Checking Haskell results appear in Bash results"
    CHECK1=$(compareResults <(echo "$HASKELL_RESULT") <(echo "$BASH_RESULT"))

    FAIL=0
    if [[ "x$CHECK1" = "xfalse" ]]
    then
        msg "Some Haskell results do not appear in Bash results"
        FAIL=1
    fi

    msg "Checking Bash results appear in Haskell results"
    CHECK2=$(compareResults <(echo "$BASH_RESULT") <(echo "$HASKELL_RESULT"))

    if [[ "x$CHECK2" = "xfalse" ]]
    then
        msg "Some Bash results do not appear in Haskell results"
        FAIL=1
    fi

    if [[ "$FAIL" -eq 1 ]]
    then
        msg "Aborting due to mismatch"
        exit 1
    fi

    msg "Ensuring Haskell results contain clusters"
    RESULT=$(echo "$HASKELL_RESULT" | jq 'map(has("cluster")) | all')
    [[ "x$RESULT" = "xtrue" ]] || {
        msg "Haskell results missing clusters"
        exit 1
    }
done
