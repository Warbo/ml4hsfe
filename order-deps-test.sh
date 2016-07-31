#! /usr/bin/env nix-shell
#! nix-shell -i bash -p jq

# Helper functions

function msg {
    echo -e "$1" >> /dev/stderr
}

function fail {
    msg "FAIL: $1"
    exit 1
}

function run {
    # Run order-deps. -v0 suppresses Cabal output, to prevent it interfering
    # with our JSON parsing. $1 should be an example JSON file.
    cabal run -v0 order-deps < "$1"
}

function inArray {
    isId      "$1"
    isIdArray "$2"

    # shellcheck disable=SC2016
    COUNT=$(echo "$2" | jq -c --argjson ast "$1" \
                           'map(select(.name == $ast.name and .module == $ast.module and .package == $ast.package)) | length')
    [[ "$COUNT" -gt 0 ]]
}

function areCyclic {
    DEPS1=$(depsClosure "$1")
    DEPS2=$(depsClosure "$2")

    isIdArray "$DEPS1"
    isIdArray "$DEPS2"
    inArray "$1" "$DEPS2" && inArray "$2" "$DEPS1"
}

function containsVersion {
    echo "$1" | grep -- '-[0-9][0-9.]*$' > /dev/null
}

# Assertions

function assertType {
    TYPE=$(echo "$1" | jq -r 'type') || fail "Couldn't get type of:\n$1"
    [[ "x$TYPE" = "x$2" ]] || fail "'$1' has type '$TYPE', not '$2'"
}

function assertAttribute {
    # shellcheck disable=SC2016
    HASATTRIBUTERESULT=$(echo "$1" | jq --arg attr "$2" 'has($attr)') ||
        fail "Couldn't check for '$2' attribute in '$1'"

    [[ "x$HASATTRIBUTERESULT" = "xtrue" ]] || fail "'$1' does not have attribute '$2'"
}

function assertEqual {
    # shellcheck disable=SC2016
    JQEQUALRESULT=$(jq -n --argjson x "$1" --argjson y "$2" '$x == $y') ||
        fail "Couldn't compare '$1' with '$2'"
    [[ "x$JQEQUALRESULT" = "xtrue" ]]
}

function isId {
    assertType      "$1" object
    assertAttribute "$1" name
    assertAttribute "$1" module
    assertAttribute "$1" package
}

function isIdArray {
    assertType "$1" array
    while read -r ENTRY
    do
        isId "$ENTRY"
    done < <(echo "$1" | jq -c '.[]')
}

function assertHaveJsonFile {
    # Check for existence of JSON files; don't check file descriptors
    [[ -n "$EX" ]] || fail "'EX' variable not set to example filename"

    FILENAMESUFFIX=$(echo "$EX" | tr '[:upper:]' '[:lower:]' | rev | cut -d '.' -f 1 | rev)
    if [[ "x$FILENAMESUFFIX" = "xjson" ]]
    then
        [[ -f "$EX" ]] || fail "Example filename '$EX' not found"
    else
        msg "File '$EX' doesn't appear to be JSON"
    fi
}

# Looking up dependencies

function depsOfKey {
    # Generate a key to use when caching depsOf results
    DEPSOFNAME=$(echo "$1" | jq -cr '.name')
    DEPSOFMOD=$(echo "$1"  | jq -cr '.module')
    DEPSOFPKG=$(echo "$1"  | jq -cr '.package')
    echo "EX $EX PKG $DEPSOFPKG MOD $DEPSOFMOD NAME $DEPSOFNAME" | tr -d '\t\n'
}

DEPSOFCACHE=/tmp/order-deps-test.cache
echo "" > "$DEPSOFCACHE"
function cachedDep {
    CACHEDDEP=$(join -j 1 -t $'\t' "$DEPSOFCACHE" <(echo "$1") | cut -f 2)
    if [[ -n "$CACHEDDEP" ]]
    then
        echo "$CACHEDDEP"
    else
        return 1
    fi
}

function cacheDep {
    DEPSOFCOUNT=$(cachedDep "$1" | wc -l)
    [[ "$DEPSOFCOUNT" -eq 0 ]] || fail "Already cached key '$1'"

    echo -e "${1}\t${2}" >> "$DEPSOFCACHE"

    # Cache must be sorted for join to work
    mv "$DEPSOFCACHE" "$DEPSOFCACHE.tmp"
    sort -t$'\t' -k 1,1 < "$DEPSOFCACHE.tmp" > "$DEPSOFCACHE"
    rm "$DEPSOFCACHE.tmp"

    DEPSOFCOUNT=$(cachedDep "$1" | wc -l)
    [[ "$DEPSOFCOUNT" -eq 1 ]] || fail "Didn't set key '$1'"
}

function calculateDepsOf {
    # Extracts the dependencies of $1 by looping through all entries in $EX
    # shellcheck disable=SC2016
    jq -c --argjson arg "$1" \
          'map(select(.name == $arg.name and .module == $arg.module and .package == $arg.package)) | map(.dependencies) | . + [[]] | .[0]' < "$EX"
}

function depsOf {
    # Return the dependencies of $1, if found

    # Results come from calculateDepsOf, but this version caches results and
    # checks a bunch of assertions too

    isId "$1"

    # Look up in cache
    DEPSOFKEY=$(depsOfKey "$1")
    if DEPSOFRESULT=$(cachedDep "$DEPSOFKEY")
    then
        echo "$DEPSOFRESULT"
        return
    fi

    assertHaveJsonFile
    DEPSOFRESULT=$(calculateDepsOf "$1")
    isIdArray "$DEPSOFRESULT"
    cacheDep "$DEPSOFKEY" "$DEPSOFRESULT"
    echo "$DEPSOFRESULT"
}

function depsClosure {
    isId "$1"

    # Output the closure of the given ID's dependencies, their dependencies, and
    # so on.
    IDS=$(echo "$1" | jq -s '.')
    while true
    do
        isIdArray "$IDS"

        PRELENGTH=$(echo "$IDS" | jq 'length')

        # Append the dependencies of each ID,
        while read -r ID
        do
            isId "$ID"
            # shellcheck disable=SC2016
            IDS=$(echo "$IDS" | jq --argfile deps <(depsOf "$ID") \
                                   '. + $deps | unique')
        done < <(echo "$IDS" | jq -c '.[]')

        isIdArray "$IDS"
        POSTLENGTH=$(echo "$IDS" | jq 'length')

        [[ "$PRELENGTH" -eq "$POSTLENGTH" ]] && break
    done

    echo "$IDS"
}

# Test functions

function testCabalBuild {
    cabal build || fail "Failed to build"
}

function testCabalTest {
    cabal test || fail "Failed to test"
}

function testInArrayWorksAsExpected {
    # Make sure our 'inArray' function works as expected
    inArray '{"name":"n","module":"m","package":"p"}' \
            '[]' &&
        fail "Found x in []"

    inArray '{"name":"n","module":"m","package":"p"}' \
            '[{"name":"a","module":"m","package":"p"}]' &&
        fail "Found x in [y]"

    inArray '{"name":"n","module":"m","package":"p"}' \
            '[{"name":"n","module":"m","package":"p"}]' ||
        fail "Didn't find x in [x]"
}

function testContainsVersionWorksAsExpected {
    containsVersion "foo-1.2.3"  || fail "Didn't spot version in 'foo-1.2.3'"
    containsVersion "foo-1.2bar" && fail "'foo-1.2bar' shouldn't be versioned"
}

function testDepsOfWorksAsExpected {
    # The ID to look up
    TESTDEPSOFID='"name":"n","module":"m","package":"p"'

    # The dependencies to use
    TESTDEPSOFDEPS='[{"name":"d","module":"m","package":"p"}]'

    # This will be read for dependency info
    EX=$(mktemp --tmpdir "order-deps-test.XXXXX")
    echo "[{$TESTDEPSOFID,\"dependencies\":$TESTDEPSOFDEPS}]" > "$EX"

    # Make sure the example dependencies were found
    # We use funky redirection to remain in the same process as our cache
    TESTDEPSOFOUTPUT=$(depsOf "{${TESTDEPSOFID}}")
    assertEqual "$TESTDEPSOFOUTPUT" "$TESTDEPSOFDEPS" ||
        fail "Couldn't get dependencies of test example"

    # Make sure these dependencies were cached
    TESTDEPSOFKEY=$(depsOfKey "{${TESTDEPSOFID}}")
    if grep -F "$TESTDEPSOFKEY" < "$DEPSOFCACHE" > /dev/null
    then
        TESTDEPSOFRESULT=$(cachedDep "$TESTDEPSOFKEY")
        assertEqual "$TESTDEPSOFRESULT" "$TESTDEPSOFDEPS"
    else
        fail "Key '$TESTDEPSOFKEY' not found"
    fi

}

function testExamplesAreJson {
    for EX in example*.json
    do
        run "$EX" > /dev/null ||
            fail "Failed to send '$EX' through order-deps"
    done
}

function testOutputsAreJson {
    for EX in example*.json
    do
        ORDERED=$(run "$EX")
        echo "$ORDERED" | jq '.' > /dev/null ||
            fail "Couldn't parse output as JSON:\n$ORDERED"
    done
}

function testExampleFields {
    for EX in example*json
    do
        FIELDS=( name module package arity quickspecable type ast dependencies )
        for FIELD in "${FIELDS[@]}"
        do
            # shellcheck disable=SC2016
            RESULT=$(jq --arg field "$FIELD" 'map(has($field)) | all' < "$EX")
            [[ "x$RESULT" = "xtrue" ]] ||
                fail "File '$EX' missing some '$FIELD' fields"
        done

        FIELDS=( name package module )
        for FIELD in "${FIELDS[@]}"
        do
            # shellcheck disable=SC2016
            RESULT=$(jq --arg field "$FIELD" \
                        '[.[] | .dependencies | .[] | has($field)] | all' < "$EX")
            [[ "x$RESULT" = "xtrue" ]] ||
                fail "File '$EX' missing some '$FIELD' fields in its deps"
        done
    done
}

function testNoVersionInExamplePackages {
    for EX in example*json
    do
        while read -r PACKAGE
        do
            containsVersion "$PACKAGE" &&
                fail "'$EX' contains versions in its package names ('$PACKAGE')"
        done < <(jq -r '.[] | .package' < "$EX")
    done
}

function testNoVersionInExampleDependencies {
    for EX in example*json
    do
        while read -r PACKAGE
        do
            containsVersion "$PACKAGE" &&
                fail "'$EX' contains versions in the package names of dependencies ('$PACKAGE')"
        done < <(jq -r '.[] | .dependencies | .[] | .package' < "$EX")
    done
}

function testNoDepsAfter {
    # The elements of one SCC shouldn't depend on anything in a later SCC
    for EX in example*.json
    do
        # DEPS stores the dependencies which have been asked for so far
        DEPS="[]"
        while read -r SCC
        do
            while read -r ELEM
            do
                # See if anything in this SCC has already been asked for
                inArray "$ELEM" "$DEPS" &&
                    fail "Dependency showed up too late:\n$ELEM"
            done < <(echo "$SCC" | jq -c '.[]')

            while read -r ELEM
            do
                # Remember which dependencies were asked for in this SCC
                # shellcheck disable=SC2016
                DEPS=$(echo "$DEPS" | jq --argfile thesedeps <(depsOf "$ELEM") \
                                         '. + $thesedeps')
            done < <(echo "$SCC" | jq -c '.[]')
        done < <(run "$EX" | jq -c '.[]')
    done
}

function testSiblingDepsAreCyclic {
    # Dependencies should only appear in the same SCC when there is a cycle
    for EX in example*.json
    do
        SCCS=$(run "$EX")
        msg "Testing with example '$EX', which gives '$(echo "$SCCS" | jq -c 'length')' SCCs"
        while read -r SCC
        do
            msg "Testing SCC of length '$(echo "$SCC" | jq -c 'length')'"
            # Loop over each SCC element
            while read -r ID
            do
                while read -r DEP
                do
                    if inArray "$DEP" "$SCC"
                    then
                        msg "$ID and $DEP should be cyclic"
                        areCyclic "$ID" "$DEP" ||
                            fail "'$ID' appears alongside '$DEP', which is too early"
                    fi
                done < <(depsOf "$ID" | jq -c '.[]')
            done < <(echo "$SCC" | jq -c '.[]')
        done < <(echo "$SCCS" | jq -c '.[]')
    done
}

function testAllTestsRunning {
    # We use the TESTS variable to allow selectively turning tests on and off.
    # This test ensures that we're informed when tests are turned off.
    while read -r DECLARED
    do
        FOUND=0
        for USED in "${TESTS[@]}"
        do
            [[ "x$DECLARED" = "x$USED" ]] && FOUND=1
        done
        [[ "$FOUND" -eq 1 ]] || fail "Test '$DECLARED' wasn't run"
    done < <(declare -F | cut -d ' ' -f 3- | grep "^test")
}

# Test invocation
msg "Running tests"

TESTS=( testCabalBuild
        testCabalTest
        testExamplesAreJson
        testOutputsAreJson
        testExampleFields
        testNoDepsAfter
        testInArrayWorksAsExpected
        testNoVersionInExamplePackages
        testNoVersionInExampleDependencies
        testContainsVersionWorksAsExpected
        testDepsOfWorksAsExpected
        testSiblingDepsAreCyclic
        testAllTestsRunning
      )
for TEST in "${TESTS[@]}"
do
    msg "Running $TEST"
    "$TEST"
    msg "PASS: $TEST"
done

testAllTestsRunning # In case we're fiddling with TESTS

msg "All tests pass"
