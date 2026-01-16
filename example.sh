#!/bin/sh
set -e

bin=$( cabal exec -- which hs-socket-check )
dummy_sock=$(mktemp -u -t .sock)

echo "--- Running example.sh ---"

# Test case 1: No socket path (should show usage)
ex1_no_sock_path() {
    echo "--- Test 1: No socket path ---"
    "${bin}" || true
}

# Test case 2: Valid command with custom arguments
ex2_custom_args() {
    echo "--- Test 2: Valid command with custom arguments ---"
    touch "$dummy_sock"
    "${bin}" -t 500 -s "$dummy_sock" || true
    rm "$dummy_sock"
}

# Test case 3: Help message
ex3_help() {
    echo "--- Test 3: Help message ---"
    "${bin}" --help || true # Allow failure, as --help typically exits with non-zero
}

# Test case 4: Invalid argument (should show usage)
ex4_invalid_arg() {
    echo "--- Test 4: Invalid argument ---"
    "${bin}" --invalid-arg || true # Allow failure, as it'll likely exit with non-zero and print usage
}

# Run all test cases
ex1_no_sock_path
ex2_custom_args
ex3_help
ex4_invalid_arg

echo "--- example.sh finished ---"