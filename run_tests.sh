#!/bin/bash

set -e

function expect_ok ()
{

    if ./Test $*; then
        echo "Test passed: $*"
    else
        echo "Test failed!"
        echo "Command line: $*"
        echo "Was NOT expected to give an error"
        exit 1
    fi
}

function expect_fail ()
{

    if ! ./Test $* > /dev/null; then
        echo "Test passed: $*"
    else
        echo "Test failed!"
        echo "Command line: $*"
        echo "Was expected to give an error"
        exit 1
    fi
}

ghc -o Test Test.hs

expect_ok --global-with-arg=13 --foo help
expect_ok help
expect_ok help --local-foo
expect_ok help --local-foo-with-arg=karhu --local-foo
expect_ok help-me
expect_ok help-me --local-foo-with-arg=karhu --local-foo
expect_ok add foo.c
expect_ok add foo.c foo2.c
expect_ok --global-foo add foo.c
expect_ok --global-foo add foo.c foo2.c
expect_ok --global-foo --global-foo2 add foo.c
expect_ok --global-foo add --local-foo foo.c
expect_ok --global-foo add --local-foo --local-foo foo.c
expect_ok --global-foo add --local-foo-with-arg=xx --local-foo --local-foo foo.c foo2.c foo3.c

# TODO get escaping right for spaces
#expect_ok --global-foo-with-arg='\"aces with spaces\"' help
#expect_ok help --local-foo-with-arg="'aces with spaces'" help

# unknown command
expect_fail 1 2
expect_fail xyzzy
expect_fail --global-foo xyzzy
expect_fail xyzzy --local-foo

# command missing cases
expect_fail
expect_fail --global-foo
expect_fail --global-foo-with-arg=karhu
expect_fail --global-foo-with-arg=karhu --local-foo

# empty option arguments
expect_fail --global-foo-with-arg= help
expect_fail help --local-foo-with-arg=
expect_fail --foo-with-arg= --foo help

# local options after final file args
expect_fail add foo.c --local-foo
expect_fail add foo.c foo2.c --local-foo
expect_fail add foo.c --local-foo-with-arg=bar
expect_fail add foo.c --local-foo-with-arg=bar --local-foo

# unknown options for a command
expect_fail --global-foo add --local-foo-xyzzy foo.c

# global foo doesn't expect args
expect_fail --global-foo=test add foo.c
# global-foo-with-args requires args
expect_fail --global-foo-with-args add foo.c

# ditto for locals
expect_fail add foo.c --local-foo-with-arg
expect_fail add foo.c --local-foo=bar

echo "ALL PASS"
