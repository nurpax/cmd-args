#!/bin/bash

function expect_ok ()
{
    ./Test $*
    if [ $? = 0 ]; then
        echo "Test passed: $*"
    else
        echo "Test failed!"
        echo "Command line: $*"
        echo "Was NOT expected to give an error"
    fi
}

function expect_fail ()
{
    ./Test $* > /dev/null
    if [ $? != 0 ]; then
        echo "Test passed: $*"
    else
        echo "Test failed!"
        echo "Command line: $*"
        echo "Was expected to give an error"
    fi
}

ghc -o Test Test.hs

expect_ok 1 2 
expect_ok --foo-with-arg=13 --foo help
expect_ok --foo-with-arg= --foo help
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
expect_ok --global-foo add --local-foo --local-foo2 foo.c
expect_ok --global-foo add --local-foo-with-arg=xx --local-foo --local-foo2 foo.c foo2.c foo3.c

# command missing cases
expect_fail
expect_fail --local-foo
expect_fail --local-foo-with-arg=karhu
expect_fail --local-foo-with-arg=karhu --local-foo

# local options after final file args
expect_fail add foo.c --local-foo
expect_fail add foo.c foo2.c --local-foo
expect_fail add foo.c --local-foo-with-args=bar
expect_fail add foo.c --local-foo-with-args=bar --local-foo
