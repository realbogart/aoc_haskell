#!/bin/sh

REPO_ROOT=$(git rev-parse --show-toplevel)

ln -s -f $REPO_ROOT/git-hooks/pre-commit $REPO_ROOT/.git/hooks/pre-commit

