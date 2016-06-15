#!/bin/sh

PACKAGES="sandi tagsoup"

cabal update || exit 1
cabal sandbox init || exit 1
cabal install $PACKAGES --dry-run | highlight-versions
read DUMMY
cabal install $PACKAGES || exit 1

cabal exec zsh
