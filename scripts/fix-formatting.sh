#!/bin/sh -e

fourmolu -i admin-tui backend server
cabal-fmt -i $(git ls-files '*.cabal')
