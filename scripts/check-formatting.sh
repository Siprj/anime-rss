#!/bin/sh -e

fourmolu --mode check admin-tui backend server
cabal-fmt -c $(git ls-files '*.cabal')
