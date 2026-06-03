#!/usr/bin/env sh
# Run the test suite in an isolated environment:
#  - R_PROFILE_USER=/dev/null  ignores ~/.Rprofile (which defines a personal `lsh`
#    and would otherwise mask the package version during tests)
#  - R_LIBS_USER  keeps the user package library on the search path (the profile
#    we just disabled is what normally sets it)
set -e
DIR="$(cd "$(dirname "$0")/.." && pwd)"
export R_PROFILE_USER=/dev/null
export R_LIBS_USER="${R_LIBS_USER:-$HOME/R/x86_64-pc-linux-gnu-library/4.4}"
exec Rscript "$DIR/dev/run-tests.R" "$@"
