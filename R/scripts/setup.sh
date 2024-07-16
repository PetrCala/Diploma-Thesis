#!/bin/bash

set -e

SCRIPTS_DIR=$(dirname "${BASH_SOURCE[0]}")
source "$SCRIPTS_DIR/shellUtils.sh"

ROJECT_ROOT_REL=$(dirname "$SCRIPTS_DIR")
PROJECT_ROOT=$(get_abs_path "$ROJECT_ROOT_REL")

if ! command -v R &>/dev/null; then
    error "R is not installed. Make sure to install it from https://www.cran.r-project.org."
    exit 1
fi

info "Setting up the environment..."

cd $PROJECT_ROOT

source "$SCRIPTS_DIR/setupR.sh" # Prints verbose output

success "Done."
