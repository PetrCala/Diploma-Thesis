#!/bin/sh

# Function to display help
show_help() {
    echo "Usage: $0 <command> [args]"
    echo
    echo "Commands:"
    echo "  setup           Setup the environment"
    echo "  help            Display this help message"
}

# Check if no arguments were provided
if [ $# -eq 0 ]; then
    show_help
    exit 1
fi

# Main switch-case to handle commands
case "$1" in
setup)
    shift
    sh scripts/setup.sh "$@"
    ;;
help)
    show_help
    ;;
*)
    echo "Error: Unknown command: $1"
    show_help
    exit 1
    ;;
esac
