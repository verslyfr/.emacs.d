# -*- mode: snippet -*-
# name: argparse_setup
# key: argparse_setup>
# group: templates
# --
import argparse
__VERSION= "0.0.1"

def getArgs(argv=None):
    parser: argparse.ArgumentParser = argparse.ArgumentParser(description="${1:description of the script}")
    parser.add_argument(
        "--version",type="version",version=__VERSION
    )
    parser.add_argument(
        "-v",
        "--verbose",
        dest="verbosity",
        action="count",
        default=0,
        help="Verbosity -v to -vvvv (between 1-4 occurrences with more leading to more "
        "verbose logging . CRITICAL=0, ERROR=1, WARN=2, INFO=3, "
        "DEBUG=4)",
    )
    return parser.parse_args(argv)
