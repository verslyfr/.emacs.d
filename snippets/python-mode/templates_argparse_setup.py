# -*- mode: snippet -*-
# name: argparse_setup
# key: argparse_setup>
# group: templates
# --
## the following should be moved to the top of the file. The Version number should be adjusted
## the __VERSION represents "Major.Minor.Bug"
import argparse
__VERSION= "0.0.1"

def get_args(argv=None):
    """Parses the command-line arguments.

    Args:
        argv (t.Any): (default: None) A list of arguments.
          If None then _sys.argv[1:] is used as the input. Supports testing of inputs via scripts.
    
    Returns:
        the results of calling argparse.ArgumentParser.parse_args
    """
    parser: argparse.ArgumentParser = argparse.ArgumentParser(description="${1:description of the script}")
    parser.add_argument(
        "--version",action="version",version=__VERSION
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

    args = parser.parse_args(argv)

    # Place any checks to perform on the args here. For example, if using a string for filename,
    # check for the existance here. parser.error and parser.exit can be used to share specific
    # issues out of this routine and stop execution.
    
    return args
