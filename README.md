# CASC - A tool to check the formatting style of Curry programs

CASC is a tool to check the formatting style of Curry programs
The preferred style for writing Curry programs,
which is partially checked by this tool,
is described in a separate web page, see
<https://www.informatik.uni-kiel.de/~curry/style/>.

Currently, CASC only checks a few formatting rules, like line lengths or
indentation of `if-then-else`, but the kind of checks
performed by CASC will be extended in the future.

## Installing the tool

The tool can be directly installed by the command

    > cpm install casc

This installs the executable `curry-style` in the bin directory of CPM.


## Using the tool

If the bin directory of CPM (default: `~/.cpm/bin`) is in your path,
execute the tool with the Curry program to be style-checked, e.g.,

    > curry-style NiceProgram

