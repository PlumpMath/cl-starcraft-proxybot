#!/bin/sh
#
# fatalist.sh
#
# author: Erik Winkels (aerique@xs4all.nl)
#
# See the LICENSE file in this project's directory for more info.


# $OSTYPE isn't exported by my Linux bash shell.
if [ "$OSTYPE" = "" ]; then
    OSTYPE=`uname`;
fi


if [ "$OSTYPE" = "Linux" -o "$OSTYPE" = "linux-gnu" ]; then
    export LD_LIBRARY_PATH=/lib:/usr/lib:/usr/local/lib/OGRE:./lib;
    if command -v sbcl 1>&2 > /dev/null; then  # Preferring SBCL on Linux.
        CL="sbcl";
    elif command -v lx86cl 1>&2 > /dev/null; then
        CL="lx86cl";
    elif command -v clisp 1>&2 > /dev/null; then
        CL="clisp";
    else
        echo "Could not find SBCL, CCL or CLISP in PATH... aborting.";
        exit 1;
    fi
#elif OSX
#    DYLD_LIBRARY_PATH=./lib:${DYLD_LIBRARY_PATH};
# ...
elif [ "$OSTYPE" = "Windows_NT" -o "$OSTYPE" = "msys" ]; then
    PATH=$PATH:./lib;
    if command -v wx86cl 1>&2 > /dev/null; then  # Preferring CCL on Windows.
        CL="wx86cl";
    elif command -v sbcl 1>&2 > /dev/null; then
        CL="sbcl";
    elif command -v clisp 1>&2 > /dev/null; then
        CL="clisp";
    else
        echo "Could not find SBCL, CCL or CLISP in PATH... aborting.";
        exit 1;
    fi
fi


if [ "$CL" = "sbcl" -o "$CL" = "lx86cl" -o "$CL" = "wx86cl" ]; then
    $CL --load fatalist.lisp --eval "(scpb::pb-start)";
elif [ "$CL" = "clisp" ]; then
    $CL -ansi -repl -i fatalist.lisp -x "(scpb::pb-start)";
fi


if [ "$OSTYPE" = "Linux" -o "$OSTYPE" = "linux-gnu" ]; then
    xset r on;
fi

exit 0;
