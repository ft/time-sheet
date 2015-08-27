#!/bin/sh

# Copyright (c) 2015 Frank Terbeck <ft@bewatermyfriend.org>
# All rights reserved.
#
# Terms for redistribution and use can be found in LICENCE.

GUILE_AUTO_COMPILE=1
export GUILE_AUTO_COMPILE
GUILE_LOAD_PATH="$PWD/scheme"
export GUILE_LOAD_PATH
GUILE_LOAD_COMPILED_PATH="$PWD/scheme"
export GUILE_LOAD_COMPILED_PATH
GUILE_WARN_DEPRECATED="detailed"
export GUILE_WARN_DEPRECATED

exec guile -s "$@"
