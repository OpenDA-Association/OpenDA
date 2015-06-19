#!/bin/sh
cygdlls=`ls .libs/cyg*.dll 2>/dev/null`
if [ x$cygdlls != x ]; then
cp $cygdlls $2
fi
