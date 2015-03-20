# There is an issue with gfortran 4.6. The flag of the dependent libraries is not correctly handled by automake/autoconf
# This results in (-l <libnane> and not -l<libname>). After all processing the compiler tries to link with -l wich causes errors.
# This fix is inspired by a post of Roger Ferrer Ibáñez (http://lists.gnu.org/archive/html/bug-libtool/2012-03/msg00004.html).
#
# Users should not use the "normal" autoconf after chanes but used this script.
#
# Autohor: Nils van Velzen, September 2012
#
autoreconf
# Copy the generated configure script such that we have a back-up
cp configure configure_org

# Change the processing of the link flags, we just remove "-l " if it appears. 
# This is probably not a very generic fix but it works for OpenDA.
cat configure_org | sed 's/output_verbose_link_cmd='\''$CC -shared $CFLAGS -v conftest.$objext 2>&1 | $GREP -v "^Configured with:" | $GREP "\\-L"'\''/output_verbose_link_cmd='\''$CC -shared $CFLAGS -v conftest.$objext 2>\&1 | $GREP -v "^Configured with:" | $GREP "\\-L" |  $SED -e "s\/-l \/\/g"'\''/' > configure

# Make it executable
chmod +x configure

# Some more facy fix as suggested by Roger Ferrer Ibáñez
# But for some reason this did not work for us
#cat configure | sed 's/output_verbose_link_cmd='\''$CC -shared $CFLAGS -v conftest.$objext 2>&1 | $GREP -v "^Configured with:" | $GREP "\\-L"'\''/output_verbose_link_cmd='\''$CC -shared $CFLAGS -v conftest.$objext 2>\&1 | $GREP -v "^Configured with:" | $GREP "\\-L" |  $SED -e "s\/-l\s\+\(@<:@^-@:>@\S*\)\/-l\\1\/g"'\''/' > configure2
