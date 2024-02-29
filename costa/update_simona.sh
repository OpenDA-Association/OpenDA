#! /bin/sh
#
# If both Simona and OpenDA environments are set up,
# this script will update the native OpenDA libraries
# in the Simona environment.

check_env () {
   #check for OPENDADIR and OPENDALIB
   if [ -z "$OPENDADIR" ]; then
      echo "OpenDA environment not initialized properly."
      return 1
   fi

   #check for SIMONADIR and UI_NAME
   if [ -z "$SIMONADIR" -o -z "$UI_NAME" ]; then
      echo "Simona environment not initialized properly."
      return 2
   fi
   return 0
}

if [ check_env ] ; then
   ODA_COSTA=$OPENDADIR/../core/native_bin/$UI_NAME
   SIM_COSTA=$SIMONADIR/src/extern/costa
   ODA_COSTA_BIN=$ODA_COSTA/bin
   ODA_COSTA_LIB=$ODA_COSTA/lib
   SIM_COSTA_BIN=$SIM_COSTA/binexe
   SIM_COSTA_LIB=$SIM_COSTA/archives
   
   # Copy binaries
   echo "Updating binaries..."
   for group in costawb
   do
      for file in $group $group.exe
      do
         if [ -e $ODA_COSTA_BIN/$file ] ; then
            cp -vfup "$ODA_COSTA_BIN/$file" "$SIM_COSTA_BIN/$file.$UI_NAME"
         fi
      done
   done
   
   # Copy libraries
   echo "Updating libraries..."
   for group in libadvec1d \
                libblas \
                libcoffee \
                libcta \
                libensemble \
                libensrf \
                libheat_stoch \
                liblapack \
                libODA_COSTA_bridge \
                liboscill \
                librrsqrt_kalmina \
                librrsqrt \
                libsimulate \
                libsir \
                libsqlite3 
   do
      for file in $group.a $group.la $group.so $group.so.0 $group.so.0.0.0
      do
          if [ -e $ODA_COSTA_LIB/$file ] ; then
             cp -vfup "$ODA_COSTA_LIB/$file" "$SIM_COSTA_LIB/$file.$UI_NAME"
          fi
      done
   done

   # Perform a simake.pl in the src/external/costa directory
   echo "Running simake.pl..."
   cd $SIM_COSTA
   simake.pl
fi

