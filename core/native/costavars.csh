# add the COSTAWB program to the path
setenv PATH "`pwd`/src/costawb:${PATH}"

# set start directory to all model dynamic libraries inside the costa distro
setenv PD "`pwd`/examples"

setenv LD_LIBRARY_PATH "${LD_LIBRARY_PATH}:${PD}/ar1/src/.libs:${PD}/heat_stoch/src/.libs:${PD}/lorenz96/src/.libs:${PD}/advec1d/scr/.libs:${PD}/heat_modelcombiner/src/.libs:${PD}/lorenz/src/.libs:${PD}/oscill/src/.libs:${PD}/pollute2d/src/.libs:`pwd`/../external/libs"


# add the openda wrapper to the JAVA Classpath
setenv CLASSPATH "${CLASSPATH}:${PD}/src/openda"

#add netcdf path
#setenv NETCDF_ROOT "`pwd`/../../opt_e"
