# Example configuration setting for compiling native code on Mac OSX.
# Change this script to your own needs!

#Note location of my netcdf and jdk
#Note my mpi distro is installed on my path

#debug (prefered)
#./configure  -with-netcdf=/Users/nils/Devel/opt CFLAGS='-Wall -O0 -g' FCFLAGS='-Wall -O0 -g' CXXFLAGS='-Wall -O0 -g' FFLAGS='-Wall -O0 -g'   --with-jdk=/System/Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents/Home


./configure  -with-netcdf=/Users/nils/Devel/opt CFLAGS='-Wall -O0 -g' FCFLAGS='-Wall -O0 -g' CXXFLAGS='-Wall -O0 -g' FFLAGS='-Wall -O0 -g'   CPPFLAGS=-I/Developer/SDKs/MacOSX10.6.sdk/System/Library/Frameworks/JavaVM.framework/Versions/1.6.0/Headers --with-netcdf=/Users/nils/Devel/opt-4.8.2/netcdf-4.3.0




#release (faster code but use only for production)
#./configure --disable-examples -with-netcdf=/Users/nils/Devel/opt CFLAGS='-Wall -O0 -g' FCFLAGS='-Wall -O0 -g' CXXFLAGS='-Wall -O0 -g' FFLAGS='-Wall -O0 -g'   --with-jdk=/System/Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents/Home


# Some other (personal variations)

#export NETCDF_ROOT=/Users/nils/Devel/opt
#./configure --disable-mpi FC=gfortran --with-jdk=/System/Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents
#./configure --disable-examples -with-netcdf=/Users/nils/Devel/opt CFLAGS='-O0 -g' FCFLAGS='-O0 -g' CXXFLAGS='-O0 -g' FFLAGS='-O0 -g'   --with-jdk=/System/Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents/Home

#./configure --prefix=/Users/nils/Devel/public/bin/darwin  --disable-examples -with-netcdf=/Users/nils/Devel/opt CFLAGS='-Wall -O0 -g' FCFLAGS='-Wall -O0 -g' CXXFLAGS='-Wall -O0 -g' FFLAGS='-Wall -O0 -g'   --with-jdk=/System/Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents/Home
