
Introduction
============

This folder contains a simple dummy model implemented in Fortran90, and a
simple java wrapper that accesses the Fortran90 model's DLL (windows) or SO (linux).

Right now the example does not compute anything at all, it is just a fake model that shows
one of the many ways if how a java OpenDA model wrapper can communicate with a Fortran90
computational core, using jna to brid the language gap.

The model wrapper implements the ModelInstance interface and the IModelAdjoint interface.
The StochasticModelInstance methods are assumed to be implemented by eather a surrounding
black box wrapper, or an extending class.


Building the java wrapper
=========================

The java wrapper can be built by the ant build files, by using IntelliJ idea 8.0,
or by creating an eclipse project.

- IntelliJ:  When the module is part of the full openda tree, the java/model_f90dll.iml
             module file can be imported in the ../../openda_1.ipr file
             If the module is checked out stand alone, open java/model_f90dll_alone.ipr
- ant build: When the module is part of the full openda tree, the regular "build" target
             should be used. This is done by giving the command:
             	ant build
             If the module is checked out stand alone, use the "build_for_stand_alone"
             target in the build-for-stand-alone.xml file:
             	ant -f build-for-stand-alone.xml build_for_stand_alone

When building or rebuilding the java wrapper and the tests, the latest 'installed' version
of the native SO/DLL will be taken from the ./bin_external directory and
will be placed in ./bin. 
This latest 'installed' version might be a wrong version for your platform, in which case
you have to build and/or install the native SO/DLL for your own compiler and/or OS
(see below, be sure to start with 'make clean' to remove the existing SO.)


Building the native DLL/SO
==========================

The native part can be built and tested by making:
- simplefortrandll.dll / libsimplefortran.so from the sources in simple_fortran_dll
- simplefortrandlltest(.exe) from the sources in simple_fortran_dll_test

Windows
-------
For windows platforms a Visual Studio 2008 solution is provided. The Debug configuration
will generate the DLL locally. The Release configuration will generate it on the
binaries directory in the test data for the java test:
./java/test/org/openda/examples/simplef90model/testData/bin
and will copy it to the ./bin_external directory.

Linux
-----
For linux platforms, the available make files can be used. The files have been prepared
for the ifort compiler. When using another compiler, please adjust the compiler name and
flags in ./native/simple_fortran_dll and ./native/simple_fortran_dll_test.

The generated SO and test executable will be placed on the ./bin directory.

'make install' will copy the SO to the binaries directory in the test data for the java test:
./java/test/org/openda/examples/simplef90model/testData/bin,
and to the directory containing the binaries that are used by ant build:
./bin_external

Java/Fortran language bridging (jna)
------------------------------------
The fortran access from java level by means of jna has been tested with the
ifort 11.0 compiler. If another compiler is used, the method names in
org.openda.examples.simplef90model.SimpleModelDLL.ISimpleFortranNativeDLL
(see java/src/org/openda/examples/simplef90model/SimpleModelDLL.java)
may have to be adjusted according to the names in the generated SO.
For instance, the gfortran compiler will prefix the method names with "__".


Running the tests
=================

native test (fortran test executable)
-------------------------------------
To run the native test, include the ./bin directory in the path(s) that are searched for
exe's and dll's/so's. On linux this can be done easiest by executing the setup script on the
native directory:
    cd native
    . ./setup.sh

NOTE: The native test will create the model instance directories work1, work2 and work3 right
under the current working directory.


java tests
----------
The java tests can be run as junit tests, or from the command prompt by providing
SimpleModelInstanceTestExe as main class.
The latter can be done by running the
    run_test.bat          (Windows)
or
    run_test.sh           (Linux)
script.
If no arguments are provided, the current working directory is taken as parent for the
model instance directories work1, work2 and work3, and the native SO libsimplefortran.so
is assumed to be placed in the current working directory.

By providing one of the arguments -h, h, -help, help, -? or ?,
help is provided on how to specify an other instances parent directory and/or another
native SO path:

Usage: <progName> [ -P<modelInstancesParentDir> ] [ -N<native DLL/SO> ]
       default modelInstancesParentDir: . (current working directory)
       default modelInstancesParentDir, Windows: ./simplefortrandll.dll
       default modelInstancesParentDir,   Linux: ./libsimplefortran.so

So for instance to run from a directory 'test' that is on the same level as java,
native, bin, etc., i.e. right under project level, either give:
       cd test
       run_test.sh -N../bin/libsimplefortran.so
or (supposing that your current working directory is the project level)
       run_test.sh -P./test -N./bin/libsimplefortran.so
