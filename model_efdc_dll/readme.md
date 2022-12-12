The EFDC model wrapper for OpenDA   
=================================

This module contains code that can be used for connecting the EFDC model to OpenDA using an in-memory coupling. Unit tests for this code are also included.

Warning: The Linux and AIX version are not yet fully implemented. 


Building and installing the Fortan source code
----------------------------------------------

The directory model_efdc_dll/native/ contains the Fortran source files. The Fortran source code needs to be compiled as a dynamic link library (Windows) or a shared library (unix/linux). This library then needs to be placed in the appropriate subdirectory in model_efdc_dll/native_bin/. When building OpenDA (with ant build in the OpenDA root directory) the contents of native_bin/ directory is copied to the OpenDA bin directory. 

For Windows there are Visual Studio 2008 and 2010 solution files available (requiring the Intel Fortran compiler). When the model is built using the release configuration the EfdcFortranDll.dll is automatically copied to the correct location for windows binaries (native_bin/win32_ifort/).  

For Linux, a makefile is present in native/ to build libEFDC.so. The code is compiled by issuing the command: 
    
    > make

The command

    > make install

copies the built library, libEFDC.so, to the native_bin/linux64_gnu directory when compiling on a 64-bit machine or to native_bin/linux32_gnu on a 32-bit machine. 
A clean recompilation can be achieved by first removing the create shared library and object files using

    > make clean

command and then compiling again. 

The makefile in model_efdc_dll/native/ recursively calls the makefiles present in all subdirectories. The code is separated in two parts, the  native/efdc_fortran_dll/original_efdc_files directory contains the original EFDC files (with some minor modifications). This part is compiled and built as a static archive. The makefile in native/efdc_fortran_dll/openDA_wrapper then builds the files for OpenDA compatibility and links this with the static archive to create the shared library libEFDC.so. 
Note that the original EFDC code cannot be compiled with gfortran. When building the source  for the GNU fortran compiler (gfortran) a number of patches is applied to the original EFDC files to make it compatible with gfortran. 

Provisional makefiles for AIX
--------------------------

For AIX there are two specific makefiles (Makefile.aix) in native/efdc_fortran_dll/openDA_wrapper and native/efdc_fortran_dll/original_efdc_files. Rename these files to Makefile (without extension) to compile for AIX.


Compiling for a new architecture or compiler
--------------------------------------------

When compiling on new machine architecture or with another compiler, a number of steps are required:
 -  Create a new directory for the new architecture and compiler combination in native_bin/. 
    For instance native_bin/aix32_xlf/ for a 32-bit executable or library on AIX with the XLF fortran compiler.
 -  Set the correct compiler and compiler options in the Makefile in native/efdc_fortran_dll/original_efdc_files/. 
 -  Set the correct compiler and compiler options in the Makefile in native/efdc_fortran_dll/openDA_wrapper/. 
    Set the NATIVE_BIN variable to the directory created in the first step.

Adding support for the new architecture in EfdcDLL.java 
-------------------------------------------------------

The shared library is loaded by the Java code in model_efdc_dll/java/src/org/openda/model_efdc_dll/EfdcDLL.java. Support for loading the library for the new architecture needs to be added here. When running on a new architecture JNA needs to load the appropriate version of the shared library. For instance when running OpenDA on AIX, make sure that the libEFDC.so from native_bin/aix32_xlf is loaded.
A new name mangling scheme is also required. Use the GfortranFunctionMapper.java or IntelFortranFunctionMapper.java as an example.These files contain a HashMap which link the Java method name (first string) to the specific function name available in libEFDC.so. For instance the method "m_openda_wrapper_init_" is linked to "__m_openda_wrapper_MOD_init" in GfortranFunctionMapper.java. The required function names in a specific version of the shared library can be obtained with:
    
    > nm libEFDC.so | grep openda 
    
Note that using a 32-bit java runtime environment (jre) for OpenDA requires a 32-bit version of the dynamical-link library or shared library. Likewise, using a 64-bit jre requires a 64-bit library. 
