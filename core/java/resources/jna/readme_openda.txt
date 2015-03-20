2013-02-19:
ODA-220: Updated all jna jar files to JNA version 3.4.0. Added extra jar file aix-ppc64.jar that is needed for IBM AIX computers, used for efdc in Korea.
Notes from Frederik:
At NIER in Korea the EFDC DLL needs to run under 64-bit AIX. By default, JNA does not come with the appropriate native JNA library.
AIX native library (aix-ppc64.jar) for 64 bit AIX ppc architecture. Downloaded from https://github.com/twall/jna/blob/master/dist/aix-ppc64.jar
Needs JNA 3.4.0
version 3.4.0 of JNA (jna-3.4.0.jar) Was demonstrated to work correctly together with aix-ppc64.jar http://grepcode.com/snapshot/repo1.maven.org/maven2/net.java.dev.jna/jna/3.4.0

2013-09-25:
Jna version 3.4 is not working on the team-city test machines, so we reverted to version 3.3. For projects that need version 3.4 some
manual changes are needed.
