2015-01-27 Created jar files libthrift-0.9.2.jar and libthrift-0.9.2-sources.jar using the following steps and jdk1.7.0_55_x86:
    1. Download source code of Thrift 0.9.2 from http://thrift.apache.org/download and extract all files.
    2. Make sure that JAVA_HOME environment variable points to the right JDK.
    3. In folder thrift-0.9.2\lib\java run the following commands:
       ant dist
       ant pack.src
