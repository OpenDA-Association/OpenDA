
This module is essentially empty. It contains no useful code or data, but serves as
a template for creating new modules. One can create a new module by:
1) make a copy of the template 
	>> svn cp module_template my_module
2) change module name in several files:
	./module.properties
	./java/unit_test_info.txt
	./unit_test_info.txt
3) create your own useful content

Other material found in this template
- there is an ant build file ./build.xml to build java code. It copies the resulting
jar-file to the directory bin. You can also add external java resources in
./java/resources/
and external native libraries. 
./bin_external/
	
This template contains a small java class and unit-test. It is small but fully functional.
./java/src/org/openda/NothingUseful.java
./java/test/org/openda/NothingUsefulTest.java
There is also an external test case in the directory tests
./tests/run_test.sh
