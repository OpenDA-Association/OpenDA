<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
        <id>/home/verlaanm/deltares/src/openda_20110523/public/tests/native_heat/./denkf.oda</id>
        <odaFile>../denkf.oda</odaFile>
        <checks>
                <check>
                        <file removeBeforeTest="yes" >../denkf.log</file>
                        <find>===DONE===</find>
                </check>
                <check>
                        <file removeBeforeTest="yes" >../denkf_results.m</file>
			<!-- last line looks like:
                        x_f_central{100}	=[-0.13477706617844842,0.04439380345957753
			-->
			<regex>x_f_central\{100\}(.*)-0.13477(.*),0.04439(.*)</regex>
                </check>
        </checks>
</testConfig>
