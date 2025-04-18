<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
        <id>/home/verlaanm/deltares/src/openda_20110523/public/tests/simple_lorenz/./lorenzEnkf.oda</id>
        <odaFile>../lorenzEnkf.oda</odaFile>
        <checks>
                <check>
                        <file removeBeforeTest="yes" >../lorenzEnkf.log</file>
                        <find>===DONE===</find>
                </check>
                <check>
                        <file removeBeforeTest="yes" >../enkf_results.m</file>
			<!--
			x_f_central{301}	=[-7.167603682683764,-10.793621052746401,18.780092646446324];
			-->
			<regex>x_f_central\{301\}(.*)-7.16(.*)</regex>
                </check>
        </checks>
</testConfig>
