<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
        <id>/home/verlaanm/deltares/src/openda_20110523/public/tests/simple_lorenz96/./Ensr.oda</id>
        <odaFile>../Ensr.oda</odaFile>
        <checks>
                <check>
                        <file removeBeforeTest="yes" >../Ensr.log</file>
                        <find>===DONE===</find>
                </check>
                <check>
                        <file removeBeforeTest="yes" >../ensr_results.m</file>
			<!-- last line looks like:
			x_f_central{102}	=[5.891248852555715,8.938625342437101
			-->
			<regex>x_f_central\{102\}(.*)\[5.89(.*)8.93(.*)</regex>
                </check>
        </checks>
</testConfig>
