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
			x_f_central{51}	=[5.834383006199769,9.0091227587933, ...
			-->
			<regex>x_f_central\{51\}(.*)\[5.83(.*)9.00(.*)</regex>
                </check>
        </checks>
</testConfig>
