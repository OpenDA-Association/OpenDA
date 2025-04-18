<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
        <id>/home/verlaanm/deltares/src/openda_20110523/public/tests/simple_lorenz96/./DEnkf.oda</id>
        <odaFile>../DEnkf.oda</odaFile>
        <checks>
                <check>
                        <file removeBeforeTest="yes" >../DEnkf.log</file>
                        <find>===DONE===</find>
                </check>
                <check>
		<!-- last line looks like
		x_f_central{51}	=[5.907863987206802,8.95173452830568, ...
		-->
                        <file removeBeforeTest="yes" >../denkf_results.m</file>
			<regex>x_f_central\{51\}(.*)\[5.90(.*)8.95(.*)</regex>
                </check>
        </checks>
</testConfig>
