<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
        <id>/home/verlaanm/deltares/src/openda_20110523/public/tests/simple_lorenz96/./Enkf.oda</id>
        <odaFile>../Enkf.oda</odaFile>
        <checks>
                <check>
                        <file removeBeforeTest="yes" >../Enkf.log</file>
                        <find>===DONE===</find>
                </check>
                <check>
		<!-- last line looks like
		x_f_central{102}	=[5.865551814494317,8.90458092953776
		-->
                        <file removeBeforeTest="yes" >../enkf_results.m</file>
			<regex>x_f_central\{102\}(.*)\[5.86(.*)8.90(.*)</regex>
                </check>
        </checks>
</testConfig>
