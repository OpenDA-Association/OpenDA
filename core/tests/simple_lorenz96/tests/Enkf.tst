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
		x_f_central{51}	=[5.879433301067781,8.98461820713056, ...
		-->
                        <file removeBeforeTest="yes" >../enkf_results.m</file>
			<regex>x_f_central\{51\}(.*)\[5.87(.*)8.98(.*)</regex>
                </check>
        </checks>
</testConfig>
