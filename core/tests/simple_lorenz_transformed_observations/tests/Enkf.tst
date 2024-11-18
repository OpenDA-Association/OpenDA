<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
        <id>/home/verlaanm/deltares/src/openda_20110523/public/tests/simple_lorenz_transformed_observations/./Enkf.oda</id>
        <odaFile>../Enkf.oda</odaFile>
        <checks>
                <check>
                        <file removeBeforeTest="yes" >../Enkf.log</file>
                        <find>===DONE===</find>
                </check>
                <check>
                        <file removeBeforeTest="yes" >../enkf_results.m</file>
			<!-- last line looks like
                        x_f_central{50}	=[-2.9210526731141533,-3.8394213552984002,19.48173201984313];
			-->
			<regex>x_f_central\{50\}(.*)-2.92(.*)-3.83(.*)19.48(.*)</regex>
                </check>
        </checks>
</testConfig>
