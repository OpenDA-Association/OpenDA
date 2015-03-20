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
			x_f_central{100}	=[-6.126947970396822,-3.765484921139002,27.45332107385561];
			x_f_central{100}	=[-1.9932608250875528,0.16612298360391708,26.95731815538849];
			-->
			<regex>x_f_central\{100\}(.*)-1.99(.*)0.16(.*)26.95(.*)</regex>
                </check>
        </checks>
</testConfig>
