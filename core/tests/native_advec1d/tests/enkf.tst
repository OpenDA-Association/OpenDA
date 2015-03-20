<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
        <id>/home/verlaanm/deltares/src/openda_20110523/public/tests/native_advec1d/./enkf.oda</id>
        <odaFile>../enkf.oda</odaFile>
        <checks>
                <check>
                        <file removeBeforeTest="yes" >../enkf.log</file>
                        <find>===DONE===</find>
                </check>
                <check>
                        <file removeBeforeTest="yes" >../enkf_results.m</file>
			<!-- last line looks like:
			x_f_central{10}	=[0.8805614798402557,0.1922857270710871,0.01420890749740441
			-->
			<regexp>xf_central\{10\}(.*)0.8(.*)0.1(.*)0.01(.*)</regexp>
                </check>
        </checks>
</testConfig>
