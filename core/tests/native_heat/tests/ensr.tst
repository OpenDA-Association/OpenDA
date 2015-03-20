<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
        <id>/home/verlaanm/deltares/src/openda_20110523/public/tests/native_heat/./ensr.oda</id>
        <odaFile>../ensr.oda</odaFile>
        <checks>
                <check>
                        <file removeBeforeTest="yes" >../ensr.log</file>
                        <find>===DONE===</find>
                </check>
                <check>
                        <file removeBeforeTest="yes" >../ensr_results.m</file>
			<!-- last line looks like:
                        x_f_central{100}	=[-0.10538095237252221,0.08306747824342421
			-->
			<regex>x_f_central\{100\}(.*)-0.10538(.*),0.08306(.*)</regex>
                </check>
        </checks>
</testConfig>
