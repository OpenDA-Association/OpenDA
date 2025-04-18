<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
        <id>/home/verlaanm/deltares/src/openda_20110523/public/tests/simple_lorenz96/./ThreeDVar.oda</id>
        <odaFile>../ThreeDVar.oda</odaFile>
        <checks>
                <check>
                        <file removeBeforeTest="yes" >../ThreeDVar.log</file>
                        <find>===DONE===</find>
                </check>
                <check>
                        <file removeBeforeTest="yes" >../threedvar_results.m</file>
			<!-- last line looks like:
			x_f_central{51}	=[6.370087895252388,8.373780710465734, ...
			-->
			<regex>x_f_central\{51\}(.*)\[6.37(.*)8.37(.*)</regex>
                </check>
        </checks>
</testConfig>
