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
		x_f_central{102}	=[5.918771659370243,8.848111030656193
		-->
                        <file removeBeforeTest="yes" >../denkf_results.m</file>
			<regex>x_f_central\{102\}(.*)\[5.91(.*)8.84(.*)</regex>
                </check>
        </checks>
</testConfig>
