<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
        <id>/home/verlaanm/deltares/src/openda_20110523/public/tests/simple_lorenz96/./DEnkfSeq.oda</id>
        <odaFile>../DEnkfSeq.oda</odaFile>
        <checks>
                <check>
                        <file removeBeforeTest="yes" >../DEnkfSeq.log</file>
                        <find>===DONE===</find>
                </check>
                <check>
		<!-- last line looks like
                x_f_central{102}	=[5.866424565651181,8.897985098008585
		-->
                        <file removeBeforeTest="yes" >../denkfseq_results.m</file>
			<regex>x_f_central\{102\}(.*)\[5.86(.*)8.89(.*)</regex>
                </check>
        </checks>
</testConfig>
