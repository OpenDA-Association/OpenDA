<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
        <id>/home/verlaanm/deltares/src/openda_20110523/public/tests/simple_lorenz96/./EnkfSeq.oda</id>
        <odaFile>../EnkfSeq.oda</odaFile>
        <checks>
                <check>
                        <file removeBeforeTest="yes" >../EnkfSeq.log</file>
                        <find>===DONE===</find>
                </check>
                <check>
		<!-- last line looks like
                x_f_central{102}	=[5.298859657721588,9.99941136547053
		-->
                        <file removeBeforeTest="yes" >../enkfseq_results.m</file>
			<regex>x_f_central\{102\}(.*)\[5.29(.*)9.99(.*)</regex>
                </check>
        </checks>
</testConfig>
