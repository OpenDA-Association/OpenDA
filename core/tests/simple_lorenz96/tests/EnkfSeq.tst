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
                x_f_central{51}	=[5.318608587817751,9.37221864086852
		-->
                        <file removeBeforeTest="yes" >../enkfseq_results.m</file>
			<regex>x_f_central\{51\}(.*)\[5.31(.*)9.37(.*)</regex>
                </check>
        </checks>
</testConfig>
