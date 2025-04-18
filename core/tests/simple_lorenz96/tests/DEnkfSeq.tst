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
                x_f_central{51}	=[5.803343423079002,8.965169757277147, ...
		-->
                        <file removeBeforeTest="yes" >../denkfseq_results.m</file>
			<regex>x_f_central\{51\}(.*)\[5.80(.*)8.96(.*)</regex>
                </check>
        </checks>
</testConfig>
