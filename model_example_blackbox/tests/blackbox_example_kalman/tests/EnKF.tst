<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
        <id>blackbox_example_kalman EnKF run</id>
        <odaFile>../EnKF.oda</odaFile>
        <checks>
                <check>
                        <file removeBeforeTest="yes" >../EnKF.log</file>
                        <find>===DONE===</find>
                </check>
                <check>
                        <file removeBeforeTest="yes" >../enkf_results.m</file>
			<regexp>pred_a_central{250}     =[128.0(.*), 110.8(.*), 217.8(.*)];</regexp>
                </check>
        </checks>
</testConfig>
