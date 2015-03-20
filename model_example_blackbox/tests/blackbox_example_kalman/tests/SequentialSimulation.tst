<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
        <id>blackbox_example_kalman simulation</id>
        <odaFile>../SequentialSimulation.oda</odaFile>
        <checks>
                <check>
                        <file removeBeforeTest="yes" >../SequentialSimulation.log</file>
                        <find>===DONE===</find>
                </check>
                <check>
                        <file removeBeforeTest="yes" >../sequentialSimulation_results.m</file>
			<regexp>pred_f_central{250}     =[154.(.*), 139.(.*), 234.(.*)];</regexp>
                </check>
        </checks>
</testConfig>
