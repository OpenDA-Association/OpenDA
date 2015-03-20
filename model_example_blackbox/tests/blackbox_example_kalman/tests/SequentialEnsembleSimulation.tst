<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
        <id>blackbox_example_kalman ensemble simulation</id>
        <odaFile>../SequentialEnsembleSimulation.oda</odaFile>
        <checks>
                <check>
                        <file removeBeforeTest="yes" >../SequentialEnsembleSimulation.log</file>
                        <find>===DONE===</find>
                </check>
                <check>
                        <file removeBeforeTest="yes" >../sequentialEnsembleSimulation_results.m</file>
			<!--
			x_f_central{500}	=[0.8284050924559869, -0.08610075244099527,...
			-->
			<regexp>x_f_central\{500\}(.*)0.8(.*)-0.08(.*)</regexp>
                </check>
        </checks>
</testConfig>
