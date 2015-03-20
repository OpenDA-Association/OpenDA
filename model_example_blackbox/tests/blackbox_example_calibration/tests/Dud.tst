<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
        <id>blackbox_example_calibration DUD calibration</id>
        <odaFile>../Dud.oda</odaFile>
        <checks>
                <check>
                        <file removeBeforeTest="yes" >../Dud.log</file>
                        <find>===DONE===</find>
                </check>
                <check>
                        <file removeBeforeTest="yes" >../dud_results.m</file>
			<find>source.factory1.discharge= 0.28</find>
                </check>
                <check>
                        <file removeBeforeTest="no" >../dud_results.m</file>
			<find>source.factory2.discharge= -0.69</find>
                </check>
        </checks>
</testConfig>
