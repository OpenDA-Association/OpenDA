<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
    <id>Sequential simulation</id>
    <odaFile>../SequentialSimulation.oda</odaFile>
    <checks>
        <!-- test if application runs through -->
        <check>
            <file removeBeforeTest="yes" >../SequentialSimulation.log</file>
            <find>===DONE===</find>
        </check>
        
        <!-- test if results are as expected -->
        <check>
            <file removeBeforeTest="yes">../SequentialSimulation_results.m</file>
            <find>pred_f_central{2}	=[0.513129];</find>
        </check>
        
        <check>
            <file removeBeforeTest="yes">../SequentialSimulation_results.m</file>
            <find>pred_f_central{3}	=[0.5199531];</find>
        </check>
        
        <check>
            <file removeBeforeTest="yes">../SequentialSimulation_results.m</file>
            <find>pred_f_central{4}	=[1.121405];</find>
        </check>
    </checks>
</testConfig>