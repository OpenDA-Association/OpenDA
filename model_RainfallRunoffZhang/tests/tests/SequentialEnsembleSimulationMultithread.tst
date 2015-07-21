<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
    <id>Sequential ensemble simulation</id>
    <odaFile>../SequentialEnsembleSimulationMultithread.oda</odaFile>
    <checks>
        <!-- test if application runs through -->
        <check>
            <file removeBeforeTest="yes" >../SequentialEnsembleSimulationMultithread.log</file>
            <find>===DONE===</find>
        </check>
        
        <!-- test if results are as expected -->
        <check>
            <file removeBeforeTest="yes">../SequentialEnsembleSimulationMultithread_results.m</file>
            <find>pred_f_central{4}	=[0.5102562];</find>
        </check>
        
        <check>
            <file removeBeforeTest="yes">../SequentialEnsembleSimulationMultithread_results.m</file>
            <find>pred_f_0{4}	=[0.4719608];</find>
        </check>
        
        <check>
            <file removeBeforeTest="yes">../SequentialEnsembleSimulationMultithread_results.m</file>
            <find>pred_f_1{4}	=[0.4801615];</find>
        </check>
        
        <check>
            <file removeBeforeTest="yes">../SequentialEnsembleSimulationMultithread_results.m</file>
            <find>pred_f_2{4}	=[0.5921579];</find>
        </check>
        
    </checks>
</testConfig>