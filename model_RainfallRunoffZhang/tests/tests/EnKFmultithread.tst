<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
    <id>EnKF</id>
    <odaFile>../EnKFmultithread.oda</odaFile>
    <checks>
        <!-- test if application runs through -->
        <check>
            <file removeBeforeTest="yes" >../EnKFmultithread.log</file>
            <find>===DONE===</find>
        </check>
        
        <!-- test if results are as expected -->
        <check>
            <file removeBeforeTest="yes">../EnKFmultithread_results.m</file>
            <find>pred_f_central{4}	=[0.5309731];</find>
        </check>
        
        <check>
            <file removeBeforeTest="yes">../EnKFmultithread_results.m</file>
            <find>pred_f_0{4}	=[0.5151807];</find>
        </check>
        
        <check>
            <file removeBeforeTest="yes">../EnKFmultithread_results.m</file>
            <find>pred_f_1{4}	=[0.5149467];</find>
        </check>
        
        <check>
            <file removeBeforeTest="yes">../EnKFmultithread_results.m</file>
            <find>pred_f_2{4}	=[0.6158509];</find>
        </check>
        
    </checks>
</testConfig>