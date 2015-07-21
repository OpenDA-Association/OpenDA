<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
    <id>EnKF augmented</id>
    <odaFile>../EnKFaugmentedMultithread.oda</odaFile>
    <checks>
        <!-- test if application runs through -->
        <check>
            <file removeBeforeTest="yes" >../EnKFaugmentedMultithread.log</file>
            <find>===DONE===</find>
        </check>
        
        <!-- test if results are as expected -->
        <check>
            <file removeBeforeTest="yes">../EnKFaugmentedMultithread_results.m</file>
            <find>pred_f_central{4}	=[0.563952];</find>
        </check>
        
        <check>
            <file removeBeforeTest="yes">../EnKFaugmentedMultithread_results.m</file>
            <find>pred_f_0{4}	=[0.5166113];</find>
        </check>
        
        <check>
            <file removeBeforeTest="yes">../EnKFaugmentedMultithread_results.m</file>
            <find>pred_f_1{4}	=[0.6055411];</find>
        </check>
        
        <check>
            <file removeBeforeTest="yes">../EnKFaugmentedMultithread_results.m</file>
            <find>pred_f_2{4}	=[0.5810266];</find>
        </check>
        
    </checks>
</testConfig>