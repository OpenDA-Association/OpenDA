<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
        <id>~/public/model_dflowfm_blackbox/tests/lake_kalman/SequentialSimulation.oda</id>
        <odaFile>../SequentialSimulation.oda</odaFile>
        <checks>
                <check>
                        <file removeBeforeTest="yes" >../SequentialSimulation.log</file>
                        <find>===DONE===</find>
                </check>
        </checks>
</testConfig>
