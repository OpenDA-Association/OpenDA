<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
        <id>~/public/model_dflowfm_blackbox/tests/lake_kalman/./Simulation.oda</id>
        <odaFile>../Simulation.oda</odaFile>
        <checks>
                <check>
                        <file removeBeforeTest="yes" >../Simulation.log</file>
                        <find>===DONE===</find>
                </check>
        </checks>
</testConfig>
