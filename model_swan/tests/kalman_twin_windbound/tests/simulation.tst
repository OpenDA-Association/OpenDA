<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
        <id>public/model_swan/tests/kalman_twin_windbound/./simulation.oda</id>
        <odaFile>../simulation.oda</odaFile>
        <checks>
                <check>
                        <file removeBeforeTest="yes" >../simulation.log</file>
                        <find>===DONE===</find>
                </check>
        </checks>
</testConfig>