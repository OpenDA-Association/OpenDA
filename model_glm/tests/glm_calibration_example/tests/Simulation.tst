<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
        <id>blackbox_example_calibration simulation</id>
        <odaFile>../Simulation.oda</odaFile>
        <checks>
                <check>
                        <file removeBeforeTest="yes" >../Simulation.log</file>
                        <find>===DONE===</find>
                </check>
                <check>
                        <file removeBeforeTest="yes" >../simulation_results.m</file>
                        <find>% Application Done</find>
                </check>
        </checks>
</testConfig>
