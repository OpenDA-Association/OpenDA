<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
        <id>/home/verlaanm/deltares/src/openda_20110523/public/core/tests/simple_river1d/./dud_average.oda</id>
        <odaFile>../dud_average.oda</odaFile>
        <checks>
                <check>
                        <file removeBeforeTest="yes" >../dud_average.log</file>
                        <find>===DONE===</find>
                </check>
                <check>
                        <file removeBeforeTest="yes" >../dud_average_results.m</file>
                        <find>alpha_a= 0.996</find>
                </check>
        </checks>
</testConfig>
