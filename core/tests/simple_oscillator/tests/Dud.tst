<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
        <id>/home/verlaanm/deltares/src/openda_20110523/public/tests/simple_oscillator/./Dud.oda</id>
        <odaFile>../Dud.oda</odaFile>
        <checks>
                <check>
                        <file removeBeforeTest="yes" >../Dud.log</file>
                        <find>===DONE===</find>
                </check>
                <check>
                        <file removeBeforeTest="yes" >../dud_results.m</file>
                        <!-- <find>t_damp= 9.0</find> does not reach 9.0 exactly -->
                        <find>t_damp= 8.9</find>
                </check>
                <check>
                        <file removeBeforeTest="yes" >../dud_results.m</file>
                        <find>omega= 1.7</find>
                </check>
        </checks>
</testConfig>
