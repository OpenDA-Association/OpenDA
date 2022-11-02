<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
        <id>~/public/model_dflowfm_blackbox/tests/estuary_kalman_FMSuite2019.01_bcfile/Enkf.oda</id>
        <odaFile>../Enkf.oda</odaFile>
        <checks>
                <check>
                        <file removeBeforeTest="yes" >../Enkf.log</file>
                        <find>===DONE===</find>
                </check>
        </checks>
</testConfig>
