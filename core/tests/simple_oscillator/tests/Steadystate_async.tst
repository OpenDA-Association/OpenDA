<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
        <id>/home/verlaanm/deltares/src/openda_20110523/public/tests/simple_oscillator/./Steadystate_async.oda</id>
	<!-- <odaFile>../Steadystate_async.oda</odaFile> -->
	<actions> <!-- First run generated steady-state files for second run -->
	   <odaFile>../Enkf_async_generate_gain.oda</odaFile>
           <odaFile>../Steadystate_async.oda</odaFile>
	</actions>
        <checks>
                <check>
                        <file removeBeforeTest="yes" >../Enkf_async_generate_gain.log</file>
                        <find>===DONE===</find>
                </check>
                <check>
                        <file removeBeforeTest="yes" >../Steadystate_async.log</file>
                        <find>===DONE===</find>
                </check>
        </checks>
</testConfig>
