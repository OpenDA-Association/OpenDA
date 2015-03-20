<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
	<id>Steadystate.oda</id>
	<!-- <odaFile>../Steadystate.oda</odaFile> -->
	<actions> <!-- First run generated steady-state files for second run -->
		<odaFile>Enkf_generate_gain.oda</odaFile>
		<odaFile>Steadystate.oda</odaFile>
	</actions>
	<checks>
		<check>
			<file removeBeforeTest="yes">Enkf_generate_gain.log</file>
			<find>===DONE===</find>
		</check>
		<check>
			<file removeBeforeTest="yes">Steadystate.log</file>
			<find>===DONE===</find>
		</check>
	</checks>
</testConfig>
