<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
	<id>Enkf for simple_oscillator</id>
	<odaFile>../Enkf.oda</odaFile>
	<checks>
		<check>
			<file removeBeforeTest="true" >../Enkf.log</file>
			<find>===DONE===</find>
		</check>
		<check>
			<file removeBeforeTest="true" >../enkf_results.m</file>
			<!-- last line
			     x_f_central{22} =[0.24074265713784465,-0.7263147129652403];
			-->
			<regex>x_f_central(.*)0.24(.*)</regex>
		</check>
		<check>
			<file>../error_file_does_not_exist.m</file>
		</check>
	</checks>
</testConfig>
