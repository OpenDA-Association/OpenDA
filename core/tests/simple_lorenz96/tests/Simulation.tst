<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
        <id>/home/verlaanm/deltares/src/openda_20110523/public/tests/simple_lorenz96/./Simulation.oda</id>
        <odaFile>../Simulation.oda</odaFile>
        <checks>
                <check>
                        <file removeBeforeTest="yes" >../Simulation.log</file>
                        <find>===DONE===</find>
                </check>
                <check>
                        <file removeBeforeTest="yes" >../simulation_results.m</file>
			<!-- last line looks like
			x_f_central{51}	=[6.045796963594913,-1.1366813812223224, ...
			-->
			<regex>x_f_central\{51\}(.*)\[6.04(.*)-1.13(.*)</regex>
                </check>
        </checks>
</testConfig>
