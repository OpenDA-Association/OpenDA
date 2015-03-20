<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
        <id>/home/verlaanm/deltares/src/openda_20110523/public/tests/simple_lorenz/./lorenzSimulation.oda</id>
        <odaFile>../lorenzSimulation.oda</odaFile>
        <checks>
                <check>
                        <file removeBeforeTest="yes" >../lorenzSimulation.log</file>
                        <find>===DONE===</find>
                </check>
                <check>
                        <file removeBeforeTest="yes" >../simulation_results.m</file>
			<!-- last line is: 
			x{4802}	=[4.096770835245913,7.812688016745072,8.672410451691654];
			x{4802}	=[-0.19626263072816622,0.12202223335386664,16.90055939530945];
			-->
			<regex>x\{4802\}(.*)-0.19(.*)0.12(.*)16.90(.*)</regex>
                </check>
        </checks>
</testConfig>
