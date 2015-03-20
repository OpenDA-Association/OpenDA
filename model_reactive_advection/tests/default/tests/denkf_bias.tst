<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
        <id>/Users/nils/Devel/openda/openda_trunk/public/model_reactive_advection/tests/default/./denkf_bias.oda</id>
        <odaFile>../denkf_bias.oda</odaFile>
        <checks>
                <check>
                        <file removeBeforeTest="yes" >../denkf_bias_results.m</file>
			<!-- last line looks like:                        
pred_a{250}	=[82.68289531889033, 46.17352459212195, 54.652997759509255, 26.540018128417305, 62.274132342224355, 130.0228893646317];
                        -->
                        <regex>pred_a\{250\}(.*)=\[82.6(.*), 46.1(.*), 54.6(.*), 26.5(.*), 62.2(.*), 130.0(.*)\];(.*)</regex>
                </check>
        </checks>
</testConfig>
