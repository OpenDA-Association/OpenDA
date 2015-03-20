<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
        <id>/Users/nils/Devel/openda/openda_trunk/public/model_reactive_advection/tests/default/./enkf.oda</id>
        <odaFile>../denkf.oda</odaFile>
        <checks>
                <check>
                        <file removeBeforeTest="yes" >../denkf_results.m</file>
			<!-- last line looks like:                        
pred_a_central{250}	=[73.8977013927, 41.2232286078, 54.6734083881, 25.8008587556, 60.0947003217, 139.351134577];
                        -->
                        <regex>pred_a\{250\}(.*)=\[73.8(.*), 41.2(.*), 54.6(.*), 25.8(.*), 60.0(.*), 139.3(.*)\];(.*)</regex>
                </check>
        </checks>
</testConfig>
