<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
        <id>/Users/nils/Devel/openda/openda_trunk/public/model_reactive_advection/tests/default/./enkf.oda</id>
        <odaFile>../enkf.oda</odaFile>
        <checks>
                <check>
                        <file removeBeforeTest="yes" >../enkf_results.m</file>
			<!-- last line looks like:                        
pred_a{250}	=[72.89353441079999, 39.54890859151001, 53.131197993469996, 25.450431954433334, 57.654408884273344, 134.33526797703328];
                        -->
                        <regex>pred_a\{250\}(.*)=\[72.8(.*), 39.5(.*), 53.1(.*), 25.4(.*), 57.6(.*), 134.3(.*)\];(.*)</regex>
                </check>
        </checks>
</testConfig>
