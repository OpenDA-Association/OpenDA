<?xml version="1.0" encoding="UTF-8"?>
<testConfig>
   <id>/home/verlaanm/deltares/src/openda_20110523/public/tests/simple_oscillator/./Enkf_write_restart.oda</id>
   <actions>
      <odaFile>../Enkf_write_restart.oda</odaFile>
      <odaFile>../Enkf_startfrom_restart.oda</odaFile>
   </actions>
   <checks>
      <check>
         <file removeBeforeTest="yes" >../Enkf_write_restart.log</file>
         <find>===DONE===</find>
     </check>
      <check>
         <file removeBeforeTest="yes" >../Enkf_startfrom_restart.log</file>
         <find>===DONE===</find>
     </check>
   </checks>
</testConfig>
