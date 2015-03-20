@ echo off

rem ============ set runid etc.

set runid=rot
set deelrun1=res_deelrun1
set deelrun2=res_deelrun2
set dirnaam=06-rot
  
rem ============ set exedir etc.
set exe=%1
set exedir=%2

echo '===========================================
echo '===  %1 %runid% ======================================
echo '===========================================
echo exedir=%exedir%
echo exe=%exe%
rem pause

rem ============ remove output files
del runid
del TMP*.*
del tri-diag.*
del *.msg
del com*.*
rem echo copy com-fff.def com-fff_orig.def

del fourier*.*
del md-diag*.*

echo ======= start exe  : ========
 
REM ONDERSTAANDE IS VOOR testen dll: test_dll_phase0.exe of test_dll_kalman0.exe
 echo %exedir%\%exe% %runid% %dirnaam% %deelrun1% %deelrun2%
      %exedir%\%exe% %runid% %dirnaam% %deelrun1% %deelrun2%
   
echo ======= end test_dll.exe =============
   rem pause

rem ============ remove output files
rem del runid
rem del TMP*.*
rem del tri*.*
rem del *.msg
rem del com*.*
rem del fourier*.*
rem del md-diag*.*
