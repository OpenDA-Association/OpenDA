rem Removes all output files and directories
del *_results.m
del openda_logfile.txt
del *.orp
for /d %%a in (.\stochModel\work*) do rd /s /q "%%~a"