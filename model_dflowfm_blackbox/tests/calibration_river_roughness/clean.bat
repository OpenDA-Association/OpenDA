rem Removes all output files and directories
del results_*.m
del results_*.csv
del openda_logfile.txt
del *.orp
for /d %%a in (.\work\work*) do rd /s /q "%%~a"
