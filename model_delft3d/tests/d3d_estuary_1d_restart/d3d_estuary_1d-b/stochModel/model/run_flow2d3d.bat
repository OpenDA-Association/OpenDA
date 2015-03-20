@ echo off
    rem
    rem This script is an example for running Delft3D-FLOW
    rem Adapt and use it for your own purpose
    rem
    rem adri.mourits@deltares.nl
    rem 07 Feb 2012
    rem 
    rem
    rem This script starts a Delft3D-FLOW (5.xx.xx) computation on Windows
    rem


    rem
    rem Specify the config file to be used here
    rem
set argfile=Est1D-truth-noNoise.ini

    rem
    rem Set the directories containing deltares_hydro.exe and libraries here
    rem
set D3D_HOME=P:\h4\opt\delft3d\Delft3D-FLOW_WAVE\5.00.05.1576\win32
set exedir=%D3D_HOME%\flow2d3d\bin
set libdir=%D3D_HOME%\flow2d3d\lib





    rem
    rem No adaptions needed below
    rem

    rem Set some (environment) parameters
set PATH=%exedir%;%libdir%;%PATH%



    rem Run
%exedir%\deltares_hydro.exe %argfile%


    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
pause
