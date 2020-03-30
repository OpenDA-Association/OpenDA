call "c:\Program Files\Deltares\Delft3D 4.04.01\x64\dflow2d3d\scripts\run_dflow2d3d.bat" %1

rem timeout 2

del tri-rst.Est1D.000000

ren tri-rst.Est1D.* tri-rst.Est1D.000000
