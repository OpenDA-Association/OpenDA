net stop "LicSvcLoc"
taskkill /f /im DHI.MikeSHE.Engine.Server.exe
taskkill /f /im M21LinkSrv.exe
net start "LicSvcLoc"
