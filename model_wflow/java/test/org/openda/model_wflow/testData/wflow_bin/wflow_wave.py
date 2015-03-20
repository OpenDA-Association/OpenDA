#!/usr/bin/python

# Wflow is Free software, see below:
# 
# Copyright (c) J. Schellekens/Deltares 2005-2011
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.



# $Rev:: 542           $:  Revision of last commit
# $Author:: schelle    $:  Author of last commit
# $Date:: 2012-11-27 1#$:  Date of last commit
"""

Run the wflow_wave  model


usage: 
wflow_wave [-h][-v level][-F runinfofile][-L logfile][-C casename][-R runId]
      [-c configfile][-T timesteps][-s seconds][-W][-E][-N][-U discharge]
      [-P parameter multiplication]
-F: if set wflow is expected to be run by FEWS. It will determine
    the timesteps from the runinfo.xml file and save the output initial
    conditions to an alternate location. The runinfo.xml file should be located
    in the inmaps directory of the case.
-X: save state at the end of the run over the initial conditions at the start    
-f: Force overwrite of existing results
-T: Set last timestep
-S: Set the start timestep (default = 1)
-s: Set the model timesteps in seconds (default 86400)
-I: re-initialize the initial model conditions with default
-i: Set input table directory (default is intbl)
-x: run for subcatchment only (e.g. -x 1)
-Q: external Qbase
-C: set the name  of the case (directory) to run
-R: set the name runId within the current case
-c: name of wflow the configuration file (default: Casename/wflow_wave.ini). 
-h: print usage information
-U: The argument to this option should be a .tss file with measured discharge in
    [m^3/s] which the progam will use to update the internal state to match 
    the measured flow. The number of columns in this file should match the 
    number of gauges.
-u: list of gauges/columns to use in update. Format:
    -u [1 , 4 ,13]
    The above example uses column 1, 4 and 13
    Note that this also sets the order in which the updating takes place! In
    general specify downstream gauges first.
-P: set parameter multiply dictionary (e.g: -P {'self.FirstZoneDepth' : 1.2}
    to increase self.FirstZoneDepth by 20%, multiply with 1.2)
-p: set input parameter (dynamic, e.g. precip) multiply dictionary 
    (e.g: -p {'Precipitation' : 1.2} to increase Precipitation 
    by 20%, multiply with 1.2)    
    
"""




import numpy
import os
import os.path
import shutil, glob
import getopt
from wf_DynamicFramework import *
import scipy
import wflow_adapt
import pcrut





wflow = "wflow_wave: "
wflowVersion = "$Revision: 542 $  $Date: 2012-11-27 19:00:43 +0100 (Tue, 27 Nov 2012) $" #: revision of the model


updateCols = [] #: columns used in updating


multpars = {} #: Dictionary with parameters and multipliers (static) (used in calibration)
multdynapars = {} #: Dictionary with parameters and multipliers (dynamic) (used in calibration)



def usage(*args):
    """
    Print usage information
    
    @param *args: command line arguments given
    
    """
    sys.stdout = sys.stderr
    for msg in args: print msg
    print __doc__
    sys.exit(0)



class WflowModel(DynamicModel):
  
  
  def __init__(self, cloneMap,Dir,RunDir,configfile):
      DynamicModel.__init__(self)
      setclone(Dir + "/staticmaps/" + cloneMap)
      self.runId=RunDir
      self.caseName=Dir
      self.Dir = Dir
      self.configfile = configfile
      
        
  
  def readmap(self, name, default, style=1):
      """
      Reads a pcraster map
      
      @param name: name of the map to read
      @param default: default value in case the maps is not found
      
      @return: pcraster map
      """

      return self._readmapNew(name, default, style)  
      
      
  def updateRunOff(self):
      """
      Updates the kinematic wave reservoir
      """
      self.WaterLevel=(self.Alpha*pow(self.SurfaceRunoff,self.Beta))/self.Bw
      # wetted perimeter (m)
      P=self.Bw+(2*self.WaterLevel)
      # Alpha  
      self.Alpha=self.AlpTerm*pow(P,self.AlpPow)
      self.OldKinWaveVolume = self.KinWaveVolume
      self.KinWaveVolume = self.WaterLevel * self.Bw * self.DCL


  def supplyVariableNamesAndRoles(self):
      """
      Returns a list of variables as a
      List of list with the following structure::
          [[ name, role, unit]
          [ name, role, unit]
          ...   
          ]
          role: 0 = input (to the model)
                1 = is output (from the model)
                2 = input/output (state information)
          unit: 0 = mm/timestep
                1 = m^3/sec
                2 = m
                3 = degree Celcius
                4 = mm
                
      @return: list of variables
      """
      
      
      varlist = [['SurfaceRunoff',2,1],
                 ['WaterLevel',2,2],
                 ['P',0,0],
                 ['PET',0,0],
                 ['T',0,3]
      ]

      return varlist
      
      
    # The following are made to better connect to deltashell/openmi
  def supplyCurrentTime(self):
      """
      gets the current time in seconds after the start of the run
      
      @return: time in seconds since the start of the model run
      """
      return self.currentTimeStep() * configget(self.config,'model','timestepsecs')
  

  def readtblDefault(self,pathtotbl,landuse,subcatch,soil, default):
    """
    First check if a prepared  maps of the same name is present
    in the staticmaps directory. next try to
    read a tbl file to match a landuse, catchment and soil map. Returns 
    the default value if the tbl file is not found.
    
    @param pathtotbl: full path to table file
    @param landuse: landuse map
    @param subcatch: subcatchment map
    @param soil: soil map
    @param default: default value
    @return: map constructed from tbl file or map with default value
    """
    
    mapname = os.path.dirname(pathtotbl) + "/../staticmaps/" + os.path.splitext(os.path.basename(pathtotbl))[0]+".map"
    if os.path.exists(mapname):
        self.logger.info("reading map parameter file: " + mapname)
        rest = cover(readmap(mapname),default)
    else:
        if os.path.isfile(pathtotbl):
            rest=cover(lookupscalar(pathtotbl,landuse,subcatch,soil), default)
            self.logger.info("Creating map from table: " + pathtotbl)
        else:
            self.logger.warn("tbl file not found (" + pathtotbl + ") returning default value: " + str(default))
            rest = scalar(default)
        
    return rest
    
    
  def suspend(self):
    """
      Suspens the model to disk. All variables needed to restart the model
      are saved to disk as pcraster maps. Use resume() to re-read them
    """
    
    
    self.logger.info("Saving initial conditions...")
    self.wf_suspend(self.SaveDir + "/outstate/")
   
    if self.OverWriteInit:            
        self.logger.info("Saving initial conditions over start conditions...")
        self.wf_suspend(self.SaveDir + "/instate/")


    if self.fewsrun:
        self.logger.info("Saving initial conditions for FEWS...")
        self.wf_suspend(self.Dir + "/outstate/")
        
    report(self.sumlevel,self.SaveDir + "/outsum/sumlevel.map")
    report(self.sumrunoff/catchmenttotal(1,self.TopoLdd),self.SaveDir + "/outsum/sumrunoff.map")


      
  def initial(self):
      
    """
    Initial part of the model, executed only once. Is read all static model
    information (parameters) and sets-up the variables used in modelling.
    
    """
    global statistics
    global multpars
    
    setglobaloption("unittrue")
    
    self.thestep = scalar(0)
    self.setQuiet(True)
    self.precipTss="../intss/P.tss" #: name of the tss file with precipitation data ("../intss/P.tss")
    self.evapTss="../intss/PET.tss" #: name of the tss file with potential evap data ("../intss/PET.tss")
    self.tempTss="../intss/T.tss" #: name of the tss file with temperature  data ("../intss/T.tss")
    self.inflowTss="../intss/Inflow.tss" #: NOT TESTED name of the tss file with inflow data ("../intss/Inflow.tss")
    self.SeepageTss="../intss/Seepage.tss" #: NOT TESTED name of the tss file with seepage data ("../intss/Seepage.tss")"    
        

    self.logger.info("running for " + str(self.nrTimeSteps()) + " timesteps") 
    self.setQuiet(True)
    
        # Set and get defaults from ConfigFile here ###################################
    self.scalarInput = int(configget(self.config,"model","ScalarInput","0"))
    self.Tslice = int(configget(self.config,"model","Tslice","1"))
    self.interpolMethod = configget(self.config,"model","InterpolationMethod","inv")
    self.reinit = int(configget(self.config,"model","reinit","0"))
    self.fewsrun = int(configget(self.config,"model","fewsrun","0"))
    self.OverWriteInit = int(configget(self.config,"model","OverWriteInit","0"))
    self.updating = int(configget(self.config,"model","updating","0"))
    self.updateFile = configget(self.config,"model","updateFile","no_set")

    self.sCatch = int(configget(self.config,"model","sCatch","0"))
    self.intbl = configget(self.config,"model","intbl","intbl")
    self.timestepsecs = int(configget(self.config,"model","timestepsecs","86400"))
    self.P_style = int(configget(self.config,"model","P_style","1"))
    self.PET_style = int(configget(self.config,"model","PET_style","1"))
    self.TEMP_style = int(configget(self.config,"model","TEMP_style","1"))
    sizeinmetres = int(configget(self.config,"layout","sizeinmetres","0"))
    alf = float(configget(self.config,"model","Alpha","60"))
    Qmax = float(configget(self.config,"model","AnnualDischarge","300"))
    self.UpdMaxDist =float(configget(self.config,"model","UpdMaxDist","100"))
    self.ExternalQbase=int(configget(self.config,'model','ExternalQbase','0'))



    # 2: Input base maps ########################################################  
    subcatch=ordinal(readmap(self.Dir + "/staticmaps/wflow_subcatch.map")) # Determines the area of calculations (all cells > 0)
    subcatch = ifthen(subcatch > 0, subcatch)
    if self.sCatch > 0:
        subcatch = ifthen(subcatch == sCatch,subcatch)
    
    self.Altitude=readmap(self.Dir + "/staticmaps/wflow_dem") * scalar(defined(subcatch)) #: The digital elevation map (DEM)
    self.TopoLdd=readmap(self.Dir + "/staticmaps/wflow_ldd.map")        #: The local drinage definition map (ldd)
    self.TopoId=readmap(self.Dir + "/staticmaps/wflow_subcatch.map")        #: Map define the area over which the calculations are done (mask)
    River=cover(boolean(readmap(self.Dir + "/staticmaps/wflow_river.map")),0) #: river network map. Fro those cell that belong to a river a specific width is used in the kinematic wave caulations
    self.RiverLength=pcrut.readmapSave(self.Dir + "/staticmaps/wflow_riverlength.map",0.0)
    # Factor to multiply riverlength with (defaults to 1.0)    
    self.RiverLengthFac=pcrut.readmapSave(self.Dir + "/staticmaps/wflow_riverlength_fact.map",1.0)

    self.OutputLoc=readmap(self.Dir + "/staticmaps/wflow_gauges.map")  #: Map with locations of output gauge(s)
    self.InflowLoc=nominal(pcrut.readmapSave(self.Dir + "/staticmaps/wflow_inflow.map",0.0))  #: Map with location of abstractions/inflows.
    self.SeepageLoc=pcrut.readmapSave(self.Dir + "/staticmaps/wflow_inflow.map",0.0)  #: Seapage from external model (if configured)
    
    if self.scalarInput:
        self.gaugesMap=readmap(self.Dir + "/staticmaps/wflow_mgauges.map") #: Map with locations of rainfall/evap/temp gauge(s). Only needed if the input to the model is not in maps
    self.OutputId=readmap(self.Dir + "/staticmaps/wflow_subcatch.map")       # location of subcatchment
  
    self.ZeroMap=0.0*scalar(subcatch)                    #map with only zero's
  
    # 3: Input time series ###################################################
    self.Rain_=self.Dir + "/inmaps/P" #: timeseries for rainfall
    self.PET_=self.Dir + "/inmaps/PET"          #: potential evapotranspiration
    self.Temp_=self.Dir + "/inmaps/TEMP"          #: temperature
    self.Inflow_=self.Dir + "/inmaps/IF" #: in/outflow locations (abstractions)
    self.Seepage_=self.Dir + "/inmaps/SE" #: in/outflow locations (abstractions)


    # Set static initial values here #########################################
   
    
    self.Latitude  =  ycoordinate(boolean(self.Altitude))
    self.Longitude =  xcoordinate(boolean(self.Altitude))
  
    self.logger.info("Linking parameters to landuse, catchment and soil...")

    self.Beta = scalar(0.6) # For sheetflow
    
    #self.M=lookupscalar(self.Dir + "/" + modelEnv['intbl'] + "/M.tbl" ,self.LandUse,subcatch,self.Soil) # Decay parameter in Topog_sbm    
    self.N=lookupscalar(self.Dir + "/" + self.intbl + "/N.tbl",self.LandUse,subcatch,self.Soil)  # Manning overland flow
    self.NRiver=lookupscalar(self.Dir + "/" + self.intbl + "/N_River.tbl",self.LandUse,subcatch,self.Soil)  # Manning river   
    
    
    # Determine real slope and cell length
    self.xl,self.yl,self.reallength = pcrut.detRealCellLength(self.ZeroMap,sizeinmetres)
    self.Slope= slope(self.Altitude)
    self.Slope=ifthen(boolean(self.TopoId),max(0.001,self.Slope*celllength()/self.reallength))
    Terrain_angle=scalar(atan(self.Slope))
   
    # Multiply parameters with a factor (for calibration etc) -P option in command line
    print multpars
    for k, v in multpars.iteritems():
        estr = k + "=" + k + "*" + str(v)
        self.logger.info("Parameter multiplication: " +  estr)
        exec estr
    
    self.N=ifthenelse(River, self.NRiver, self.N)
    

    # Determine river width from DEM, upstream area and yearly average discharge
    # Scale yearly average Q at outlet with upstream are to get Q over whole catchment
    # Alf ranges from 5 to > 60. 5 for hardrock. large values for sediments
    # "Noah J. Finnegan et al 2005 Controls on the channel width of rivers: 
    # Implications for modeling fluvial incision of bedrock"

    upstr = catchmenttotal(1, self.TopoLdd)
    Qscale = upstr/mapmaximum(upstr) * Qmax
    W = (alf * (alf + 2.0)**(0.6666666667))**(0.375) * Qscale**(0.375) * (max(0.0001,windowaverage(self.Slope,celllength() * 4.0)))**(-0.1875) * self.N **(0.375)
    RiverWidth = W
 
    # Which columns/gauges to use/ignore in kinematic wave updating   
    self.UpdateMap = self.ZeroMap 
    
    if self.updating:        
        touse = numpy.zeros(gaugear.shape,dtype='int')    
        for thecol in updateCols:
            idx = (gaugear == thecol).nonzero()
            touse[idx] = thecol
        self.UpdateMap = numpy2pcr(Nominal,touse,0.0)
        # Calulate distance to updating points (upstream) annd use to scale the correction
        # ldddist returns zer for cell at the gauges so add 1.0 tp result
        self.DistToUpdPt = cover(min(ldddist(self.TopoLdd,boolean(cover(self.UpdateMap,0)),1) * self.reallength/celllength(),self.UpdMaxDist),self.UpdMaxDist)


    # Initializing of variables
    self.logger.info("Initializing of model variables..")
    self.TopoLdd=lddmask(self.TopoLdd,boolean(self.TopoId))   
    catchmentcells=maptotal(scalar(self.TopoId))
 
    # Used to seperate output per LandUse/management classes
    #OutZones = self.LandUse
    #report(self.reallength,"rl.map")
    #report(catchmentcells,"kk.map")
    self.QMMConv = self.timestepsecs/(self.reallength * self.reallength * 0.001) #m3/s --> mm
    
    self.sumprecip=self.ZeroMap #: accumulated rainfall for water balance
    self.sumrunoff=self.ZeroMap                          #: accumulated runoff for water balance (weigthted for upstream area)
    self.sumlevel=self.ZeroMap                          #: accumulated level for water balance
    self.ForecQ_qmec=self.ZeroMap  # Extra inflow to kinematic wave reservoir for forcing in m^/sec
    self.KinWaveVolume=self.ZeroMap
    self.OldKinWaveVolume=self.ZeroMap
    self.Qvolume=self.ZeroMap
    self.Q=self.ZeroMap
    # cntd
   
    self.Aspect=scalar(aspect(self.Altitude))# aspect [deg]
    self.Aspect  = ifthenelse(self.Aspect <= 0.0 , scalar(0.001),self.Aspect)
    # On Flat areas the Aspect function fails, fill in with average...
    self.Aspect = ifthenelse (defined(self.Aspect), self.Aspect, areaaverage(self.Aspect,self.TopoId))

    

    # Set DCL to riverlength if that is longer that the basic length calculated from grid  
    drainlength = detdrainlength(self.TopoLdd,self.xl,self.yl)
    
    self.DCL=max(drainlength,self.RiverLength) # m
    # Multiply with Factor (taken from upscaling operation, defaults to 1.0 if no map is supplied
    self.DCL = self.DCL * max(1.0,self.RiverLengthFac)
    
    # water depth (m) 
    # set width for kinematic wave to cell width for all cells
    self.Bw=detdrainwidth(self.TopoLdd,self.xl,self.yl)
    # However, in the main river we have real flow so set the width to the 
    # width of the river
    
    self.Bw=ifthenelse(River, RiverWidth, self.Bw)
    
    # term for Alpha                             
    self.AlpTerm=pow((self.N/(sqrt(self.Slope))),self.Beta)
    # power for Alpha
    self.AlpPow=(2.0/3.0)*self.Beta
    # initial approximation for Alpha
    
    # Define timeseries outputs There seems to be a bug and the .tss files are 
    # saved in the current dir...
    tssName = os.path.join(self.Dir, "outtss", "exf")
    self.logger.info("Create timeseries outputs...")
    
    toprinttss = configsection(self.config,'outputtss')
    for a in toprinttss:
        tssName = self.Dir + "/" + self.runId + "/" +  self.config.get("outputtss",a)
        estr = "self." + self.config.get("outputtss",a) + "Tss=wf_TimeoutputTimeseries('" + tssName + "', self, self.OutputId,noHeader=False)"
        self.logger.info("Creating tss output: " + a + "(" + self.config.get('outputtss',a) + ")")
        exec estr

    self.runTss=wf_TimeoutputTimeseries(self.Dir + "/" + self.runId +  "/run",self, self.OutputLoc,noHeader=False)
    self.levTss=wf_TimeoutputTimeseries(self.Dir + "/" + self.runId  + "/lev",self, self.OutputLoc,noHeader=False)

    # Save some summary maps
    self.logger.info("Saving summary maps...")
    report(Terrain_angle,self.Dir + "/" + self.runId + "/outsum/angle.map")
    report(self.Slope,self.Dir + "/" + self.runId + "/outsum/slope.map")
    report(self.N,self.Dir + "/" + self.runId + "/outsum/N.map")
    report(self.xl,self.Dir + "/" + self.runId + "/outsum/xl.map")
    report(self.yl,self.Dir + "/" + self.runId + "/outsum/yl.map")
    report(self.reallength,self.Dir + "/" + self.runId + "/outsum/rl.map")
    report(self.DCL,self.Dir + "/" + self.runId + "/outsum/DCL.map")
    report(self.Bw,self.Dir + "/" + self.runId + "/outsum/Bw.map")
    report(ifthen(River,self.Bw),self.Dir + "/" + self.runId + "/outsum/RiverWidth.map")
    if self.updating:
        report(self.DistToUpdPt,self.Dir + "/" + self.runId + "/outsum/DistToUpdPt.map")
    
    

    self.SaveDir = self.Dir + "/" + self.runId + "/"
    self.logger.info("Starting Dynamic run...")


  def resume(self):
    """ read initial state maps (they are output of a previous call to suspend()) """
    
    if self.reinit == 1:
        self.logger.info("Setting initial conditions to default (zero!)")
        self.FreeWater =  cover(0.0) #: Water on surface (state variable [mm])
        self.SurfaceRunoff = cover(0.0) #: Discharge in kinimatic wave (state variable [m^3/s])
        self.WaterLevel = cover(0.0) #: Water level in kinimatic wave (state variable [m])
    else:
        self.wf_resume(self.Dir + "/instate/")

    P=self.Bw+(2.0*self.WaterLevel)
    self.Alpha=self.AlpTerm*pow(P,self.AlpPow)

    self.OldSurfaceRunoff = self.SurfaceRunoff
    #self.SurfaceRunoffMM=self.SurfaceRunoff * modelEnv['timestepsecs']/(self.reallength * self.reallength * 0.001) #mm
    self.SurfaceRunoffMM=self.SurfaceRunoff * self.QMMConv
        # Determine initial kinematic wave volume
    self.KinWaveVolume = self.WaterLevel * self.Bw * self.DCL
    self.OldKinWaveVolume = self.KinWaveVolume
    
    
  def dynamic(self):
    
    self.logger.debug("Step: "+str(int(self.thestep + self._d_firstTimeStep))+"/"+str(int(self._d_nrTimeSteps)))
    self.thestep = self.thestep + 1

    
    if self.scalarInput:
            # gaugesmap not yet finished. Should be a map with cells that
            # hold the gauges with an unique id
            Precipitation = timeinputscalar(self.precipTss,self.gaugesMap)
            Inflow = cover(0.0)
            #Seepage = cover(timeinputscalar(self.SeepageTss,self.SeepageLoc),0)
            Precipitation = pcrut.interpolategauges(Precipitation,self.interpolMethod)
            PotEvaporation=timeinputscalar(self.evapTss,self.gaugesMap)
            PotEvaporation = pcrut.interpolategauges(PotEvaporation,self.interpolMethod)
            #self.report(PotEvaporation,'p')
            Temperature=timeinputscalar(self.tempTss,self.gaugesMap)
            Temperature = pcrut.interpolategauges(Temperature,self.interpolMethod)
            Temperature = Temperature + self.TempCor
    else:
            Precipitation=cover(self.readmap(self.Rain_,0.0,style=self.P_style),0.0)
            PotEvaporation=cover(self.readmap(self.PET_,0.0,style=self.PET_style),0.0)
            #Inflow=cover(self.readmap(self.Inflow),0)
            # These ar ALWAYS 0 at present!!!
            Inflow=pcrut.readmapSave(self.Inflow_,0.0)
            if self.ExternalQbase:
                Seepage = cover(self.readmap(self.Seepage_,0.0),0.0)
            else:
                Seepage=cover(0.0)
            Temperature=self.readmap(self.Temp_,0.0,style=self.TEMP_style)
            Temperature = Temperature + self.TempCor
            #Inflow=spatial(scalar(0.0))
    
    # Multiply input parameters with a factor (for calibration etc) -p option in command line
    for k, v in multdynapars.iteritems():
        estr = k + "=" + k + "*" + str(v)
        self.logger.debug("Dynamic Parameter multiplication: " +  estr)
        exec estr    
    

    # Direct runoff generation
    if self.ExternalQbase:
        DirectRunoffStorage=self.QuickFlow+Seepage+self.RealQuickFlow
    else:
        DirectRunoffStorage=self.QuickFlow+self.BaseFlow+self.RealQuickFlow

    InwaterMM=max(0.0,DirectRunoffStorage)
    Inwater=(InwaterMM * self.reallength * self.reallength * 0.001) / self.timestepsecs # m3/s
    Inwater=Inwater + Inflow # Add abstractions/inflows in m^3/sec
    
    ##########################################################################
    # Runoff calculation via Kinematic wave ##################################
    ##########################################################################
    # per distance along stream
    q=Inwater/self.DCL + self.ForecQ_qmec/self.DCL
    self.OldSurfaceRunoff=self.SurfaceRunoff
    self.SurfaceRunoff = kinematic(self.TopoLdd, self.SurfaceRunoff,q,self.Alpha, self.Beta,self.Tslice,self.timestepsecs,self.DCL) # m3/s
    self.SurfaceRunoffMM=self.SurfaceRunoff*self.QMMConv # SurfaceRunoffMM (mm) from SurfaceRunoff (m3/s)
    
    self.updateRunOff()
    InflowKinWaveCell=upstream(self.TopoLdd,self.SurfaceRunoff)
    self.MassBalKinWave = (self.KinWaveVolume - self.OldKinWaveVolume)/self.timestepsecs  + InflowKinWaveCell + Inwater - self.SurfaceRunoff
    Runoff=self.SurfaceRunoff

    # Updating
    # --------
    # Assume a tss file with as many columns as outpulocs. Start updating for each non-missing value and start with the
    # first column (nr 1). Assumes that outputloc and columns match!

    if self.updating:
        QM = timeinputscalar(self.updateFile, self.UpdateMap) * self.QMMConv
        
        # Now update the state. Just add to the Ustore
        # self.UStoreDepth =  result
        # No determine multiplication ratio for each gauge influence area.
        # For missing gauges 1.0 is assumed (no change).
        # UpDiff = areamaximum(QM,  self.UpdateMap) - areamaximum(self.SurfaceRunoffMM, self.UpdateMap)
        UpRatio = areamaximum(QM,  self.UpdateMap)/areamaximum(self.SurfaceRunoffMM, self.UpdateMap)
    
        UpRatio = cover(areaaverage(UpRatio,self.TopoId),1.0)
        # Now split between Soil and Kyn  wave
        UpRatioKyn = min(MaxUpdMult,max(MinUpdMult,(UpRatio - 1.0) * UpFrac + 1.0))
        UpRatioSoil = min(MaxUpdMult,max(MinUpdMult,(UpRatio - 1.0) * (1.0 - UpFrac) + 1.0))
        
        # update/nudge self.UStoreDepth for the whole upstream area, 
        # not sure how much this helps or worsens things
        #if UpdSoil:      
        #    toadd = min((self.UStoreDepth * UpRatioSoil) - self.UStoreDepth,StorageDeficit * 0.95)
        #    self.UStoreDepth = self.UStoreDepth + toadd
        
        # Update the kinematic wave reservoir up to a maximum upstream distance
        # TODO:  add (much smaller) downstream updating also?
        MM = (1.0 - UpRatioKyn)/self.UpdMaxDist
        UpRatioKyn = MM * self.DistToUpdPt + UpRatioKyn
        
        self.SurfaceRunoff = self.SurfaceRunoff *  UpRatioKyn
        self.SurfaceRunoffMM=self.SurfaceRunoff*self.QMMConv # SurfaceRunoffMM (mm) from SurfaceRunoff (m3/s)
        # water depth (m) 
        #self.WaterLevel=(self.Alpha*(self.SurfaceRunoff**self.Beta))/self.Bw    
        # # wetted perimeter (m)
        #P=self.Bw+2*self.WaterLevel
        # Alpha  
        #self.Alpha=self.AlpTerm*(P**self.AlpPow)
        self.updateRunOff()
        Runoff=self.SurfaceRunoff
        

    
    self.sumprecip=self.sumprecip  +  Precipitation                     #accumulated rainfall for water balance
    self.sumevap=self.sumevap + ActEvap                           #accumulated evaporation for water balance
    self.sumpotevap=self.sumpotevap + PotEvaporation 
    self.sumptemp=self.sumtemp + Temperature
    self.sumrunoff=self.sumrunoff  + self.SurfaceRunoffMM                        #accumulated runoff for water balance
    self.sumlevel=self.sumlevel  + self.WaterLevel
    
    # sample timeseries
    # Do runoff and level always
    self.runTss.sample(Runoff)
    self.levTss.sample(self.WaterLevel)
    # Get rest from ini file
    toprinttss = configsection(self.config,'outputtss')
    for a in toprinttss:
        estr = "self." + self.config.get("outputtss",a) + "Tss.sample(" + a +")"
        eval(estr)

    # Print .ini defined outputmaps per timestep
    toprint = configsection(self.config,'outputmaps')
    for a in toprint:
        eval("self.report(" + a  + ", self.Dir + \"/\" + self.runId + \"/outmaps/" + self.config.get("outputmaps",a) +"\")")



# The main function is used to run the program from the command line

def main():  
    """
    Perform command line execution of the model.
    """      
    global multpars
    caseName = "default_hbv"
    runId = "run_default"
    configfile="wflow_wave.ini"
    _lastTimeStep = 10
    _firstTimeStep = 1
    fewsrun=False
    runinfoFile="runinfo.xml"
    timestepsecs=86400
    wflow_cloneMap = 'wflow_subcatch.map'
    NoOverWrite=1
    
    ## Main model starts here
    ########################################################################
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'QXS:F:hC:Ii:T:NR:u:s:P:p:Xx:U:f')
    except getopt.error, msg:
        pcrut.usage(msg)
    
    for o, a in opts:
        if o == '-F': 
            runinfoFile = a
            fewsrun = True
        if o == '-P': 
            exec ("multpars =" + a,globals(), globals())
        if o == '-p': 
            exec "multdynapars =" + a
            exec ("multdynapars =" + a,globals(), globals())
        if o == '-C': caseName = a
        if o == '-R': runId = a
        if o == '-c': configfile = a
        if o == '-s': timestepsecs = int(a)
        if o == '-T': _lastTimeStep=int(a)
        if o == '-S': _firstTimeStep=int(a)
        if o == '-h': usage()
        if o == '-f': NoOverWrite = 0
        

     
    if fewsrun: 
        _lastTimeStep =  wflow_adapt.getTimeStepsfromRuninfo(runinfoFile) * 86400/timestepsecs
        _firstTimeStep = 1
        
    myModel = WflowModel(wflow_cloneMap, caseName,runId,configfile)
    dynModelFw = wf_DynamicFramework(myModel, _lastTimeStep,firstTimestep=_firstTimeStep)
    dynModelFw.createRunId(NoOverWrite=NoOverWrite)    
    
    for o, a in opts:
        if o == '-X': configset(myModel.config,'model','OverWriteInit','1',overwrite=True) 
        if o == '-I': configset(myModel.config,'model','reinit','1',overwrite=True) 
        if o == '-i': configset(myModel.config,'model','intbl',a,overwrite=True)
        if o == '-s': configset(myModel.config,'model','timestepsecs',a,overwrite=True)
        if o == '-x': configset(myModel.config,'model','sCatch',a,overwrite=True)
        if o == '-c': configset(myModel.config,'model','configfile', a,overwrite=True)
        if o == '-M': configset(myModel.config,'model','MassWasting',"1",overwrite=True)
        if o == '-N': configset(myModel.config,'model','nolateral','1',overwrite=True) 
        if o == '-Q': configset(myModel.config,'model','ExternalQbase','1',overwrite=True)
        if o == '-U': 
            configset(myModel.config,'model','updateFile',a,overwrite=True)
            configset(myModel.config,'model','updating',"1",overwrite=True)
        if o == '-u': 
            print a
            exec "updateCols =" +  a

        
    
    #dynModelFw.run()
    dynModelFw._runInitial()
    dynModelFw._runResume()
    dynModelFw._runDynamic(_firstTimeStep,_lastTimeStep)
    dynModelFw._runSuspend()
    
    
    fp = open(caseName + "/" + runId + "/runinfo/configofrun.ini",'wb')
    myModel.config.write(fp )
    
    
    
    os.chdir("../../")


if __name__ == "__main__":
    main()
