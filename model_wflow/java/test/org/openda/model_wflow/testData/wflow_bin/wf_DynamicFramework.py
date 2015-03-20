# -*- coding: utf-8 -*-
"""
wf_DynamicFramework
-------------------

This is a replacement for the standard pcraster/python DynamicFramwork class.\
It provides extra functionality to simplify linking the models build in the framwork
with other models. The provided functionality allows external programs to control
and interrogate the model.

Compared to the original pcraster class the usermodel needs to be extended with
the stateVariables() method which lists the state variables. Other items to be
exchanged most be listed in the API section of the .ini file

In addition, the following methods must also be called at  startup:
    

    - createRunId()

.. todo::
    
    add automatic reporting of variables set in the ini file

$Author: schelle $
$Id: wf_DynamicFramework.py 545 2012-11-27 18:06:21Z schelle $
$Rev: 545 $
"""
import numpy
import ConfigParser
from wf_Timeoutput import *
import pcrut
import shutil, glob
try:
    import PCRaster
    from PCRaster import *
    from PCRaster.Framework import *
    from PCRaster.NumPy import *
except ImportError:
    import pcraster as PCRaster
    from pcraster import *
    from pcraster.framework import *
    #from pcraster.numpy import *
    
from wflow_lib import *
import scipy.io


class wf_exchnageVariables():
    """
    List of exchange variables
    """
    
    def __init__(self):
        self.vars = []
    

    def varexists(self,name):

        exists = 0
        for item in self.vars:
            if item[0] == name:
                exists = 1
                
        return exists

            
    def addvar(self,name,role,unit):
        

        if not self.varexists(name):        
            tvar = [name,role,unit]
            self.vars.append(tvar)


    def getvars(self):
        
        return self.vars 
        
    def getvarStyle(self,name):
        """
        returns 2 if this is a input variable to be set from api otherwise 1
        ( in the ini 0 is for in memory variables)
        
        """
        for xx in self.vars:        
            if xx.__contains__(name):
                if xx[1] == 0:
                    return 2
                else:
                    return 1
        return 1   
        


class wf_DynamicFramework(frameworkBase.FrameworkBase):
  ## \brief Constructor
  #
  # \param userModel class containing the user model
  # \param lastTimeStep last timestep to run
  # \param firstTimestep sets the starting timestep of the model (optional,
  #        default is 1)
  #
  def __init__(self, userModel, lastTimeStep=0, firstTimestep=1):
    frameworkBase.FrameworkBase.__init__(self)
    
    self.exchnageitems = wf_exchnageVariables()
    
    self._d_model = userModel
    self._testRequirements()

    if firstTimestep > lastTimeStep:
      msg = "Cannot run dynamic framework: Start timestep smaller than end timestep"
      raise frameworkBase.FrameworkError(msg)

    # fttb
    self._addMethodToClass(self._readmapNew)
    self._addMethodToClass(self._reportNew)
    self._addMethodToClass(self.wf_suspend)
    self._addMethodToClass(self.wf_resume)
    self._addMethodToClass(self.wf_readmap)


    self._userModel()._setNrTimeSteps(lastTimeStep)
    self._d_firstTimestep = firstTimestep
    self._userModel()._setFirstTimeStep(self._d_firstTimestep)
    


  def loggingSetUp(self,caseName,runId,logfname,model,modelversion):
    """
    Sets up the logging system assuming we are in the runId directory
    """

    # Set logging
    logfile= caseName + "/" + runId +"/" + logfname
    logger = pcrut.setlogger(logfile,model)
    logger.info(model + " " +  modelversion + " Case: " + caseName + " Runid: " + runId)

    

    return logger
    
  def createRunId(self,intbl="intbl",logfname="wflow.log",NoOverWrite=True,model="model",modelVersion="no version"):   
    """
    Create runId dir and copy table files to it
    Also changes the working dir to the case/runid directory
    """
    
    caseName = self._userModel().caseName
    runId = self._userModel().runId

    configfile=self._userModel().configfile
    if not os.path.isdir(caseName +"/" + runId):
        os.makedirs(caseName +"/" + runId + "/outmaps/")
        os.makedirs(caseName +"/" + runId + "/outstate/")
        os.makedirs(caseName +"/" + runId + "/outsum/")
        os.makedirs(caseName +"/" + runId + "/intbl/")
        os.makedirs(caseName +"/" + runId + "/runinfo/")
    else:
        if os.path.exists(caseName +"/" + runId +"/run.tss"):
            if NoOverWrite:
                print "ERROR: refusing to overwrite an existing run: " + caseName +"/" + runId +"/run.tss"
                exit(1)
                    
    for file in glob.glob(caseName + "/" + intbl + "/*.tbl"):
        shutil.copy(file, caseName +"/" + runId + "/" + intbl)
    shutil.copy(caseName + "/" + configfile, caseName +"/" + runId + "/runinfo") 
    #os.chdir(caseName +"/" + runId)
    
    self._userModel().logger = self.loggingSetUp(caseName,runId,logfname,model,modelVersion)
    self._userModel().config = self.iniFileSetUp(caseName,runId,configfile)
    
    self.outputFormat = int(configget(self._userModel().config,'framework','outputformat','1'))

    
  
  def wf_suspend(self, directory):
      """
      Suspend the state variables to disk as .map files
      
      """
      allvars = self._userModel().stateVariables()
      
      for var in allvars:
          exec "report(self._userModel()." + var + ",\"" + directory + "/" + var + ".map\")"

  def wf_savedynMaps(self):
      """
      Save the maps defined in the ini file for the dynamic section
      
      .. todo::
          
          Save maps to be used in memory at startup and do not call the ini file each time
          
      """
      # Print .ini defined outputmaps per timestep
      toprint = configsection(self._userModel().config,'outputmaps')
      for a in toprint:
          b = a.replace('self','self._userModel()')
          eval("self._userModel().report(" + b  + ", self._userModel().Dir + \"/\" + self._userModel().runId + \"/outmaps/" + self._userModel().config.get("outputmaps",a) +"\")")
      
      
  def wf_resume(self, directory):
      """
      Resumes the state variables from disk as .map files
      
      """
      allvars = self._userModel().stateVariables()
      
      for var in allvars:
          exec "self._userModel()." + var + "= readmap(\"" + directory + "/" + var + ".map\")"
               
 
  def wf_QuickSuspend(self):
      """
      Save the state variable of the current timestep in memory
      it uses the wf_supplyVariableNamesAndRoles() function to find them.
      The variables are inserted into the model object
      This function is normally caled as part of the run. Normally there is
      no need to call it directly.
      
          
      """
      allvars = self._userModel().stateVariables()
      
      for var in allvars:
          exec "self._userModel()." + var + "_laststep = self._userModel()." +  var
              
              
  def wf_QuickResume(self):
      """
      Resumes the state variable of the previous timestep in memory
      it uses the wf_supplyVariableNamesAndRoles() function to find them.
      The variables are inserted into the model object
      
     
      """
      allvars = self._userModel().stateVariables()
      
      for var in allvars:
          exec "self._userModel()." + var + " = self._userModel()." +  var + "_laststep"
  
    
  def iniFileSetUp(self,caseName,runId,configfile):
    """
    Reads .ini file and returns a config object. 

    
    Input:
        - caseName - dir with case
        - runId - run dir within case
        - configfile - name of the configfile (.ini type)
        
    Output:
        - python config object
        
    """

    config = ConfigParser.SafeConfigParser()
    config.optionxform = str
    config.read(caseName + "/" + configfile)
       
    return config  


  def wf_setValuesAsNumpy(self,mapname,values):
      """
      set a map with values from a numpy array. Current settings for
      dimensions are assumed.
      
      Input: 
          - mapname - string with name of map
          - values - numpy array
      """
      
      
      arpcr = numpy2pcr(Scalar,values,1E31)
      
      if hasattr(self._userModel(), mapname):
          exec "self._userModel()." + mapname + " = arpcr"
      else:
          self.showWarning(mapname + " is not defined in the usermodel: setting anyway")
          exec "self._userModel()." + mapname + " = arpcr"
          
          
  def wf_setValues(self,mapname,values):
      """
      set a map with values from a python list or a single scalar 
      value. In case a single value is specified the value will be distributed
      uniformly over the map. Current settings for
      dimensions are assumed.
      
      Input: 
          - mapname - string with name of map
          - values - single list of value of length rows * cols or a single
             scalar
      """
      if isinstance(values,list):
      	      ar = array(values)
      	      ar.reshape(getrows(),getcols())
      	      arpcr = numpy2pcr(Scalar,ar.reshape(getrows(),getcols()),1E31)
      else:
      	      arpcr = cover(values)
      
      if hasattr(self._userModel(), mapname):
          exec "self._userModel()." + mapname + " = arpcr"
      else:
          self.showWarning(mapname + " is not defined in the usermodel: setting anyway")
          exec "self._userModel()." + mapname + " = arpcr"

#TODO: add getrowcol
     	      
  def wf_setValueRowCol(self,mapname,value,row,col):
      """
      set single value in a map on row, col (0 based). All other values in the 
      map remain the same. Numbering starts at the upper left corner.
      
      Input: 
          - mapname - string with name of map
          - row - row to set the value in
          - col - column to set the value in
          - values - single scalar
         
      Output:
          - nothing
      """

      if hasattr(self._userModel(), mapname):
          exec "ar = pcr2numpy(self._userModel()." + mapname +",1E31)"
          ar[row,col] = value
          arpcr = numpy2pcr(Scalar,ar,1E31)                
          exec "self._userModel()." + mapname + " = arpcr"
      else:
           self.showWarning(mapname + " is not defined in the usermodel")


  def wf_setValue(self,mapname,value,xcor,ycor):
      """
      set single value in a map on xcor, ycor (0 based). All other values in the 
      map remain the same.
      
      Input: 
          - mapname - string with name of map
          - xcor - xcor to set the value in
          - ycor - ycor to set the value in
          - value - single scalar
          
      
      Output:
          - nothing    
      """
      
      if hasattr(self._userModel(), mapname):
          exec "pcrmap = self._userModel()." + mapname
          row, col = getRowColPoint(pcrmap,xcor,ycor)
          ar = pcr2numpy(pcrmap,1E31)
          ar[row,col] = value
          arpcr = numpy2pcr(Scalar,ar,1E31)                
          exec "self._userModel()." + mapname + " = scalar(arpcr)"
      else:
           self.showWarning(mapname + " is not defined in the usermodel")
    

  def wf_setValueLdd(self,mapname,value,xcor,ycor):
      """
      set single value in an ldd on xcor, ycor (0 based). All other values in the 
      map remain the same. Calls lddrepair to ensure the ldd is sound
      
      Input: 
          - mapname of tipy ldd - string with name of map
          - xcor - xcor to set the value in
          - ycor - ycor to set the value in
          - values - single scalar (see pcraster ldddescription for legal values)
                      e.g. use 5 for setting a pit
      
      Output:
          - nothing
      """

      if hasattr(self._userModel(), mapname):
          exec "pcrmap = self._userModel()." + mapname
          ar = pcr2numpy(pcrmap,1E31)
          row, col = getRowColPoint(pcrmap,xcor,ycor)
          ar[row,col] = value
          arpcr = numpy2pcr(Scalar,ar,1E31)                
          exec "self._userModel()." + mapname + " = lddrepair(ldd(arpcr))"
      else:
           self.showWarning(mapname + " is not defined in the usermodel")
    
        		      	   
  
  def wf_multParameterValues(self,mapname,value):
      """
      multiply a parameter map with a single scalar 
      value. Current settings for dimensions are assumed.
      
      This method must be called *after* the runinitial() method
      
      Input: 
          - mapname - string with name of map
          - value - single scalar
    
      Output:
          - nothing
      """
      

      arpcr = cover(value)
	      
      if hasattr(self._userModel(), mapname):
	      exec "self._userModel()." + mapname + " = arpcr * " + "self._userModel()." + mapname
      else:
	      self.showWarning(mapname + " is not defined in the usermodel")


       
  def wf_setParameterValues(self,mapname,values):
      """
      set a parameter map with values from a python list or a single scalar 
      value. In case a single value is specified the value will be distributed
      uniformly over the map. Current settings for dimensions are assumed.
      
      This method must be called _after_ the runinitial() method
      
      Input: 
          - mapname - string with name of map
          - values - single list of value of length rows * cols or a single
             scalar
      
      Output:
          - nothing
          
          
      """
      
      if isinstance(values,list):
	      ar = array(values)
	      
	      ar.reshape(getrows(),getcols())
	      arpcr = numpy2pcr(Scalar,ar.reshape(getrows(),getcols()),1E31)
      else:
      	      arpcr = cover(values)
	      
      if hasattr(self._userModel(), mapname):
	      exec "self._userModel()." + mapname + " = arpcr"
      else:
	      self.showWarning(mapname + " is not defined in the usermodel")
      	      

  def wf_supplyParameterAsList(self,mapname):
      """
      Returns a python list for the specified parameter map and the current
      timestep. If the maps is not dynamic the current status of the map is
      returned.
      
      Input: 
          - mapname (string)
          
      Output: 
          - list
      """
      if hasattr(self._userModel(), mapname):
      	      exec "retval = pcr2numpy(self._userModel()." + mapname + ",1E31)"
      	      return retval.flatten().tolist() 
      else:
      	      self.showWarning(mapname + " is not defined in the usermodel")
      	      return []


  def wf_supplyMapAsList(self,mapname):
      """
      Returns a python list for the specified map and the current
      timestep. If the maps is not dynamic the current status of the map is
      returned which may be undefined for maps that are filled with data 
      at the end of a run
      
      
      Input: 
          - mapname (string)
          
      Output: 
          - list
      """
      
      if hasattr(self._userModel(), mapname):
      	      exec "retval = pcr2numpy(self._userModel()." + mapname + ",1E31)"
      	      return retval.flatten().tolist() 
      else:
      	      self.showWarning(mapname + " is not defined in the usermodel")
      	      return []   


  def wf_supplyMapAsNumpy(self,mapname):
      """
      Returns a numpy array (matrix) for the specified map and the current
      timestep. If the maps is not dynamic the current staus of the map is
      returns which may be undefined for maps that are filled with data 
      at the end of a run
      
      Input: 
          - mapname (string)
          
      Output: 
          - numpy array
      """
      
      exec "retval = pcr2numpy(self._userModel()." + mapname + ",1E31)"
      return retval
      
    
  def wf_supplyGridDim(self):
      """
      return the dimension of the current model grid as list::
      
       [ Xul, Yul, xsize, ysize, rows, cols]
      """
      
      return getgridparams()
      

  def wf_supplyVariableNamesAndRoles(self):
      """
      Returns a list of variables
      List of list with the following structure::
          
          [[ name, role, unit]
          [ name, role, unit]
          ...   
          ]
          role: 0 = input (to the model)
                1 = is output (from the model)
                2 = input/output (state information)
                3 = model parameter
          unit: 0 = mm/timestep
                1 = m^3/sec
                2 = m
                3 = degree Celcius
                4 = mm
                5 = -
    
      The first time this function is called the exchangeitems object is filled
      with data from the ini file.
      """
      
      
      res = self.exchnageitems.getvars()
      
      # Fill object with data from ini file
      if size(res) == 0:
          API = configsection(self._userModel().config,'API')
          for a in API:
              tt = []
              line = self._userModel().config.get("API",a)
              tt.append(a)
              tt.append(int(line.split(',')[0]))
              tt.append(int(line.split(',')[1]))
              res.append(tt)
              self.exchnageitems.addvar(tt[0],tt[1],tt[2])
          
      if hasattr(self._userModel(), 'supplyVariableNamesAndRoles'):
          return self._userModel().supplyVariableNamesAndRoles()
      else:
          return res

              


  def wf_supplyVariableNames(self):
      """
      returns:
          - the a list of variable names
          
      """
      
  
      varlist= self.wf_supplyVariableNamesAndRoles()
      ret = range(len(varlist))
      for ss in range(len(varlist)):
          ret[ss] = varlist[ss][0]

      return ret
  

  def wf_supplyVariableRoles(self):
      """
      returns:
          - the a list of variable roles
      """
      
   
      varlist= self.wf_supplyVariableNamesAndRoles()
      ret = range(len(varlist))
      for ss in range(len(varlist)):
          ret[ss] = varlist[ss][1]
 
      return ret
 

  def wf_supplyVariableCount(self):
      """
      returns:
          - the number of exchangable variables
      """
      
     
      varlist= self.wf_supplyVariableNamesAndRoles()
              
      return len(varlist)
     

  def wf_supplyVariableUnits(self):
      """
      returns:
          - the a list of variable units
      """
      

      varlist= self.wf_supplyVariableNamesAndRoles()
      ret = range(len(varlist))
      for ss in range(len(varlist)):
          ret[ss] = varlist[ss][2]

      return ret
          


  def wf_supplyCurrentTime(self):
      """
      gets the current time in seconds after the start of the run
      Assumed daily timesteps if not defined in the user model
      
      Output:
         - current model time (since start of the run)
      
      .. todo::
      	
      	Get timestep info from from config file
      	
      """
      if hasattr(self._userModel(), 'supplyCurrentTime'):
          return  self._userModel().supplyCurrentTime()            
      else:
          return self._userModel().self.currentTimeStep() * 86400

         
  def wf_supplyRowCol(self,mapname,xcor,ycor):
      """ 
      returns a tuple (Row,Col) for the given X and y coordinate
      
      Input:
          - mapname
          - xcor
          - ycor
          
      Output:
          - tuple with row, col
      """
      return getRowColPoint(mapname,xcor,ycor)
     
  def wf_supplyScalar(self,mapname,xcor,ycor):
      """
      returns a single value for the x and y coordinates from the
      map given uses getValAtPoint(in_map,xcor,ycor) from terrain_lib.py
      
      Input:
          - mapname
          - xcor
          - ycor
          
      Output:
          - value at location xcor, ycor
      
      """
      exec "pcmap = self._userModel()." + mapname
      
      return getValAtPoint(pcmap,xcor,ycor)   

 
  def wf_supplyScalarRowCol(self,mapname,row,col):
      """
      returns a single value for row and col from the
      map given.      
      
      Input:
          - mapname
          - xcor
          - ycor
          
      Output:
          - value at location row, col
      """
      
      exec "pcmap = self._userModel()." + mapname
      
      npmap = pcr2numpy(pcmap,NaN)
      
      return npmap[row,col]    
   


  def _userModel(self):
    """ Returns the class provided by the user """
    return self._d_model

  def _runDynamic(self,firststep,laststep):
    """
    Runs the dynamic model from firststep to laststep
    
    Input:
        
        :ivar firststep: first timestep of the model run
        :ivar laststep: last timestep of the model run
        
    """
    self._userModel()._setInDynamic(True)
    self._userModel()._setNrTimeSteps(laststep)
    step = firststep

    while step <= self._userModel().nrTimeSteps():

      self._incrementIndentLevel()
      self._atStartOfTimeStep(step)
      self._userModel()._setCurrentTimeStep(step)
      if hasattr(self._userModel(), 'dynamic'):
        self._incrementIndentLevel()
        self._traceIn("dynamic")
        self._userModel().dynamic()
        self._traceOut("dynamic")
        self._decrementIndentLevel()
        # Save state variables in memory
        self.wf_QuickSuspend()
        self.wf_savedynMaps()

      self._timeStepFinished()
      self._decrementIndentLevel()
      step += 1

    self._userModel()._setInDynamic(False)





  ## \brief Re-implemented from ShellScript.
  #
  # Runs a dynamic user model.
  def run(self):
    """ Runs the dynamic model for all timesteps """
    
    self._atStartOfScript()
    if(hasattr(self._userModel(), "resume")):
      if self._userModel().firstTimeStep() == 1:
        self._runInitial()
      else:
        self._runResume()
    else:
      self._runInitial()

    self._runDynamic()

    # only execute this section while running filter frameworks
    if hasattr(self._userModel(), "suspend") and hasattr(self._userModel(), "filterPeriod"):
      self._runSuspend()

    return 0
    
  def _reportNew(self, variable, name, style=1):
    """
    outputformat: (set in the [framework] section of the init file). 
        1: pcraster
        2: numpy (compressed)
        3: matlab
        4: numpy text files (large and slow)
        
        ..
        # Example:
            
        [framework]
        outputformat = 4
    """
    head, tail = os.path.split(name)

    if re.search("\.", tail):
      msg = "File extension given in '" + name + "' not allowed, provide filename without extension"
      raise FrameworkError(msg)


    directoryPrefix = ""
    nameSuffix = ".map"
    newName = ""

    if hasattr(self._userModel(), "_inStochastic"):
      if self._userModel()._inStochastic():
        if self._userModel()._inPremc():
          newName = name + nameSuffix
        elif self._userModel()._inPostmc():
          newName = name + nameSuffix
        else:
          directoryPrefix = str(self._userModel().currentSampleNumber())

    if self._userModel()._inInitial():
      newName = name + nameSuffix

    if hasattr(self._userModel(), "_inDynamic"):
      if self._userModel()._inDynamic() or self._inUpdateWeight():
        newName = generateNameT(name, self._userModel().currentTimeStep())

    path = os.path.join(directoryPrefix, newName)
    
    if self.outputFormat == 1:
        try:
            import PCRaster
        except ImportError:
            import pcraster as PCRaster
        PCRaster.report(variable, path)    
    elif self.outputFormat == 2:
        numpy.savez(path,pcr2numpy(variable,-999))
    elif self.outputFormat == 3:
        scipy.io.savemat(path,mdict={str(self._userModel().currentTimeStep()) : pcr2numpy(variable,-999)})
    elif self.outputFormat == 4:
        numpy.savetxt(path,pcr2numpy(variable,-999),fmt="%0.6g")
    
    
    
    
  def wf_readmap(self, name, default):
    """
      Adjusted version of readmapNew. the style variable is used to indicated
      how the data is read::
          
          1 - default: reads pcrmaps
          2 - memory: assumes the map is made available (in memory) using
          the in-memory interface
          
      .. note:
          
          the style variable is set using the variable list from the API
          section in the ini file
          
    """
    directoryPrefix = ""
    nameSuffix = ".map"
    newName = ""
    
    varname = os.path.basename(name)        
    
    # check and if neede file the variable name object    
    thevars = self.exchnageitems.getvars()
    if size(thevars) == 0:
        self.wf_supplyVariableNamesAndRoles()
    
    style = self.exchnageitems.getvarStyle(varname)
    #print varname + " : " + str(style)

    
    if hasattr(self._userModel(), "_inStochastic"):
      if self._userModel()._inStochastic():
        if self._userModel()._inPremc() or self._userModel()._inPostmc():
          newName = name + nameSuffix
        else:
          directoryPrefix = str(self._userModel().currentSampleNumber())

    if hasattr(self._userModel(), "_inInitial"):
      if self._userModel()._inInitial():
        newName = name + nameSuffix

    if self._inResume():
      timestep = self._userModel().firstTimeStep()
      newName = generateNameT(name, timestep - 1)

    if hasattr(self._userModel(), "_inDynamic"):
      if self._userModel()._inDynamic() or self._inUpdateWeight():
        timestep = self._userModel().currentTimeStep()
        newName = generateNameT(name, timestep)

    if style==1:
        path = os.path.join(directoryPrefix, newName)
        assert path is not ""
        try:
            import PCRaster
        except ImportError:
            import pcraster as PCRaster
        if os.path.isfile(path):
            return cover(PCRaster.readmap(path),default)
        else:
            #logger.warn("returning 0.0")
            return scalar(default)  
    elif style == 2:
        # first get basename (last bit of path)
        name = os.path.basename(name)
        if hasattr(self._userModel(),name):
            exec "retval = self._userModel()." + name
            return retval
        else:
            exec "self._userModel()." + name + " = cover(scalar(default))"
            exec "retval = self._userModel()." + name
            return retval
    else:
        return cover(scalar(default))


  ## \brief testing the requirements for the dynamic framework
  #
  # To use the dynamic framework the user must implement the following methods
  # in this class:
  # - either "run" or "initial" and "dynamic"
  def _testRequirements(self):
    if hasattr(self._userModel(), "_userModel"):
      msg = "The _userModel method is deprecated and obsolete"
      self.showWarning(msg)

    if( not hasattr(self._userModel(), "dynamic") and not hasattr(self._userModel(), "run")):
      msg = "Cannot run dynamic framework: Implement dynamic method"
      raise frameworkBase.FrameworkError(msg)


    if not hasattr(self._userModel(), "initial"):
      if self._debug():
        self.showWarning("No initial section defined.")

  def setQuiet(self,quiet=True):
    self.showWarning("I'm not going to be quiet!!!!")
