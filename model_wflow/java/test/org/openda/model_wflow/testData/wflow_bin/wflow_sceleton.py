#!/usr/bin/python

"""
Definition of the wflow_sceleton model.
---------------------------------------

This simple model calculates soil temperature using
air temperature as a forcing.

$Author: schelle $
$Id: wflow_sceleton.py 542 2012-11-27 18:00:43Z schelle $
$Rev: 542 $
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



class WflowModel(DynamicModel):  
  """
  The user defined model class. This is your work!
  """
  
  def __init__(self, cloneMap,Dir,RunDir,configfile):
      """
      *Required*
      
      The init function **must** contain what is shown below. Other functionality
      may be added if needed.
      
      """
      DynamicModel.__init__(self)   
      setclone(Dir + "/staticmaps/" + cloneMap)
      self.runId=RunDir      
      self.caseName=Dir
      self.Dir = Dir
      self.configfile = configfile

  def stateVariables(self):
      """ 
      *Required*
      
      Returns a list of state variables that are essential to the model. 
      This list is essential for the resume and suspend functions to work.
      
      This function is specific for each model and **must** be present. This is
      where you specify the state variables of you model. If your model is stateless
      this function must return and empty array (states = [])
      
      In the simple example here the TSoil variable is a state 
      for the model.
      
      """
      states = ['TSoil']
      
      return states
      
      
  def supplyCurrentTime(self):
      """
      *Optional*
      
      Supplies the current time in seconds after the start of the run
      This function is optional. If it is not set the framework assumes
      the model runs with daily timesteps.
      
      Ouput:
      
          - time in seconds since the start of the model run
          
      """
      
      return self.currentTimeStep() * int(configget(self.config,'model','timestepsecs','86400'))
  
  def suspend(self):
    """
      *Required*
      
      Suspends the model to disk. All variables needed to restart the model
      are saved to disk as pcraster maps. Use resume() to re-read them
      
      This function is required. 
      
    """
        
    self.logger.info("Saving initial conditions...")
    #: It is advised to use the wf_suspend() function 
    #: here which will suspend the variables that are given by stateVariables 
    #: function.
    self.wf_suspend(self.SaveDir + "/outstate/")

      
  def initial(self):
      
    """
    *Required*
    
    Initial part of the model, executed only once. It reads all static model
    information (parameters) and sets-up the variables used in modelling.
    
    This function is required. The contents is free. However, in order to
    easily connect to other models it is advised to adhere to the directory
    structure used in the other models.
    
    """
    #: pcrater option to calculate with units or cells. Not really an issue
    #: in this model but always good to keep in mind.
    setglobaloption("unittrue")
    
    #: Note the use of the configget functione below. This way you sepcify a default
    #: for a parameter but it can be overwritten by the uses in the ini file.
    self.timestepsecs = int(configget(self.config,'model','timestepsecs','86400'))
    
    self.basetimestep=86400
    self.SaveMapDir = self.Dir + "/" + self.runId + "/outmaps"
    self.TEMP_mapstack=self.Dir + configget(self.config,"inputmapstacks","Temperature","/inmaps/TEMP") 
    self.Altitude=readmap(self.Dir + "/staticmaps/wflow_dem")
    self.logger.info("Starting Dynamic run...")


  def resume(self):
    """ 
    *Required*

    This function is required. Read initial state maps (they are output of a 
    previous call to suspend()). The implementation showns her is the most basic 
    setup needed.
    
    """
    self.logger.info("Reading initial conditions...")
    #: It is advised to use the wf_resume() function 
    #: here which pick upt the variable save by a call to wf_suspend()
    self.wf_resume(self.Dir + "/instate/")    


    
  def dynamic(self):
      """
      *Required*
      
      This is where all the time dependent functions are executed. Time dependent
      output should also be saved here.
      """
      
      Temperature = self.wf_readmap(self.TEMP_mapstack,0.0)     
      self.TSoil = self.TSoil + 0.1125 * (Temperature - self.TSoil) * self.timestepsecs/self.basetimestep 
      
      self.report(self.TSoil,self.SaveMapDir + "/TS")
    

# The main function is used to run the program from the command line

def main():  
    """
    *Optional*
    
    Perform command line execution of the model. This example uses the getopt
    module to parse the command line options.
    
    The user can set the caseName, the runDir, the timestep and the configfile.
    """      
    global multpars
    caseName = "default"
    runId = "run_default"
    configfile="wflow_seleton.ini"
    _lastTimeStep = 10
    _firstTimeStep = 1
    timestepsecs=86400
    wflow_cloneMap = 'wflow_subcatch.map'

    opts, args = getopt.getopt(sys.argv[1:], 'C:S:T:c:s:R:')
    
    for o, a in opts:
        if o == '-C': caseName = a
        if o == '-R': runId = a
        if o == '-c': configfile = a
        if o == '-s': timestepsecs = int(a)
        if o == '-T': _lastTimeStep=int(a)
        if o == '-S': _firstTimeStep=int(a)
        
    myModel = WflowModel(wflow_cloneMap, caseName,runId,configfile)
    dynModelFw = wf_DynamicFramework(myModel, _lastTimeStep,firstTimestep=_firstTimeStep)
    dynModelFw.createRunId(NoOverWrite=False)    
    dynModelFw._runInitial()
    dynModelFw._runResume()
    dynModelFw._runDynamic(_firstTimeStep,_lastTimeStep)
    dynModelFw._runSuspend()
    

if __name__ == "__main__":
    main()
