/* OpenDA v2.4.1 
* Copyright (c) 2017 OpenDA Association 
* All rights reserved.
* 
* This file is part of OpenDA. 
* 
* OpenDA is free software: you can redistribute it and/or modify 
* it under the terms of the GNU Lesser General Public License as 
* published by the Free Software Foundation, either version 3 of 
* the License, or (at your option) any later version. 
* 
* OpenDA is distributed in the hope that it will be useful, 
* but WITHOUT ANY WARRANTY; without even the implied warranty of 
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
* GNU Lesser General Public License for more details. 
* 
* You should have received a copy of the GNU Lesser General Public License
* along with OpenDA.  If not, see <http://www.gnu.org/licenses/>.
*/
package org.openda.application;
import org.openda.application.gui.ApplicationScreen;
import org.openda.application.gui.ControlGui;
import org.openda.blackbox.interfaces.ITimeHorizonConsumer;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.*;
import org.openda.utils.*;
import org.openda.utils.io.OpenDaConfigurationReader;
import org.openda.utils.performance.OdaGlobSettings;
import org.openda.utils.performance.OdaTiming;

import java.io.File;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.List;

/**
 * Runs an application in a separate thread. This allows safe use from within a gui.
 * After initialization use:
 * start() to start or continue the thread
 * pause() to pause the thread
 * getStatus() to get the present status
 * getMessage() to get the status message
 * @author verlaanm
 * TODO getProgress() show which percentage of the work is done
 */
public class ApplicationRunner implements Runnable{

    private static boolean runningInTest = true;

	private int currentAlgorithmStep = 0;

	protected File workingDir= new File(".");
    protected File restartInFile = null;
    protected File restartOutFilePrefix = null;
    protected String restartOutFileExtension = null;
    protected boolean addRestartTimeTag=true;
    protected boolean doWriteRestart = false;
    protected boolean doReadRestart = false;
    protected Time[] restartTimes=new Time[0];

	// possible status values
    public static enum Status {
        INITIALIZING,
        INITIALIZED,
        RUNNING,
        PAUSED,
        STOPPED,
        FINISHED,
        ERROR
    }

    // possible requests to thread
    public static enum Request {
        CONTINUE,
        PAUSE,
        STOP
    }

    // status value and request
    private Status status;
    private Request request;

	// thread
	private Thread runner;

	// openda
	protected IAlgorithm algorithm;
    protected IStochObserver stochObserver;
    protected IStochModelFactory stochModelFactory;

    public static void setRunningInTest(boolean runningInTest) {
        ApplicationRunner.runningInTest = runningInTest;
        ApplicationRunner.runningInTest = true;
	}

	public static boolean getRunningInTest() {
        return runningInTest;
    }

    /**
	 * Create a new thread with this inputfile.
	 */
	public ApplicationRunner(){
        if(this.runner==null){
			synchronized (this) {
				this.status = Status.INITIALIZING;
			}
			Results.putProgression("Application initializing");

        }else{ // somehow there was already a thread
			this.runner  = null;
			this.status  = Status.ERROR;
		}
	}


    /**
	 * Create a new thread with this inputfile.
	 * @param workingDir directory where the input file resides
	 * @param fileName input file for the application
	 * @param getStarted should this thread start working immediately
	 */
	public ApplicationRunner(File workingDir, String fileName, boolean getStarted){
        if(this.runner==null){
			synchronized (this) {
				this.status = Status.INITIALIZING;
			}
			Results.putProgression("Application initializing");
            initialization(workingDir, fileName, getStarted);

        }else{ // somehow there was already a thread
			this.runner  = null;
			this.status  = Status.ERROR;
		}
	}

    public void initialization(File workingDir, String fileName, boolean getStarted) {
        // run application here
    	this.workingDir=workingDir;
        this.algorithm = startApplication(workingDir, fileName);

        synchronized (this) {
            this.status = Status.INITIALIZED;
        }
        Results.putProgression("Application initializing finished");

        this.runner = new Thread(this);

        // possibly get started
        this.pause();
        if(getStarted){
            this.resume();
        }
        this.runner.start(); //always start, run() gets stopped later
    }

    public void pause(){
		this.request = ApplicationRunner.Request.PAUSE;
	}

    public void resume(){
        this.request = ApplicationRunner.Request.CONTINUE;
    }

    public void stop() {
        this.request = ApplicationRunner.Request.STOP;
    }

	public Status getStatus(){
		return this.status;
	}

	private void waitIfRequested(){

        // check if we should stop
        if(this.status==Status.INITIALIZED || this.status==Status.RUNNING || this.status==Status.PAUSED){
			if(this.request==ApplicationRunner.Request.STOP){
                changeStatus(Status.STOPPED);
            }
        }

		// start from initialized if requested
        // System.out.println("WAITIFREQUESTED, status: " + status.toString() + ", request: " + request.toString());
        if(this.status==Status.INITIALIZED){
			if(this.request==ApplicationRunner.Request.CONTINUE){
                changeStatus(Status.RUNNING);
            } else if (this.request==ApplicationRunner.Request.PAUSE) {
                changeStatus(Status.PAUSED);
            }
        }

		// pauze if requested
		if(this.status==Status.RUNNING){
			if(this.request==ApplicationRunner.Request.PAUSE){
                changeStatus(Status.PAUSED);
            }
        }

		// block while pauzed
		while(this.status==Status.PAUSED){
			// continue if requested
			if(this.request==ApplicationRunner.Request.CONTINUE){
                changeStatus(Status.RUNNING);
			}
            else if(this.request==ApplicationRunner.Request.STOP){
                changeStatus(Status.STOPPED);
            }
			// wait a while
			try{
				Thread.sleep(200);
			}catch(InterruptedException e){
				// do nothing
			}
		} //while pauzed
	}

    private void changeStatus(Status status) {
        synchronized (this) {
            this.status = status;
        }
        ApplicationScreen.statusChangedHandler(status);
        ControlGui.statusChangedHandler(status);
    }

    /**
	 * Do the actual work
	 */
	public void run(){
		OdaTiming timerRun = new OdaTiming("Algorithm bla");
		timerRun.start();

		// Start the algorithm
		Results.putProgression("Initializing application");
        synchronized (this) {
            changeStatus(Status.INITIALIZING);
        }
        try {
            if (doReadRestart) {
                IModelState savedInternalState  = this.algorithm.loadPersistentState(restartInFile);
                this.algorithm.restoreInternalState(savedInternalState);
            } else {
            this.algorithm.prepare();
            if (doWriteRestart) {
                this.writeRestart();
            }

            }
        } catch (Exception e) {
			finishApplication();
            Results.putProgression("Error preparing algorithm.");
            Results.putProgression("Error message: "+e.getMessage());
            synchronized (this) {
                changeStatus(Status.ERROR);
            }
            Results.reset();
            if (runningInTest) {
                throw new RuntimeException(e);
            }
            return;
        }
        Results.putProgression("Application initialized");
        synchronized (this) {
            changeStatus(Status.INITIALIZED);
        }

        // Run the algorithm
        waitIfRequested();
        while(!(this.status == Status.STOPPED) && this.algorithm.hasNext()){
			Results.putProgression("Application starting next step");
			try {
				//
				// next step
				//
				this.algorithm.next();
				//
				// restarts
				if (doWriteRestart) {
					this.writeRestart();
				}
            } catch (Exception e) {
				logAlgorithmStepErrorAndFinish(e);
                synchronized (this) {
                    changeStatus(Status.ERROR);
                }
                Results.reset();
                if (runningInTest) {
                    throw new RuntimeException(e);
                }
                return;
            }
            waitIfRequested();
        }
        timerRun.stop();
		timerRun.printAll(this.workingDir);

		synchronized (this) {
            changeStatus(Status.FINISHED);
		}
		finishApplication();
		Results.putProgression((this.status == Status.STOPPED) ? "Application Stopped" : "Application Done");
		Results.reset();
	}

	protected void logAlgorithmStepErrorAndFinish(Exception e) {
		Results.putProgression("Error running algorithm step.");
		Results.putProgression("Error message: "+e.getMessage());
		Results.putProgression("Error type :"+e.getClass().getSimpleName());
		StringWriter stringWriter = new StringWriter();
		PrintWriter stackTrace = new PrintWriter(stringWriter);
		e.printStackTrace(stackTrace);
		Results.putProgression("Stacktrace :"+stringWriter.toString());
		finishApplication();
	}

	protected void finishApplication() {
		try {
			if (algorithm != null) {
				algorithm.finish();
			}
			if (stochModelFactory != null) {
				stochModelFactory.finish();
			}
			if (stochObserver != null) {
				stochObserver.free();
			}
		} catch (Exception e) {
			//catch and log any exceptions during finish application, otherwise these might "overshadow" any exceptions thrown during the run.
			Results.putMessage("Exception during call to finish application, message was: " + e.getMessage());
		}
	}

	/* ===========================================================================================
	 * 
	 *  routines to set up the algorithm for an OpenDaApplication
	 * 
	 */
	protected IAlgorithm startApplication(File workingDir, String fileName) {
		algorithm = startApplication(workingDir, fileName, null);
		return algorithm;
	}

	protected IAlgorithm startApplication(File workingDir, String fileName, ITime timeHorizon) {

		IAlgorithm algorithm;
		try {
            Results.addResultWriter(new InstanceStore(new File(workingDir, fileName), true));
            algorithm = createApplicationComponents(new File(workingDir,fileName), timeHorizon);
			algorithm.setStochComponents(stochObserver, stochModelFactory);
			stochObserver.setParent(algorithm);
		} catch (Exception e) {
			finishApplication();
            if (runningInTest) {
                throw new RuntimeException(e);  
            }
			System.err.println(e.getMessage());
			e.printStackTrace();
			Results.putProgression("Error initializing component "+fileName+".");
			Results.putProgression("Error message: "+e.getMessage());
			throw new RuntimeException(e.getMessage());
		}
		return algorithm;
	}


	public IAlgorithm createApplicationComponents(File applicationConfigFile, ITime timeHorizon) {

        OpenDaConfiguration configuration = readConfigAndCreateResultWriters(applicationConfigFile);
		OdaTiming timerCreateObserver     = new OdaTiming("CreateStochObserver");
		OdaTiming timerCreateModelFactory = new OdaTiming("CreateModelFactory");
		OdaTiming timerCreateAlgorithm    = new OdaTiming("CreateAlgorithm");

		timerCreateObserver.start();
        createStochObserver(configuration);
		timerCreateObserver.stop();
		timerCreateModelFactory.start();
        createStochModelFactory(configuration);
		if (timeHorizon != null && (stochModelFactory instanceof ITimeHorizonConsumer)) {
			((ITimeHorizonConsumer)stochModelFactory).setTimeHorizon(timeHorizon);
		}
		timerCreateModelFactory.stop();

        // Create the algorithm
		timerCreateAlgorithm.start();
		IAlgorithm algorithm = ObjectSupport.createAlgorithm(configuration.getComponentConfig(OpenDaConfiguration.ALGORITHM));
		timerCreateAlgorithm.stop();

        return algorithm;
    }

    private OpenDaConfiguration readConfigAndCreateResultWriters(File applicationConfigFile) {
        // Read configuration

        OpenDaConfigurationReader openDaConfigurationReader = new OpenDaConfigurationReader(applicationConfigFile);
        OpenDaConfiguration configuration = openDaConfigurationReader.getOpenDaConfiguration();

		// apply global settings immediatly
		OdaTiming.setDoTiming(configuration.getDoTiming());
		OdaGlobSettings.setProductionRun(configuration.getProductionRun());
		OdaGlobSettings.setTimePrecision(configuration.getTimePrecision());
		OdaGlobSettings.setVectorPrecisionFloat(configuration.getVectorPrecisionIsFloat());
        OdaGlobSettings.setVectorIsNative(configuration.getVectorIsNative());
		if (configuration.getInitialSeedType() == StochVector.InitialSeedType.specify) {
			StochVector.setInitialSeedType(configuration.getInitialSeedType(), configuration.getInitialSeedValue());
		} else {
			StochVector.setInitialSeedType(configuration.getInitialSeedType());
		}

        doReadRestart = configuration.getDoReadRestart();
        restartInFile = configuration.getRestartInFile();
        restartOutFilePrefix = configuration.getRestartOutFilePrefix();
        restartOutFileExtension = configuration.getRestartOutFileExtension();
        addRestartTimeTag = configuration.getRestartOutFileTimeTag();
        doWriteRestart = configuration.getDoWriteRestart();
		String timeFormat = configuration.getRestartOutFileTimeFormat();
		String outFileTimes = configuration.getRestartOutFileTimes();
		if((outFileTimes!=null)&&(outFileTimes.length()>0)){
			//parse sequence
			Results.putMessage("outFileTimes="+outFileTimes);
			if(timeFormat.equals("dateTimeString")){
				this.restartTimes = Time.Mjds2Times(TimeUtils.dateTimeSequenceString2Mjd(outFileTimes));
			}else{ //use Mjd
				this.restartTimes = Time.Mjds2Times(TimeUtils.MjdSequenceString2Mjd(outFileTimes));
			}
		}

		// Get class types
        List<OpenDaResultWriterConfig> resultWriterConfigs = configuration.getResultWriterConfigs();

        final Class[] resultWriterImplementations = new Class[resultWriterConfigs.size()];

        for (int i = 0; i < resultWriterImplementations.length ; i++) {
			OpenDaResultWriterConfig resultWriterConfig = resultWriterConfigs.get(i);
            if ( resultWriterConfig.getClassName() != null ) {
                try {
                    resultWriterImplementations[i] = getClass(IResultWriter.class, resultWriterConfig.getClassName());
                } catch (ClassNotFoundException e) {
                    throw new RuntimeException(
                            "DaApplication ERROR: Class specified in config. file could not be found: " + e.getMessage());
                }
                addResultWriter(resultWriterConfig, resultWriterImplementations[i]);
            } else {
                resultWriterImplementations[i] = null;
            }
        }

        if (!Results.hasWriters()) {
            // default result writer
            String resultsFilePath = applicationConfigFile.getAbsolutePath();
            if (resultsFilePath.endsWith(".xml") || resultsFilePath.endsWith(".XML") || resultsFilePath.endsWith(".Xml")) {
                resultsFilePath = resultsFilePath.substring(0, resultsFilePath.length() - 4) + "-results.txt";
            }
            File resultsFile = new File(resultsFilePath);
            IResultWriter asciiResultWriter = new AsciiResultWriter(resultsFile.getParentFile(), resultsFile.getName());
            Results.addResultWriter(asciiResultWriter);
        }

		String versionString = "OpenDA version " + VersionUtils.getVersionFromManifest(OpenDaApplication.class);
		Results.putProgression(versionString);

        return configuration;
    }

    protected void createStochObserver(OpenDaConfiguration configuration) {
        // Create the stoch observer
        IConfigurable observerInstance = ObjectSupport.createConfigurable("Stoch Observer: ",
                configuration.getComponentConfig(OpenDaConfiguration.STOCH_OBSERVER));
        if (!(observerInstance instanceof IStochObserver)) {
            throw new RuntimeException(observerInstance.getClass().getName() +
                    " is not implementing the IStochObserver interface");
         }
        stochObserver = (IStochObserver) observerInstance;
    }

    protected void createStochModelFactory(OpenDaConfiguration configuration) {
        // Create the stoch model factory
        IConfigurable factoryInstance = ObjectSupport.createConfigurable("Stoch Model Factory: ",
                configuration.getComponentConfig(OpenDaConfiguration.STOCH_MODEL_FACTORY));
        if (!(factoryInstance instanceof IStochModelFactory)) {
            throw new RuntimeException(factoryInstance.getClass().getName() +
                    " is not implementing the StochModelFactory interface");
        }
        stochModelFactory = (IStochModelFactory) factoryInstance;
    }

    private static void addResultWriter(OpenDaResultWriterConfig resultWriterConfig, Class resultWriterImplementation) {
		String resultWriterString = "ResultWriter: " +
		"\n\tclassName: " + resultWriterConfig.getClassName() +
		"\n\tdir.: " + resultWriterConfig.getWorkingDir() +
		"\n\tconfig.: " + resultWriterConfig.getArguments()[0];
		IResultWriter resultWriter;
		if ( resultWriterImplementation != null ) {
			final Constructor resultWriterConstructor = getWorkingDirAndConfigStringConstructor(resultWriterImplementation);
			try {
				resultWriter = (IResultWriter) resultWriterConstructor.newInstance(resultWriterConfig.getWorkingDir(),
						resultWriterConfig.getArguments()[0]);
			} catch (Exception e) {
				throw new RuntimeException(composeErrorMessage(resultWriterString, e));
			}
			Results.addResultWriter(resultWriter, resultWriterConfig.getResultSelectionConfig());
		}
	}

    private static Class getClass(Class implementedInterface, String className) throws ClassNotFoundException {
		Class implementingClass = Class.forName(className);
		if (!implementedInterface.isAssignableFrom(implementingClass)) {
			throw new RuntimeException(implementingClass + " does not implement the " + implementedInterface + " interface");
		}
		return implementingClass;
	}

	private static Constructor getWorkingDirAndConfigStringConstructor(Class implementingClass) {
		Constructor constructor;
		try {
			final Class workingDirClass = File.class;
			constructor = implementingClass.getConstructor(workingDirClass, String.class);
			if (!Modifier.isPublic(constructor.getModifiers())) {
				throw new RuntimeException("Constructor \"" + constructor.toString() + "\" must be public");
			}
		} catch (NoSuchMethodException e) {
			throw new RuntimeException("Constructor \"" + implementingClass + "(File workingDir, String configFile)\" not found");
		}
		return constructor;
	}

	private static String composeErrorMessage(String componentString, Exception e) {
		String errorMessage = "Could not create " + componentString;
		if (e instanceof InvocationTargetException) {
			Throwable t = ((InvocationTargetException)e).getTargetException();
			if ( t != null )
				errorMessage += "\n\terror.:" + t.getMessage();
		} else {
			if ( e.getCause() != null ) {
				errorMessage += "\n\tERROR: " + e.getCause().getMessage();
			}
		}
		return errorMessage;
	}

	public IAlgorithm getAlgorithm(){
		return this.algorithm;
	}

    public IStochModelFactory getStochModelFactory() {
        return stochModelFactory;
    }

    public IStochObserver getStochObserver() {
        return stochObserver;
    }

    public static IStochModelFactory createStochModelFactoryComponent(File applicationConfigFile) {

        // Read configutation
        OpenDaConfigurationReader openDaConfigurationReader = new OpenDaConfigurationReader(applicationConfigFile);
        OpenDaConfiguration configuration = openDaConfigurationReader.getOpenDaConfiguration();

        // Create the stoch model factory
        IConfigurable factoryInstance = ObjectSupport.createConfigurable("Stoch Model Factory: ",
                configuration.getComponentConfig(OpenDaConfiguration.STOCH_MODEL_FACTORY));
        if (!(factoryInstance instanceof IStochModelFactory)) {
            throw new RuntimeException(factoryInstance.getClass().getName() +
                    " is not implementing the StochModelFactory interface");
        }

        return (IStochModelFactory) factoryInstance;
    }
    
    protected void writeRestart(){
		ITime time = algorithm.getCurrentTime();
		boolean writeAtThisTime;
		if (time == null) {
			writeAtThisTime = true;
		}
		else {
			writeAtThisTime = (time.getMJD() > algorithm.getTimeHorizon().getBeginTime().getMJD());
			if ((this.restartTimes.length>0) & (time!=null)){
				writeAtThisTime=false;
				for(int i=0;i<this.restartTimes.length;i++){
					double currentTime = (time.getBeginTime().getMJD() + time.getEndTime().getMJD())/2d;
					if(Math.abs(currentTime-this.restartTimes[i].getMJD())<OdaGlobSettings.getTimePrecision()){
						writeAtThisTime=true;
					}
				}
			}
		}
		if (writeAtThisTime){
			IModelState savedInternalState = this.algorithm.saveInternalState();
			File stateFile;
			if(time==null){
				String timeString="";
				if(addRestartTimeTag){
					timeString= String.valueOf(++currentAlgorithmStep);
				}
				stateFile = new File(restartOutFilePrefix.getAbsolutePath() +
						timeString+ restartOutFileExtension);
			}else{
				this.restartOutFileExtension="zip"; //zip extension for kalmanfilters
				double currentTime = (time.getBeginTime().getMJD() + time.getEndTime().getMJD())/2d;
				String timeString="";
				if(addRestartTimeTag){
					timeString= TimeUtils.mjdToString(currentTime);
				}
				stateFile = new File(restartOutFilePrefix.getAbsolutePath() +
						timeString+ restartOutFileExtension);
			}
			savedInternalState.savePersistentState(stateFile);
			this.algorithm.releaseInternalState(savedInternalState);
		}

    }
}
