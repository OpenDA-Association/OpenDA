/* MOD_V2.0
 * Copyright (c) 2015 Netherlands eScience Center
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
package org.openda.model_bmi;

import bmi.EBMI;

import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TSocket;
import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TTransportException;
import org.openda.blackbox.config.BBUtils;
import org.openda.blackbox.interfaces.IModelFactory;
import org.openda.blackbox.interfaces.ITimeHorizonConsumer;
import org.openda.interfaces.IModelInstance;
import org.openda.interfaces.IStochModelFactory.OutputLevel;
import org.openda.interfaces.ITime;
import org.openda.model_bmi.thrift.BMIService;
import org.openda.utils.DistributedCounter;
import org.openda.utils.Results;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.lang.ProcessBuilder.Redirect;
import java.net.ServerSocket;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * ModelFactory that creates instances of the BMIModelInstance class.
 *
 * For more information about BMI see:
 * http://csdms.colorado.edu/wiki/BMI_Description https://github.com/csdms/bmi
 */
@SuppressWarnings("unused")
public class BmiModelFactory implements IModelFactory, ITimeHorizonConsumer {
	private static final Logger LOGGER = LoggerFactory.getLogger(BmiModelFactory.class);
	
	private static final int MAX_CONNECT_ATTEMPTS = 50;
	private static final long CONNECT_TIMEOUT = 100; // ms
	
	// python specific variables.
	private String modelPythonExecutablePath;
	private File modelPythonPath;
	private String modelPythonModuleName;
	private String modelPythonClassName;
	private ArrayList<BmiModelForcingConfig> forcingConfiguration;
	private ArrayList<BmiModelForcingConfig> staticLimitConfiguration;
	private List<BmiModelStateExchangeItemsInfo> modelStateExchangeItemInfos;

	// model variables.
	private File modelTemplateDirectory = null;
	private File instanceDirectoryWithoutPostfix = null;
	//the path and name of the model configuration file (relative to the model template directory).
	private String relativeModelConfigFilePath = null;

	private ITime timeHorizonFromOutside = null;

	//TODO try using RmiClientStochModelFactory to distribute ensemble members over different hosts. AK
	private String[] hosts = null;

	
	/**
	 * Counter to keep track of generated modelInstanceNumbers that have already
	 * been used.
	 */
	private DistributedCounter currentModelInstanceNumber = new DistributedCounter(-1);
	private String inputStateDir = null;
	private String outputStateDir = null;
	private double modelMissingValue = Double.NaN;

	// called by OpenDA
	public BmiModelFactory() {
		// NOTHING
	}

	/**
	 * Initialize the configurable. Specify what its "working directory" is
	 * (usually meaning: the directory where its configuration file is), and
	 * provide its arguments.
	 *
	 * @param configRootDir
	 *            the directory that contains the configuration files for this
	 *            modelFactory (this is not the same as the 'current working
	 *            directory').
	 * @param arguments
	 *            The arguments needed to initialize. Typically the first
	 *            argument can be a configuration file name string, speficied
	 *            relative to the working dir.
	 */
	@Override
	public void initialize(File configRootDir, String[] arguments) {
		if (arguments == null || arguments.length < 1) {
			throw new IllegalArgumentException(getClass().getSimpleName()
					+ ": First argument should be: relative bmiModelFactoryConfig file path.");
		}
		File bmiModelFactoryConfigFile = new File(configRootDir, arguments[0]);
		if (!bmiModelFactoryConfigFile.exists()) {
			throw new IllegalArgumentException(getClass().getSimpleName() + ": Cannot find bmiModelFactoryConfig file "
					+ bmiModelFactoryConfigFile.getAbsolutePath());
		}

		// read model factory config.
		Results.putMessage(getClass().getSimpleName() + ": reading bmiModelFactory config file "
				+ bmiModelFactoryConfigFile.getAbsolutePath());
		BmiModelFactoryConfigReader configReader = new BmiModelFactoryConfigReader(bmiModelFactoryConfigFile);
		this.modelPythonPath = configReader.getPythonModelPythonPath();
		this.modelPythonModuleName = configReader.getPythonModelModuleName();
		this.modelPythonClassName = configReader.getPythonModelClassName();
		this.modelPythonExecutablePath = configReader.getPythonExecutablePath();
		this.modelTemplateDirectory = configReader.getModelTemplateDirectory();
		this.instanceDirectoryWithoutPostfix = new File(this.modelTemplateDirectory.getParentFile(), "work");
		this.relativeModelConfigFilePath = configReader.getRelativeModelConfigFilePath();
		this.forcingConfiguration = configReader.getBmiModelForcingConfigs();
		this.staticLimitConfiguration = configReader.getStaticLimitDataConfigs();
		this.modelStateExchangeItemInfos = configReader.getModelStateExchangeItemInfos();
		this.inputStateDir = configReader.getInputStateDir();
		this.outputStateDir = configReader.getOutputStateDir();
		this.modelMissingValue = configReader.getModelMissingValue();

		this.hosts = configReader.getHosts();

		// remove work directories from previous runs.
		Results.putMessage(getClass().getSimpleName() + ": removing work directories from previous run.");
		BBUtils.removeExistingModelInstanceDirectories(this.instanceDirectoryWithoutPostfix);
	}

	@Override
	public void setTimeHorizon(ITime timeHorizon) {
		this.timeHorizonFromOutside = timeHorizon;
	}

	@Override
	public IModelInstance getInstance(String[] arguments, OutputLevel outputLevel) {
		try {
			// ID of this member
			this.currentModelInstanceNumber.inc();
			int instanceID = this.currentModelInstanceNumber.val();

			// create a output directory for this member
			// String workDir = String.format("work%02d", instanceID);
			File instanceDirectory = new File(this.instanceDirectoryWithoutPostfix.getAbsolutePath() + instanceID);
			BBUtils.makeDirectoryClone(this.modelTemplateDirectory, instanceDirectory);

			//this code assumes that the current working directory is the OpenDA bin folder.
			//This is reasonable, since OpenDA should always be started from the OpenDA bin folder.
			File opendaPythonPath = new File("python");
			if (!opendaPythonPath.exists() || !opendaPythonPath.isDirectory()) {
				throw new RuntimeException(getClass().getSimpleName() + ": Cannot find directory 'python' within current working directory " + new File("").getAbsoluteFile()
						+ ". Please make sure that OpenDA is started from the OpenDA bin directory.");
			}

			EBMI model;
			if (this.hosts == null) {
				model = createModelBridge(null, this.modelPythonExecutablePath, opendaPythonPath, modelPythonPath,
						modelPythonModuleName, modelPythonClassName, instanceDirectory);
			} else {
				// we allocate instances to hosts round-robin
				String host = hosts[instanceID % hosts.length];
				model = createModelBridge(host, this.modelPythonExecutablePath, opendaPythonPath, modelPythonPath,
						modelPythonModuleName, modelPythonClassName, instanceDirectory);
			}

			// modelConfigFile must be a relative path.
			File instanceConfigFile = new File(instanceDirectory, this.relativeModelConfigFilePath);

			return new BmiModelInstance(this.currentModelInstanceNumber.val(), model, instanceDirectory, instanceConfigFile,timeHorizonFromOutside,this.forcingConfiguration, staticLimitConfiguration, modelStateExchangeItemInfos, inputStateDir, outputStateDir, modelMissingValue);
		} catch (Exception e) {
			LOGGER.error("failed to create instance", e);
			throw new RuntimeException(e);
		}
	}
	
	// Obtain a free port by opening a server socket without explicitly giving
	// the port,
	// asking its port, and closing the server socket again.
	public static int getFreePort() throws IOException {
		ServerSocket serverSocket = new ServerSocket(0);
		int result = serverSocket.getLocalPort();

		serverSocket.close();

		return result;
	}
	
	/**
	 * Creates and starts a server process that is capable of running the python
	 * model with the given properties.
	 */
	private static Process startModelProcess(String host, int port, String pythonExecutable, File opendaPythonPath,
			File modelPythonPath, String modelPythonModuleName, String modelPythonClassName, File runDir)
			throws IOException {
		ProcessBuilder builder = new ProcessBuilder();
		builder.directory(runDir);
		//TODO Niels: use Result.putMessage(), otherwise log messages just disappear. AK
		LOGGER.info("Running process with current working directory " + runDir.getAbsolutePath());
		Results.putMessage("Running process with current working directory " + runDir.getAbsolutePath());

		// add openda python code and model python code to python path.
		char pathSeparatorChar = File.pathSeparatorChar;
		String currentPythonPath = builder.environment().get("PYTHONPATH");
		String newPythonPath = opendaPythonPath.getAbsolutePath() + pathSeparatorChar
				+ modelPythonPath.getAbsolutePath() + pathSeparatorChar + currentPythonPath;
		builder.environment().put("PYTHONPATH", newPythonPath);
		LOGGER.info("Running with PYTHONPATH: " + newPythonPath);
		LOGGER.info("Running with PATH: " + builder.environment().get("PATH"));
		
		// this assumes we can login with ssh without a password, or specifying
		// any options
		if (host != null && host != "127.0.0.1" ) {
			builder.command().add("ssh");
			builder.command().add(host);
			builder.command().add("cd");
			builder.command().add(runDir + ";");
			builder.command().add("PYTHONPATH=" + newPythonPath + pathSeparatorChar + "$PYTHONPATH");
		}

		File pythonMainScript = new File(opendaPythonPath, "thrift_bmi_raster_server.py");
		builder.command().add(pythonExecutable);
		builder.command().add("-u");
		
		builder.command().add(pythonMainScript.getAbsolutePath());
		builder.command().add(modelPythonModuleName);
		builder.command().add(modelPythonClassName);
		builder.command().add(host);
		builder.command().add(Integer.toString(port));

		builder.redirectError(Redirect.INHERIT);
		builder.redirectOutput(Redirect.INHERIT);

		LOGGER.info("Running command: " + Arrays.toString(builder.command().toArray()));
		return builder.start();
	}

	/**
	 * Creates and returns a connection to the given server process.
	 */
	private static TTransport connectToCode(String host, int port, Process process) throws IOException {
		Exception exception = null;
		for (int i = 0; i < MAX_CONNECT_ATTEMPTS; i++) {
			// first check if the process is still alive
			try {
				int exitValue = process.exitValue();
				throw new IOException("Process has ended while waiting for Thrift connection with exit code "
						+ exitValue);
			} catch (IllegalThreadStateException e) {
				// We are hoping to end up here, because it means the process is
				// still running.
				// Note: Java 8 allows a smarter way of implementing this.
			}

			// then try connecting to the code
			try {
				TTransport transport = new TSocket(host, port);
				transport.open();
				LOGGER.info("obtained connection on the " + i + "th attempt");
				return transport;
			} catch (TTransportException e) {
				exception = e;
				LOGGER.info("could not connect to code on the " + i + "th attempt, retrying...");
			}

			// finally, wait a certain time before trying again
			try {
				Thread.sleep(CONNECT_TIMEOUT);
			} catch (InterruptedException e) {
				// IGNORE
			}
		}

		assert exception != null;
		throw new IOException("Failed to connect to model, message was: " + exception.getMessage(), exception);
	}

	/**
	 * Creates a Model and a Thrift Bridge to this model.
	 */
	protected static EBMI createModelBridge(String host, String pythonExecutable, File opendaPythonPath, File modelPythonPath,
			String modelPythonModuleName, String modelPythonClassName, File modelRunDir) throws IOException {
		// start local server.
		int port = getFreePort();
		
		if (host == null) {
			//localhost
			host = "127.0.0.1";
		}
		
		Process process = startModelProcess(host, port, pythonExecutable, opendaPythonPath, modelPythonPath,
				modelPythonModuleName, modelPythonClassName, modelRunDir);

		// connect to server.
		TTransport transport = connectToCode(host, port, process);

		// create client.
		TProtocol protocol = new TBinaryProtocol(transport);
		BMIService.Client client = new BMIService.Client(protocol);

		return new ThriftBmiBridge(client, process, transport);
	}

	@Override
	public void finish() {
		// do nothing.
	}

	public static class BmiModelStateExchangeItemsInfo {
		private String stateId;
		private String[] modelStateExchangeItemIds;
		private Double[] modelStateExchangeItemLowerLimits;
		private Double[] modelStateExchangeItemUpperLimits;
		private final String[] lowerLimitExchangeItemIds;
		private final String[] upperLimitExchangeItemIds;

		public BmiModelStateExchangeItemsInfo(String stateId, String[] modelStateExchangeItemIds, Double[] modelStateExchangeItemLowerLimits, Double[] modelStateExchangeItemUpperLimits, String[] lowerLimitExchangeItemIds, String[] upperLimitExchangeItemIds) {
			this.stateId = stateId;
			this.modelStateExchangeItemIds = modelStateExchangeItemIds;
			this.modelStateExchangeItemLowerLimits = modelStateExchangeItemLowerLimits;
			this.modelStateExchangeItemUpperLimits = modelStateExchangeItemUpperLimits;
			this.lowerLimitExchangeItemIds = lowerLimitExchangeItemIds;
			this.upperLimitExchangeItemIds = upperLimitExchangeItemIds;
		}

		public String[] getModelStateExchangeItemIds() {
			return modelStateExchangeItemIds;
		}

		public Double[] getModelStateExchangeItemLowerLimits() {
			return modelStateExchangeItemLowerLimits;
		}

		public Double[] getModelStateExchangeItemUpperLimits() {
			return modelStateExchangeItemUpperLimits;
		}

		public String[] getLowerLimitExchangeItemIds() {
			return lowerLimitExchangeItemIds;
		}

		public String[] getUpperLimitExchangeItemIds() {
			return upperLimitExchangeItemIds;
		}

		public String getStateId() {
			return stateId;
		}
	}
}
