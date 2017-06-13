/* OpenDA v2.4 
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

package org.openda.model_wflow;

import java.io.File;
import java.util.Collection;
import java.util.Date;

import org.openda.blackbox.config.BBStochModelVectorConfig;
import org.openda.blackbox.config.BBUtils;
import org.openda.blackbox.interfaces.IModelFactory;
import org.openda.blackbox.interfaces.ITimeHorizonConsumer;
import org.openda.exchange.dataobjects.NetcdfDataObject;
import org.openda.exchange.dataobjects.NetcdfDataObject.GridStartCorner;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IModelInstance;
import org.openda.interfaces.IStochModelFactory.OutputLevel;
import org.openda.interfaces.ITime;
import org.openda.utils.DistributedCounter;
import org.openda.utils.Results;
import org.openda.utils.Time;

/**
 * ModelFactory that creates instances of the WflowModelInstance class.
 *
 * For information about the WFLOW model see www.openstreams.org
 * and https://publicwiki.deltares.nl/display/OpenS/wflow+-+PCRaster-Python+based+distributed+hydrological+models
 * For information about the PCRaster framework see http://pcraster.geo.uu.nl/
 *
 * @author Arno Kockx
 */
public class WflowModelFactory implements IModelFactory, ITimeHorizonConsumer {
	private String pythonModuleNameOfModelToUse = null;
	private File caseDirectory = null;
	private String templateRunId = null;
	private String configFileName = null;
	private String cloneMapFileName = null;
	private IDataObject[] inputDataObjects = null;
	private String relativeModelOutputFilePath = null;
	private String relativeAnalysisOutputFilePath = null;
    private String[] outputExchangeItemIds = null;
    private String relativeScalarModelOutputFilePath = null;
    private String relativeScalarAnalysisFilePath = null;
    private Collection<BBStochModelVectorConfig> scalarOutputVectorCollection;

	private ITime configuredTimeHorizon = null;
	private ITime timeHorizonFromOutside = null;
	private ITime overrulingTimeHorizon = null;

	/**
	 * Counter to keep track of generated modelInstanceNumbers that have already
	 * been used.
	 */
	private DistributedCounter currentModelInstanceNumber = new DistributedCounter(-1);

    /**
	 * Initialize the configurable. Specify what its "working directory" is (usually meaning: the directory
	 * where its configuration file is), and provide its arguments.
	 *
	 * @param configRootDir the directory that contains the configuration files for this modelFactory
	 *				   (this is not the same as the 'current working directory').
	 * @param arguments The arguments needed to initialize. Typically the first argument can be a configuration
	 *				  file name string, speficied relative to the working dir.
	 */
	public void initialize(File configRootDir, String[] arguments) {
		if (arguments == null || arguments.length < 1) {
			throw new IllegalArgumentException(getClass().getSimpleName() + ": First argument should be: relative wflowModelFactoryConfig file path.");
		}
		File wflowModelFactoryConfigFile = new File(configRootDir, arguments[0]);
		if (!wflowModelFactoryConfigFile.exists()) {
			throw new IllegalArgumentException(getClass().getSimpleName() + ": Cannot find wflowModelFactoryConfig file "
					+ wflowModelFactoryConfigFile.getAbsolutePath());
		}

		//read model factory config.
		Results.putMessage(getClass().getSimpleName() + ": reading WflowModelFactory config file "
				+ wflowModelFactoryConfigFile.getAbsolutePath());
		WflowModelFactoryConfigReader modelConfigReader = new WflowModelFactoryConfigReader(wflowModelFactoryConfigFile);
		this.pythonModuleNameOfModelToUse = modelConfigReader.getPythonModuleName();
		this.caseDirectory = modelConfigReader.getCaseDirectory();
		this.templateRunId = modelConfigReader.getTemplateRunId();
		this.configFileName = modelConfigReader.getModelConfigFileName();
		this.cloneMapFileName = modelConfigReader.getCloneMapFileName();
		String[] relativeInputFilePaths = modelConfigReader.getRelativeInputFilePaths();
		this.relativeModelOutputFilePath = modelConfigReader.getRelativeModelOutputFilePath();
		this.relativeAnalysisOutputFilePath = modelConfigReader.getRelativeAnalysisOutputFilePath();
        this.outputExchangeItemIds = modelConfigReader.getOutputExchangeItemIds();
        this.scalarOutputVectorCollection = modelConfigReader.getScalarOutputVectorCollection();
        this.relativeScalarModelOutputFilePath = modelConfigReader.getRelativeScalarModelOutputFilePath();
        this.relativeScalarAnalysisFilePath = modelConfigReader.getRelativeScalarAnalysisOutputFilePath();
		Date startDateTime = modelConfigReader.getStartDateTime();
		Date endDateTime = modelConfigReader.getEndDateTime();
		if (startDateTime != null && endDateTime != null) {
			this.configuredTimeHorizon = new Time(startDateTime, endDateTime);
		}

		//remove work directories from previous runs.
		Results.putMessage(getClass().getSimpleName() + ": removing work directories from previous run.");
		BBUtils.removeExistingModelInstanceDirectories(new File(this.caseDirectory, this.templateRunId));

		//create inputDataObjects.
		createInputDataObjects(relativeInputFilePaths);
	}

	public void setTimeHorizon(ITime timeHorizon) {
		this.timeHorizonFromOutside = timeHorizon;
	}

	private void createInputDataObjects(String[] inputFilePaths) {
		//check if input files exist.
		for (int n = 0; n < inputFilePaths.length; n++) {
			File inputFile = new File(this.caseDirectory, inputFilePaths[n]);
			if (!inputFile.exists()) {
				throw new RuntimeException("Cannot find configured input file " + inputFile.getAbsolutePath());
			}
		}

		//create input dataObjects.
		this.inputDataObjects = new IDataObject[inputFilePaths.length];
		for (int n = 0; n < inputFilePaths.length; n++) {
			NetcdfDataObject inputDataObject = new NetcdfDataObject();
			//this code assumes that grid values in wflow start at upperLeft corner, then contain the first row from left to right,
			//then the second row from left to right, etc. Also see org.openda.model_wflow.WflowModelInstance.createGeometryInfo.
			inputDataObject.setInternalGridStartCorner(GridStartCorner.NORTH_WEST);
			inputDataObject.initialize(this.caseDirectory, new String[]{inputFilePaths[n], "true", "false"});
			this.inputDataObjects[n] = inputDataObject;
		}
	}

	/**
	 * TimeHorizon set from 'outside' overrules timeHorizon configured in modelFactoryConfig.
	 */
	private ITime determineTimeHorizon() {
		if (this.timeHorizonFromOutside != null) {
			Results.putMessage(getClass().getSimpleName() + ": using timeHorizon set from outside: "
					+ this.timeHorizonFromOutside.toString());
			return this.timeHorizonFromOutside;

		} else if (this.configuredTimeHorizon != null) {
			Results.putMessage(getClass().getSimpleName() + ": using timeHorizon configured in wflowModelFactory config file: "
					+ this.configuredTimeHorizon.toString());
			return this.configuredTimeHorizon;

		} else {
			throw new RuntimeException(getClass().getSimpleName()
					+ ": If timeHorizon not set from outside, then timeHorizon should be configured in wflowModelFactory config file.");
		}
	}

	/**
	 * Create an instance of the Model
	 *
	 * @param arguments Arguments for this instance. (arguments == null) or (arguments.length == 0) means: no arguments.
	 * @param outputLevel The level of output to be produced by the new instance (default, suppressed, etc.)
	 * @return The Model instance
	 */
	public IModelInstance getInstance(String[] arguments, OutputLevel outputLevel) {
		if (this.overrulingTimeHorizon == null) {
			this.overrulingTimeHorizon = determineTimeHorizon(); 
		}

		this.currentModelInstanceNumber.inc();
		String instanceRunId = this.templateRunId + this.currentModelInstanceNumber.val();
		return new WflowModelInstance(this.overrulingTimeHorizon, this.pythonModuleNameOfModelToUse,
				this.caseDirectory, instanceRunId, this.configFileName, this.cloneMapFileName,
				this.inputDataObjects, this.relativeModelOutputFilePath, this.relativeAnalysisOutputFilePath,
                this.outputExchangeItemIds, this.relativeScalarModelOutputFilePath, this.relativeScalarAnalysisFilePath,
                this.scalarOutputVectorCollection);
	}

	public void finish() {
		// no action needed (yet)
	}
}
