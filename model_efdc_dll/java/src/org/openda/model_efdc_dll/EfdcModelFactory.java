/* OpenDA v2.3.1 
* Copyright (c) 2016 OpenDA Association 
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
package org.openda.model_efdc_dll;

import java.io.File;
import java.util.TimeZone;

import org.openda.blackbox.config.BBUtils;
import org.openda.blackbox.interfaces.IModelFactory;
import org.openda.blackbox.interfaces.ITimeHorizonConsumer;
import org.openda.interfaces.IModelInstance;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IStochModelFactory.OutputLevel;
import org.openda.interfaces.ITime;
import org.openda.model_efdc.EfdcEventTox2InpIoObject;
import org.openda.model_efdc.EfdcInpIoObject;
import org.openda.utils.DistributedCounter;
import org.openda.utils.Results;

/**
 * ModelFactory that creates instances of the EfdcModelInstance class that use
 * the dll version of the EFDC model.
 *
 * For information about the EFDC model see http://www.epa.gov/athens/wwqtsc/html/efdc.html
 *
 * @author Arno Kockx
 */
public class EfdcModelFactory implements IModelFactory, ITimeHorizonConsumer {
	private static final String EFDC_INP_FILE_NAME = "EFDC.INP";
	private static final String EFDC_INP_TEMPLATE_FILE_NAME = "EFDC_TEMPLATE.INP";
	private static final String EVENT_TOX2_INP_FILE_NAME = "EVENT_TOX2.INP";
	private static final String EVENT_TOX2_INP_TEMPLATE_FILE_NAME = "EVENT_TOX2_TEMPLATE.INP";
	private static final String TSTART = "TSTART";
	private static final String TSTOP = "TSTOP";

	private File efdcDllFile = null;
	private File templateDirectory = null;
	private File instanceDirectoryWithoutPostfix = null;
	private String[] relativeInputFilePaths = null;
	private String relativeModelOutputFilePath = null;
	private String relativeAnalysisOutputFilePath = null;
	/**
	 * The timeZone that is used by the model.
	 * This is required to convert the times of the data values
	 * to/from the timeZone that is used by the model.
	 * Default is GMT.
	 */
	private TimeZone modelTimeZone = TimeZone.getTimeZone("GMT");
	private ITime timeHorizonFromOutside = null;
	private boolean templateFileDone = false;
	private boolean efdcDllInitialized = false;


	/**
	 * Counter to keep track of generated modelInstanceNumbers that have already been used.
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
			throw new IllegalArgumentException("First argument should be: relative efdcModelFactoryConfig file path.");
		}
		File efdcModelFactoryConfigFile = new File(configRootDir, arguments[0]);
		if (!efdcModelFactoryConfigFile.exists()) {
			throw new IllegalArgumentException(getClass().getSimpleName() + ": Cannot find efdcModelFactoryConfig file "
					+ efdcModelFactoryConfigFile.getAbsolutePath());
		}

		//read model factory config.
		Results.putMessage(this.getClass().getSimpleName() + ": reading EfdcModelFactory config file "
				+ efdcModelFactoryConfigFile.getAbsolutePath());
		EfdcModelFactoryConfigReader modelConfigReader = new EfdcModelFactoryConfigReader(efdcModelFactoryConfigFile);
		this.efdcDllFile = modelConfigReader.getEfdcDllFile();
		this.templateDirectory = modelConfigReader.getTemplateDirectory();
		this.instanceDirectoryWithoutPostfix = modelConfigReader.getInstanceDirectoryWithoutPostfix();
		this.relativeInputFilePaths = modelConfigReader.getRelativeInputFilePaths();
		this.relativeModelOutputFilePath = modelConfigReader.getRelativeModelOutputFilePath();
		this.relativeAnalysisOutputFilePath = modelConfigReader.getRelativeAnalysisOutputFilePath();
		this.modelTimeZone = modelConfigReader.getModelTimeZone();

		//remove work directories from previous runs.
		Results.putMessage(this.getClass().getSimpleName() + ": removing work directories from previous run.");
		BBUtils.removeExistingModelInstanceDirectories(this.instanceDirectoryWithoutPostfix);
	}

	public void setTimeHorizon(ITime timeHorizon) {
		this.timeHorizonFromOutside = timeHorizon;
	}

	/**
	 * @param startTime in MJD
	 * @param endTime in MJD
	 * @param timeZoneOffsetInHours relative to GMT.
	 */
	private static void writeRunPeriodInModelConfigFiles(File templateDir,
			double startTime, double endTime, double timeZoneOffsetInHours) {

		//use EfdcInpIoObject to write timeHorizon in EFDC.INP file.
		BBUtils.makeFileClone(new File(templateDir, EFDC_INP_TEMPLATE_FILE_NAME),
				new File(templateDir, EFDC_INP_FILE_NAME));
		EfdcInpIoObject efdcInpIoObject = new EfdcInpIoObject();
		efdcInpIoObject.initialize(templateDir, EFDC_INP_FILE_NAME,
				new String[]{String.valueOf(timeZoneOffsetInHours), TSTART, TSTOP});
		for (IPrevExchangeItem exchangeItem : efdcInpIoObject.getExchangeItems()) {
			if (TSTART.equals(exchangeItem.getId())) {
				exchangeItem.setValuesAsDoubles(new double[]{startTime});
			} else if (TSTOP.equals(exchangeItem.getId())) {
				exchangeItem.setValuesAsDoubles(new double[]{endTime});
			}
		}
		efdcInpIoObject.finish();

		//use EfdcEventTox2InpIoObject to write timeHorizon in EVENT_TOX2.INP file.
		BBUtils.makeFileClone(new File(templateDir, EVENT_TOX2_INP_TEMPLATE_FILE_NAME),
				new File(templateDir, EVENT_TOX2_INP_FILE_NAME));
		EfdcEventTox2InpIoObject efdcEventTox2InpIoObject = new EfdcEventTox2InpIoObject();
		efdcEventTox2InpIoObject.initialize(templateDir, EVENT_TOX2_INP_FILE_NAME,
				new String[]{String.valueOf(timeZoneOffsetInHours), TSTART, TSTOP});
		for (IPrevExchangeItem exchangeItem : efdcEventTox2InpIoObject.getExchangeItems()) {
			if (TSTART.equals(exchangeItem.getId())) {
				exchangeItem.setValuesAsDoubles(new double[]{startTime});
			} else if (TSTOP.equals(exchangeItem.getId())) {
				exchangeItem.setValuesAsDoubles(new double[]{endTime});
			}
		}
		efdcEventTox2InpIoObject.finish();
	}

	/**
	 * Create an instance of the Model
	 *
	 * @param arguments Arguments for this instance. (arguments == null) or (arguments.length == 0) means: no arguments.
	 * @param outputLevel The level of output to be produced by the new instance (default, suppressed, etc.)
	 * @return The Model instance
	 */
	public IModelInstance getInstance(String[] arguments, OutputLevel outputLevel) {

		if (!templateFileDone) {
			//timeHorizon set from 'outside' overrules timeHorizon in efdc model config files.
			if (this.timeHorizonFromOutside != null) {
				Results.putMessage(getClass().getSimpleName() + ": using timeHorizon set from outside: "
						+ this.timeHorizonFromOutside.toString());

				//set timeHorizon in efdc model config files.
				double modelTimeZoneOffsetInHours = (double) this.modelTimeZone.getRawOffset() / (1000.0 * 3600.0);
				writeRunPeriodInModelConfigFiles(this.templateDirectory, this.timeHorizonFromOutside.getBeginTime().getMJD(),
						this.timeHorizonFromOutside.getEndTime().getMJD(), modelTimeZoneOffsetInHours);
			} else {
				//if timeHorizon has not been set from outside, then assume that timeHorizon has been set in the efdc model config files manually.
				Results.putMessage(getClass().getSimpleName() + ": using timeHorizon from efdc model config files.");
			}
			templateFileDone = true;
		}

		//create new instance folder.
		this.currentModelInstanceNumber.inc();
		int modelInstanceNumber = this.currentModelInstanceNumber.val();
		File instanceDirectory = new File(this.instanceDirectoryWithoutPostfix.getAbsolutePath() + modelInstanceNumber);
		BBUtils.makeDirectoryClone(this.templateDirectory, instanceDirectory);

		if (!efdcDllInitialized) {
			//init efdc dll.
			EfdcDLL.initialize(this.efdcDllFile, this.instanceDirectoryWithoutPostfix.getParentFile(), instanceDirectory, this.modelTimeZone);
			efdcDllInitialized = true;
		}

		//create new instance.
		return new EfdcModelInstance(instanceDirectory, relativeInputFilePaths, relativeModelOutputFilePath, relativeAnalysisOutputFilePath, modelInstanceNumber, this);
	}

	public void finish() {
		// TODO: shut down
	}

	public int getCurrentModelInstanceNumber() {
		return this.currentModelInstanceNumber.val();
	}
}
