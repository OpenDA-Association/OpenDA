/* MOD_V2.0
* Copyright (c) 2012 OpenDA Association
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

package org.openda.utils;

import java.io.File;
import java.util.List;

/**
 * OpenDa Application Configuration
 */
public class OpenDaConfiguration {

    public static final int STOCH_OBSERVER = 0;
    public static final int STOCH_MODEL_FACTORY = 1;
    public static final int ALGORITHM = 2;
    public static final int NUM_COMPONENT_TYPES = 3;

    private final OpenDaComponentConfig[] componentConfigs;
    private final List<OpenDaResultWriterConfig> resultWriterConfigs;
    private final File restartInFile;
    private final File restartOutFilePrefix;
    private final boolean doReadRestart;
    private final boolean doWriteRestart;
    private final String restartOutFileExtension;
	private final String restartOutFileTimes;
	private final String restartOutFileTimeFormat;
	private final boolean restartOutFileTimeTag;
	private final boolean doTiming;
	private final boolean productionRun;
	private final double timePrecision;
	private final boolean vectorPrecisionIsFloat;
	private final boolean vectorIsNative;
	private StochVector.InitialSeedType initialSeedType;
	private final int initialSeedValue;
	private final boolean restartOnlyAtEndOfRun;

	public OpenDaConfiguration(OpenDaComponentConfig stochObserverConfig,
							   OpenDaComponentConfig stochModelFactoryConfig,
							   OpenDaComponentConfig algorithmConfig,
							   List<OpenDaResultWriterConfig> resultWriterConfigs,
							   File restartInFile,
							   File restartOutFilePrefix, String restartOutFileExtension,
							   String restartOutFileTimes, boolean restartOnlyAtEndOfRun, String restartOutFileTimeFormat, boolean restartOutFileTimeTag,
							   boolean doTiming, boolean productionRun,
							   double timePrecision, boolean vectorPrecisionIsFloat, boolean vectorIsNative,
							   StochVector.InitialSeedType initialSeedType, int initialSeedValue) {
		this.restartOnlyAtEndOfRun = restartOnlyAtEndOfRun;
		componentConfigs = new OpenDaComponentConfig[NUM_COMPONENT_TYPES];
		componentConfigs[STOCH_OBSERVER] = stochObserverConfig;
		componentConfigs[STOCH_MODEL_FACTORY] = stochModelFactoryConfig;
		componentConfigs[ALGORITHM] = algorithmConfig;
		this.resultWriterConfigs = resultWriterConfigs;
		this.restartInFile = restartInFile;
		this.restartOutFilePrefix = restartOutFilePrefix;
		this.restartOutFileExtension = restartOutFileExtension;
		this.restartOutFileTimes = restartOutFileTimes;
		this.restartOutFileTimeFormat = restartOutFileTimeFormat;
		this.restartOutFileTimeTag=restartOutFileTimeTag;
		doReadRestart = restartInFile != null;
		doWriteRestart = restartOutFilePrefix != null;
		this.doTiming = doTiming;
		this.productionRun = productionRun;
		this.timePrecision = timePrecision;
		this.vectorPrecisionIsFloat = vectorPrecisionIsFloat;
		this.vectorIsNative = vectorIsNative;
		this.initialSeedType = initialSeedType;
		this.initialSeedValue = initialSeedValue;
	}

	public OpenDaComponentConfig getComponentConfig(int componentIndex) {
        assert componentIndex >= 0 && componentIndex <= ALGORITHM;
        return componentConfigs[componentIndex];
    }

    public List<OpenDaResultWriterConfig> getResultWriterConfigs() {
        return resultWriterConfigs;
    }

    public File getRestartInFile() {
        return restartInFile;
    }

    public File getRestartOutFilePrefix() {
        return restartOutFilePrefix;
    }

    public String getRestartOutFileExtension() {
        return restartOutFileExtension;
    }

	public String getRestartOutFileTimes() {
		return restartOutFileTimes;
	}

	public String getRestartOutFileTimeFormat() {
		return restartOutFileTimeFormat;
	}
	
	public boolean getRestartOutFileTimeTag() {
		return restartOutFileTimeTag;
	}	

	public boolean getDoReadRestart() {
        return doReadRestart;
    }

    public boolean getDoWriteRestart() {
        return doWriteRestart;
    }

	public boolean getDoTiming() {
		return doTiming;
	}

	public boolean getProductionRun() {
		return productionRun;
	}

	public double getTimePrecision(){
		return timePrecision;
	}

	public boolean getVectorPrecisionIsFloat(){
		return vectorPrecisionIsFloat;
	}

	public boolean getVectorIsNative(){
		return vectorIsNative;
	}

	public StochVector.InitialSeedType getInitialSeedType() { return initialSeedType; }

	public int getInitialSeedValue() { return initialSeedValue; }

	public boolean isRestartOnlyAtEndOfRun() {
		return restartOnlyAtEndOfRun;
	}
}
