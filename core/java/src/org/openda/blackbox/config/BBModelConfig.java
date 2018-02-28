/* OpenDA v2.4.3 
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

package org.openda.blackbox.config;

import org.openda.interfaces.ITime;

import java.io.File;
import java.util.Collection;

/**
 * TODO: description
 */
public class BBModelConfig {

    private BBWrapperConfig wrapperConfig;
    private Collection<BBModelVectorConfig> vectorConfigs;
	private boolean skipModelActionsIfInstanceDirExists;
	private boolean doCleanUp;
    private File configRootDir;
    private String instanceNumberFormat;
    private ITime startTime;
    private ITime endTime;
    private String[] startTimeExchangeItemIds;
    private String[] endTimeExchangeItemIds;
	private String[] restartFileNames;
	private String savedStatesDirPrefix;
    private double timeStepMJD;
    private String[] timeStepExchangeItemIds;

    public BBModelConfig(File configRootDir, BBWrapperConfig wrapperConfig,
                         String instanceNumberFormat, ITime startTime, ITime endTime,
                         double timeStepMJD, String[] startTimeExchangeItemIds, String[] endTimeExchangeItemIds,
                         String[] timeStepExchangeItemIds, Collection<BBModelVectorConfig> vectorConfigs,
                         boolean skipModelActionsIfInstanceDirExists,
                         boolean doCleanUp,
                         String[] restartFileNames, String savedStatesDirPrefix) {
        this.configRootDir = configRootDir;
        this.wrapperConfig = wrapperConfig;
        this.instanceNumberFormat = instanceNumberFormat;
        this.startTime = startTime;
        this.endTime = endTime;
        this.timeStepMJD = timeStepMJD;
        this.startTimeExchangeItemIds = startTimeExchangeItemIds;
        this.endTimeExchangeItemIds = endTimeExchangeItemIds;
        this.timeStepExchangeItemIds = timeStepExchangeItemIds;
        this.vectorConfigs = vectorConfigs;
		this.skipModelActionsIfInstanceDirExists = skipModelActionsIfInstanceDirExists;
		this.doCleanUp = doCleanUp;
		this.restartFileNames = restartFileNames;
		this.savedStatesDirPrefix = savedStatesDirPrefix;
	}

    public BBWrapperConfig getWrapperConfig() {
        return wrapperConfig;
    }

    public Collection<BBModelVectorConfig> getVectorConfigs() {
        return vectorConfigs;
    }

    public boolean doCleanUp() {
        return doCleanUp;
    }

	public boolean skipModelActionsIfInstanceDirExists() {
		return skipModelActionsIfInstanceDirExists;
	}

	public File getConfigRootDir() {
        return configRootDir;
    }

	public String getInstanceNumberFormat() {
		return instanceNumberFormat;
	}

	public ITime getStartTime() {
        return startTime;
    }

    public ITime getEndTime() {
        return endTime;
    }

    public double getTimeStepMJD(){
        return timeStepMJD;
    }

    public String[] getStartTimeExchangeItemIds() {
        return startTimeExchangeItemIds;
    }

    public String[] getEndTimeExchangeItemIds() {
        return endTimeExchangeItemIds;
    }

	public String[] getRestartFileNames() {
		return restartFileNames;
	}

	public String getSavedStatesDirPrefix() {
		return savedStatesDirPrefix;
	}

    public String[] getTimeStepExchangeItemIds() {
        return timeStepExchangeItemIds;
    }
}
