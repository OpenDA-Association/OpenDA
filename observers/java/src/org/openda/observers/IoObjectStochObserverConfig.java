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
package org.openda.observers;

import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * Configuration for IoObject based stochastic observer.
 */
public class IoObjectStochObserverConfig implements Serializable{

    private IoStochObsUncertaintyConfig uncertaintyModuleConfig;
    private ArrayList<IoStochObsIoObjectConfig> ioObjectConfigs;
	private List<String> assimilationObsIds;
	private List<String> validationObsIds;
    private boolean removeMissingValues;

	public IoObjectStochObserverConfig(IoStochObsUncertaintyConfig uncertaintyModuleConfig,
                                       ArrayList<IoStochObsIoObjectConfig> ioObjectConfigs,
                                       List<String> assimilationObsIds,
                                       List<String> validationObsIds, boolean removeMissingValues) {
        this.uncertaintyModuleConfig = uncertaintyModuleConfig;
        this.ioObjectConfigs = ioObjectConfigs;
		this.assimilationObsIds = assimilationObsIds;
		this.validationObsIds = validationObsIds;
        this.removeMissingValues = removeMissingValues;
    }

    public IoStochObsUncertaintyConfig getUncertaintyModuleConfig() {
        return uncertaintyModuleConfig;
    }

    public ArrayList<IoStochObsIoObjectConfig> getIoObjectConfigs() {
        return ioObjectConfigs;
    }

	public List<String> getAssimilationObsIds() {
		return assimilationObsIds;
	}

	public List<String> getValidationObsIds() {
		return validationObsIds;
	}

    public boolean removeMissingValues() {
        return removeMissingValues;
    }

    /**
     * Configuration for an IoObject in the based IoObject based stochastic observer.
     */
    public static class IoStochObsIoObjectConfig implements Serializable{

        private File workingDir;
        private String className;
        private String fileName;
        private String[] arguments;
        private double missingValue;

        public IoStochObsIoObjectConfig(File workingDir, String className, String fileName, String[] arguments, double missingValue) {
            this.workingDir = workingDir;
            this.className = className;
            this.fileName = fileName;
            this.arguments = arguments;
            this.missingValue = missingValue;
        }

        public File getWorkingDir() {
            return workingDir;
        }

        public String getClassName() {
            return className;
        }

        public String getFileName() {
            return fileName;
        }

        public String[] getArguments() {
            return arguments;
        }

        public double getMissingValue() {
            return missingValue;
        }
    }

    /**
     * Configuration for the uncertainty  module in an IoObject based stochastic observer.
     */
    public static class IoStochObsUncertaintyConfig implements Serializable {
        private File workingDir;
        private String className;
        private String[] arguments;

        public IoStochObsUncertaintyConfig(File workingDir, String className, String[] arguments) {
            this.workingDir = workingDir;
            this.className = className;
            this.arguments = arguments;
        }

        public File getWorkingDir() {
            return workingDir;
        }

        public String getClassName() {
            return className;
        }

        public String[] getArguments() {
            return arguments;
        }
    }
}
