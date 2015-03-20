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

package org.openda.model_swan;

import java.io.File;

/**
 * Configuration information for the model factory for a SwanCalibStochModel.
 *
 * Code copied and adapted from DStochModelConfig.
 */
public class SwanCalibStochModelConfig {

    private File configRootDir;
    private SwanCalibComponentConfig uncertaintyConfig;
    private SwanCalibComponentConfig modelConfig;

    public SwanCalibStochModelConfig(File configRootDir, SwanCalibComponentConfig modelConfig,
    		SwanCalibComponentConfig uncertaintyConfig) {

    	this.configRootDir = configRootDir;
        this.uncertaintyConfig = uncertaintyConfig;
        this.modelConfig = modelConfig;
    }

    public File getConfigRootDir() {
        return configRootDir;
    }

    public SwanCalibComponentConfig getUncertaintyConfig() {
        return uncertaintyConfig;
    }

    public SwanCalibComponentConfig getModelConfig() {
        return modelConfig;
    }

    public static class SwanCalibComponentConfig {

        private String className;
        private String [] arguments;
        private File workingDir;

        public SwanCalibComponentConfig(File workingDir, String className, String[] arguments) {
            this.workingDir = workingDir;
            this.className = className;
            this.arguments = arguments;
        }

        public String getClassName() {
            return className;
        }

        public String[] getArguments() {
            return arguments;
        }

        public File getWorkingDir() {
            return workingDir;
        }
    }
}