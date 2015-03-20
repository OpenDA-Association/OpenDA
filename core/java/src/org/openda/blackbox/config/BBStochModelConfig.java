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


package org.openda.blackbox.config;

import java.io.File;

/**
 * TODO: description
 */
public class BBStochModelConfig {

    private BBModelConfig bbModelConfig;
    private File modelFactoryWorkingDir;
    private BBAction modelFactoryAction;
    private BBAction uncertaintyAction;
    private BBStochModelVectorsConfig bbStochModelVectorsConfig;
    private File uncertaintyWorkingDir;
	String restartStatesDirPrefix;
	String restartStatesNoiseModelPrefix;
	String modelRestartStateFile;
    boolean useUncertaintyEngine;

    public BBStochModelConfig(BBModelConfig bbModelConfig,
							  File modelFactoryWorkingDir, BBAction modelFactoryAction,
							  File uncertaintyWorkingDir, BBAction uncertaintyAction,
							  BBStochModelVectorsConfig bbStochModelVectorsConfig,
							  String restartStatesDirPrefix,
							  String restartStatesNoiseModelPrefix,
							  String modelRestartStateFile,
                              boolean useUncertaintyEngine) {
        this.bbModelConfig = bbModelConfig;
        this.modelFactoryWorkingDir = modelFactoryWorkingDir;
        this.modelFactoryAction = modelFactoryAction;
        this.uncertaintyWorkingDir = uncertaintyWorkingDir;
        this.uncertaintyAction = uncertaintyAction;
        this.bbStochModelVectorsConfig = bbStochModelVectorsConfig;
		this.restartStatesDirPrefix = restartStatesDirPrefix;
		this.restartStatesNoiseModelPrefix = restartStatesNoiseModelPrefix;
		this.modelRestartStateFile = modelRestartStateFile;
        this.useUncertaintyEngine = useUncertaintyEngine;
	}

    public BBModelConfig getBbModelConfig() {
        return bbModelConfig;
    }

    public File getModelFactoryWorkingDir() {
        return modelFactoryWorkingDir;
    }

    public BBAction getModelFactoryAction() {
        return modelFactoryAction;
    }

    public File getUncertaintyWorkingDir() {
        return uncertaintyWorkingDir;
    }

    public BBAction getUncertaintyAction() {
        return uncertaintyAction;
    }

    public BBStochModelVectorsConfig getBbStochModelVectorsConfig() {
        return bbStochModelVectorsConfig;
    }

	public String getRestartStatesDirPrefix() {
		return restartStatesDirPrefix;
	}

	public String getRestartStatesNoiseModelPrefix() {
		return restartStatesNoiseModelPrefix;
	}

	public String getModelRestartStateFile() {
		return modelRestartStateFile;
	}

}
