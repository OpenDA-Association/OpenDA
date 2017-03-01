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

package org.openda.blackbox.wrapper;

import org.openda.blackbox.config.BBModelConfig;
import org.openda.blackbox.config.BBModelConfigReader;
import org.openda.blackbox.config.BBUtils;
import org.openda.blackbox.config.BBWrapperConfig;
import org.openda.blackbox.interfaces.IModelFactory;
import org.openda.blackbox.interfaces.ITimeHorizonConsumer;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.ITime;
import org.openda.utils.DistributedCounter;

import java.io.File;

/**
 * Factory for creating instances of the type BBModelInstance.
 */
public class BBModelFactory implements IModelFactory, ITimeHorizonConsumer {

    protected BBModelConfig bbModelConfig;
    protected DistributedCounter instanceNumber;
	protected ITime timeHorizon = null;  // only used when timeHorizon is set from outside.
		// An alternative is the use of timeExchangeItems

    /**
     * Empty constructor. Will be called before the initialize call is called.
     */
    public BBModelFactory() {
		this.instanceNumber=new DistributedCounter(0);
    }

    /**
     * {@inheritDoc}
     */
    public void initialize(File workingDir, String[] arguments) {
        BBModelConfigReader modelConfigReader = new BBModelConfigReader(new File(workingDir, arguments[0]));
        this.bbModelConfig = modelConfigReader.getBBModelConfig();
        bbModelConfig.getWrapperConfig().validate();
		this.instanceNumber=new DistributedCounter(0);
        cleanUpModelEnsembleDirectories();
    }

	public void setTimeHorizon(ITime timeHorizon) {
		this.timeHorizon = timeHorizon;
	}

    public BBModelFactory(BBModelConfig bbModelConfig) {
        this();
        this.bbModelConfig = bbModelConfig;
        this.bbModelConfig.getWrapperConfig().validate();
        if (!bbModelConfig.skipModelActionsIfInstanceDirExists()) cleanUpModelEnsembleDirectories();
    }

    public BBModelInstance getInstance(String[] arguments, IStochModelFactory.OutputLevel outputLevel) {
        int newInstanceNumber=instanceNumber.val();
		instanceNumber.inc();
        return new BBModelInstance(this.bbModelConfig, newInstanceNumber, this.timeHorizon);
    }

	public void finish() {
		// no action needed (yet)
	}

	private void cleanUpModelEnsembleDirectories() {
        int tries = 0;
        for (int i = 0; i < 1000; i++) { // fixed number at present
            if (tries > 10) break; // just to avoid going through the whole loop
            // tries is the counter to check how many times I cannot find the directory
            if (!removeInstance(bbModelConfig, i)) {
                tries++;
            }
        }
    }

    public boolean removeInstance(BBModelConfig bbModelConfig, int instanceNumber) {

        this.bbModelConfig = bbModelConfig;
        File configRootDir = bbModelConfig.getConfigRootDir();

        // Update alias with instance number
        String instanceNumberString = BBModelInstance.getInstanceNumberString(bbModelConfig, instanceNumber);
        bbModelConfig.getWrapperConfig().getAliasDefinitions().setAliasValue("instanceNumber", instanceNumberString);

        BBWrapperConfig bbWrapperConfig = bbModelConfig.getWrapperConfig();
        if (bbWrapperConfig.getCloneType() != BBWrapperConfig.CloneType.None) {
            File instanceFileOrDir = new File(configRootDir, bbModelConfig.getWrapperConfig().getInstanceName());
            if (instanceFileOrDir.exists()) {
                BBUtils.deleteFileOrDir(instanceFileOrDir);
                return true;
            }
        }
        return false;
    }
}
