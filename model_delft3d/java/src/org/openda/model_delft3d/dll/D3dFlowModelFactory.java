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
package org.openda.model_delft3d.dll;

import org.openda.blackbox.interfaces.IModelFactory;
import org.openda.interfaces.IModelInstance;
import org.openda.interfaces.IStochModelFactory;
import org.openda.utils.io.FileRemover;

import java.io.File;


public class D3dFlowModelFactory implements IModelFactory {

    private D3dFlowModelConfig d3dFlowModelConfig = null;
	File modelDir = null;

	public void initialize(File configRootDir, String[] arguments) {
        if (arguments.length != 1) {
            throw new RuntimeException("Argument expected: schematization file");
        }
        D3dFlowModelConfigReader modelConfigReader = new D3dFlowModelConfigReader(new File(configRootDir, arguments[0]));
        d3dFlowModelConfig = modelConfigReader.getD3dFlowModelConfig();
		FileRemover fileRemover = new FileRemover();
		String[] filesFromPreviousRuns = {"trih-*.*", "trim-*.*", "tri-diag*.*",
				"ArmaNoiseModel-log*.*", "cta_state_*.*", "TMPstate_*.*", "TMP_*.*"};
		modelDir = d3dFlowModelConfig.getModelDir();
		fileRemover.initialize(modelDir, filesFromPreviousRuns);
		D3dFlowDll.loadDLL(d3dFlowModelConfig.getDllType(),
						   d3dFlowModelConfig.getD3dFlowDllPath());
		D3dFlowDll.initializeModel(modelDir,
				d3dFlowModelConfig.getInputFileName(),
				d3dFlowModelConfig.getMaxNumInstancesInMemory());
    }

    public IModelInstance getInstance(String[] arguments, IStochModelFactory.OutputLevel outputLevel) {
        return new D3dFlowModelInstance(modelDir, d3dFlowModelConfig, outputLevel);
    }


	//to finalize the corresponding dll manually (only for main model):
	public void finish() {
		D3dFlowDll.finish(modelDir);
		d3dFlowModelConfig = null;
		modelDir = null;
	}

    
    public void finalize() {
		// TODO: reactivate when 'finish instance' has been realized in D3dFlowModelInstance'
		// For now: d3dFlowDll.finish() is called when closing main model
		D3dFlowDll.finish(modelDir);
        try {
            super.finalize();
        } catch (Throwable ignored) {
        }
    }
}
