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
package org.openda.model_delft3d.dll;

import org.openda.blackbox.config.AliasDefinitions;

import java.io.File;
import java.util.Collection;
import java.util.List;

/**
 * Configuration for DLL Based Delft3d Flow wrapper
 */
public class D3dFlowModelConfig {

	public enum DllType {
        win32_ifort, linux64_gnu
    }

    private DllType dllType;
    private String d3dFlowDllPath;
    private File modelDir;
	private File directoryForSavedStates;
	private String inputFileName;
	private String[] arguments;
	private int maxNumInstancesInMemory;
    private List<D3dFlowExchangeItemConfig> exchangeItemList;

	public D3dFlowModelConfig(File configRootDir,
							  String binDirName, String platformName, String dllName,
							  String modelDirString, String inputFileName, String[] arguments,
							  String directoryForSavedStatesName,
							  int maxNumInstancesInMemory,
							  List<D3dFlowExchangeItemConfig> exchangeItemList,
							  AliasDefinitions aliasDefinitions) {
		this.maxNumInstancesInMemory = maxNumInstancesInMemory;
		this.exchangeItemList = exchangeItemList;

        if (platformName.equalsIgnoreCase("win32_ifort")) {
            dllType = DllType.win32_ifort;
        } else if(platformName.equalsIgnoreCase("linux64_gnu")) {
        	dllType = DllType.linux64_gnu;
        } else {
            throw new RuntimeException("D3dFlowModelConfig: unrecognized platform: " + platformName);
        }

        this.modelDir = new File(configRootDir, modelDirString);
        if (!this.modelDir.exists()) {
            throw new RuntimeException(
                    "D3dFlowModelConfig: model directory does not exist: " + this.modelDir.getAbsolutePath());
        }
		this.directoryForSavedStates = new File(configRootDir, directoryForSavedStatesName);

		if (binDirName != null) {
			Collection<String> usedAliasIds = aliasDefinitions.getUsedAliasIds(binDirName);
			aliasDefinitions.validate();
			binDirName = aliasDefinitions.apply(binDirName, usedAliasIds);
			File binDir = new File(configRootDir, binDirName);
            if (!binDir.exists()) {
				// Relative bin. dir specificaton not valid. Check if the binDir is an absolute path.
				File[] driveRoots = File.listRoots();
				boolean binDirNameIsAbsolutePath = false;
				for (File driveRoot : driveRoots) {
					if (binDirName.toLowerCase().startsWith(driveRoot.getAbsolutePath().toLowerCase())) {
						binDirNameIsAbsolutePath = true;
					}
				}
				if (binDirNameIsAbsolutePath) {
					binDir = new File(binDirName);
				}
			}
			if (!binDir.exists()) {
				throw new RuntimeException("D3dFlowModelConfig: bin. dir. does not exist: " +
						binDir.getAbsolutePath());
			}
			File nativeDll = new File(binDir, dllName);
            if (!nativeDll.exists()) {
                throw new RuntimeException("D3dFlowModelConfig: native DLL does not exist: " +
                        nativeDll.getAbsolutePath());
            }
            d3dFlowDllPath = nativeDll.getAbsolutePath();
        }

        if (d3dFlowDllPath == null) {
            // no dir./dll. path specified, pass DLL name direct to init function
            d3dFlowDllPath = dllName;
        }

        File mdFile = new File(this.modelDir, inputFileName);
        if (!mdFile.exists()) {
            throw new RuntimeException(
                    "D3dFlowModelConfig: model definition file does not exist: " + mdFile.getAbsolutePath());
        }
        this.inputFileName = mdFile.getName();

        if (exchangeItemList.size() == 0) {
            throw new RuntimeException(
                    "D3dFlowModelConfig: omitting list of exchange items not yet implemented");
        }

		this.arguments = arguments;
	}

    public DllType getDllType() {
        return dllType;
    }

    public String getD3dFlowDllPath() {
        return d3dFlowDllPath;
    }

    public File getModelDir() {
        return modelDir;
    }

	public File getDirectoryForSavedStates() {
		return directoryForSavedStates;
	}

	public String getInputFileName() {
        return inputFileName;
    }

	public String[] getArguments() {
		return arguments;
	}

	public int getMaxNumInstancesInMemory() {
		return maxNumInstancesInMemory;
	}

	public List<D3dFlowExchangeItemConfig> getExchangeItemList() {
        return exchangeItemList;
    }
}
