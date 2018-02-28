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

import org.openda.interfaces.*;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.Instance;
import org.openda.utils.Time;
import org.openda.utils.Vector;
import org.openda.utils.io.FileBasedModelState;

import java.io.File;
import java.lang.String;
import java.text.SimpleDateFormat;
import java.util.HashMap;
import java.util.List;

/**
 * Example of an OpenDa model that is accessing a model DLL
 */
public class D3dFlowModelInstance extends Instance implements IModelInstance {

	private HashMap<String, IPrevExchangeItem> modelExchangeItems = null;

	private int modelInstanceId;
	private File directoryForRestartFiles = null;
	private int currentTimeStep = 0;

	private double lastTimeStepMJD = 0.0;
	private ITime timeHorizon = null;
	private File modelDir;

	public D3dFlowModelInstance(File modelDir, D3dFlowModelConfig d3dFlowModelConfig, IStochModelFactory.OutputLevel outputLevel) {
		this.modelDir = modelDir;

		this.modelInstanceId = D3dFlowDll.createInstance(modelDir);

		this.directoryForRestartFiles = d3dFlowModelConfig.getDirectoryForSavedStates();

		if (this.modelExchangeItems == null) {
			this.modelExchangeItems = new HashMap<String, IPrevExchangeItem>();

			for (D3dFlowExchangeItemConfig exchangeItemConfig : d3dFlowModelConfig.getExchangeItemList()) {
				String exchangeItemIdInConfig = exchangeItemConfig.getId();
				int exchangeItemHandle = D3dFlowDll.getBoundaryExchangeItemID(
						exchangeItemIdInConfig, exchangeItemConfig.getType());
				if (exchangeItemConfig.getMetatype().equalsIgnoreCase("Forcings")) {
					// boundary conditions
					D3dBoundaryExchangeItem exchangeItem = new D3dBoundaryExchangeItem(
							modelDir,
							exchangeItemIdInConfig, exchangeItemConfig.getType(),
							exchangeItemHandle, IPrevExchangeItem.Role.Input,
							this.modelInstanceId);
					this.modelExchangeItems.put(exchangeItemIdInConfig, exchangeItem);
				} else {
					// results (monitor stations)
					D3dResultExchangeItem exchangeItem;
					String exchangeItemId = exchangeItemIdInConfig + ".waterlevel";
					if (exchangeItemConfig.getType() == D3dFlowExchangeItemConfig.EI_waterlevel) {
						exchangeItem = new D3dResultExchangeItem(
								modelDir,
								exchangeItemId, exchangeItemHandle, IPrevExchangeItem.Role.Output,
								this.modelInstanceId);
					} else {
						throw new RuntimeException("Invalid exchange item type: " + exchangeItemIdInConfig);
					}
					this.modelExchangeItems.put(exchangeItemId, exchangeItem);
				}
			}


			// state
			D3dStateExchangeItem stateExchangeItem = new D3dStateExchangeItem(modelDir, "state", this.modelInstanceId);
			this.modelExchangeItems.put(stateExchangeItem.getId(), stateExchangeItem);
		}
	}

    public void initialize(File workingDir, String[] arguments) {
        // no action needed (handled by constructors)
    }

	public ITime getTimeHorizon() {
		if (timeHorizon == null) {
			D3dFlowDll.selectInstance(modelDir, this.modelInstanceId);
			timeHorizon = D3dFlowDll.getTimeHorizon();
		}
		return timeHorizon;
	}

	public ITime getCurrentTime() {
    	D3dFlowDll.selectInstance(modelDir, this.modelInstanceId);
		return new Time(D3dFlowDll.getCurrentTime());
	}

	public void compute(ITime targetTime) {
		D3dFlowDll.selectInstance(modelDir, this.modelInstanceId);
		ITime currentTime = getCurrentTime();
		while (targetTime.getMJD() > (currentTime.getMJD() + 1.0E-6*lastTimeStepMJD)) {
			// note: this criterion should match the one when collecting observations!
			double oldcurrentTimeMJD = currentTime.getMJD();
			D3dFlowDll.performTimeStep(modelDir, ++currentTimeStep);
			currentTime = getCurrentTime();
			lastTimeStepMJD = currentTime.getMJD() - oldcurrentTimeMJD;
		}
	}

    public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance) {
        D3dFlowDll.selectInstance(modelDir, this.modelInstanceId);
        List<IPrevExchangeItem> observationExchangeItems = observationDescriptions.getExchangeItems();
        IVector[] observedLocalization = new IVector[observationExchangeItems.size()];
        for (int i = 0, observationExchangeItemsSize = observationExchangeItems.size(); i < observationExchangeItemsSize; i++) {
            IPrevExchangeItem exchangeItem = observationExchangeItems.get(i);
            String[] location = exchangeItem.getId().split("\\.");
            int stateSize = D3dFlowDll.getStateSize();
            double[] values = D3dFlowDll.getObservedLocalization(location[0], distance, stateSize);
            observedLocalization[i] = new Vector(values);
        }
        return observedLocalization;
    }

    public String[] getExchangeItemIDs() {
		return this.modelExchangeItems.keySet().toArray(new String[this.modelExchangeItems.keySet().size()]);
	}

	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		if (role == IPrevExchangeItem.Role.InOut) {
			return getExchangeItemIDs();
		}
		throw new UnsupportedOperationException("getExchangeItemIDs(role = in or out): Selection not yet implemented yet.");
	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		throw new UnsupportedOperationException("nl.deltares.openda.models.d3dflow.dll.D3dFlowModelInstance.getDataObjectExchangeItem(): Not implemented yet.");
	}

	public IPrevExchangeItem getExchangeItem(String exchangeItemId) {
		IPrevExchangeItem exchangeItem = this.modelExchangeItems.get(exchangeItemId);
		if (exchangeItem == null) {
			throw new RuntimeException("Invalid exchange item id: " + exchangeItemId);
		}
		return exchangeItem;
	}

	public IModelState saveInternalState() {
		checkRestartFileDirectory();
		String restartFileName = determineRestartFileName(getCurrentTime());
		String netcdfRestartFilePath = new File(directoryForRestartFiles, restartFileName).getAbsolutePath();
		D3dFlowDll.storeCurrentInstanceRestartfile(modelDir, netcdfRestartFilePath);
		return new FileBasedModelState(directoryForRestartFiles, new File(netcdfRestartFilePath).getAbsolutePath());
	}

	public void restoreInternalState(IModelState savedModelState) {
		if (!(savedModelState instanceof FileBasedModelState)) {
			throw new IllegalArgumentException("Unknown state type (" + savedModelState.getClass().getName() +
					" for " + this.getClass().getName() + ".releaseInternalState");
		}
		checkRestartFileDirectory();
		FileBasedModelState modelState = (FileBasedModelState) savedModelState;
		modelState.setDirContainingModelstateFiles(directoryForRestartFiles);
		modelState.restoreState();
		String restartFilePath = modelState.getFilesInModelState().get(0).getAbsolutePath();
		D3dFlowDll.selectInstanceFromRestartfile(modelDir, this.modelInstanceId, restartFilePath);
	}

	public void releaseInternalState(IModelState savedModelState) {
		if (!(savedModelState instanceof FileBasedModelState)) {
			throw new IllegalArgumentException("Unknown state type (" + savedModelState.getClass().getName() +
					" for " + this.getClass().getName() + ".releaseInternalState");
		}
		FileBasedModelState modelState = (FileBasedModelState) savedModelState;
		modelState.releaseState(directoryForRestartFiles);
	}

	public IModelState loadPersistentState(File persistentStateFile) {
		return FileBasedModelState.loadPersistenState(persistentStateFile, this.directoryForRestartFiles);
	}

	public File getModelRunDir() {
		return modelDir;
	}

	public void finish() {
		// no model specific actions needed
	}

	private String determineRestartFileName(ITime time) {
		String timeString = new SimpleDateFormat("yyyyMMdd_HHmmss").format(Time.timeStampToDate(time));
		return "d3d_state_" + timeString + ".nc";
	}

	private void checkRestartFileDirectory() {
		if (!this.directoryForRestartFiles.exists()) {
			if (!this.directoryForRestartFiles.mkdirs()) {
				throw new RuntimeException("Could not create dir. for restart files: " +
						this.directoryForRestartFiles);
			}
		}
	}
}
