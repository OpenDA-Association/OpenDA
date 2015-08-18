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

import org.openda.blackbox.config.*;
import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.blackbox.interfaces.SelectorInterface;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.*;
import org.openda.utils.Instance;
import org.openda.utils.Results;
import org.openda.utils.Time;
import org.openda.utils.io.FileBasedModelState;

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.*;

/**
 * Black box module's implementation of a model instance
 */
public class BBModelInstance extends Instance implements IModelInstance {
    private final String ALL_ELEMENTS_FROM_IO_OBJECT = "allElementsFromIoObject";

    protected BBModelConfig bbModelConfig;

    private File instanceFileOrDir;
    private HashMap<String, IoObjectInterface> ioObjects;
    private HashMap<String, IDataObject> dataObjects;
    protected HashMap<String, BBExchangeItem> bbExchangeItems = new HashMap<String, BBExchangeItem>();
    protected HashMap<String, SelectorInterface> selectors;
    private boolean doAdditionalComputeActions = false;
    private ITime currentTime = null;
    private ITime timeHorizon = null;
    boolean newInstanceDir = true;
    int instanceNumber = -1; //default invalid
    AliasDefinitions aliasDefinitions=null;

    /**
     * Create new blackbox model instance. You should not need to call this routine manually, but use the
     *  getInstance method from the factory instead. 
     * @param bbModelConfig configuration, typically parsed from an input file.
     * @param instanceNumber number for this copy of the model. Starts at 0 and does not reuse numbers
     *  even when a model has been cleaned from memory.
     * @param timeHorizon feed this timeHorizon from outside to the model. If it is null then use the
     * configuration or defaults to zet the timeHorizon
     */
	public BBModelInstance(BBModelConfig bbModelConfig, int instanceNumber, ITime timeHorizon) {
		Results.putMessage("Create new BBModelInstance with number: "+instanceNumber);
		this.bbModelConfig = bbModelConfig;
		File configRootDir = bbModelConfig.getConfigRootDir();

		// Update alias with instance number
		this.instanceNumber=instanceNumber;
		this.aliasDefinitions=bbModelConfig.getWrapperConfig().getAliasDefinitions().clone();
		this.aliasDefinitions.setAliasValue("instanceNumber", String.valueOf(instanceNumber));
		
		// Create clone of template directory
        BBWrapperConfig bbWrapperConfig = bbModelConfig.getWrapperConfig();
        if (bbWrapperConfig.getCloneType() != BBWrapperConfig.CloneType.None) {
			File templateFileOrDir = new File(configRootDir, bbModelConfig.getWrapperConfig().getTemplateName(this.aliasDefinitions));
			instanceFileOrDir = new File(configRootDir, bbModelConfig.getWrapperConfig().getInstanceName(this.aliasDefinitions));
			if (instanceFileOrDir.exists() && bbModelConfig.skipModelActionsIfInstanceDirExists()) {
                newInstanceDir = false;
			} else {
				if (bbWrapperConfig.getCloneType() == BBWrapperConfig.CloneType.Directory) {
					BBUtils.makeDirectoryClone(templateFileOrDir, instanceFileOrDir);
				} else if (bbWrapperConfig.getCloneType() == BBWrapperConfig.CloneType.File) {
					BBUtils.makeFileClone(templateFileOrDir, instanceFileOrDir);
				}
			}
		}

        ioObjects = new HashMap<String, IoObjectInterface>();
        dataObjects = new HashMap<String, IDataObject>();
        selectors = new HashMap<String, SelectorInterface>();
        System.out.println("Start Instance initialization");
		if (newInstanceDir || !bbModelConfig.skipModelActionsIfInstanceDirExists()) {
			for (BBAction action : bbWrapperConfig.getInitializeActions()) {
				action.run(configRootDir,this.aliasDefinitions);
			}
		}

		//
		// Time
		//
		// Find the timeHorizon for this model instance
		if (timeHorizon == null) { // use configuration
			// no time horizon specified by 'outside world', determine the time horizon
			// from the BBModelConfig or from the StartTime/StopTime-exchange items.
			determineTimeHorizon();
		}else{ // set from outside
			this.timeHorizon = timeHorizon;
		}
		this.currentTime = this.timeHorizon.getBeginTime();
		double currentTimeAsMjd=this.timeHorizon.getBeginTime().getMJD();
		this.aliasDefinitions.setAliasValue("currentTime", TimeUtils.mjdToString(currentTimeAsMjd));
		this.aliasDefinitions.setAliasValue("targetTime", TimeUtils.mjdToString(this.timeHorizon.getEndTime().getMJD()));
		
		if(!this.aliasDefinitions.containsKey("startTime")){
			this.aliasDefinitions.add("startTime", "%", "%", null, null);
		}
		this.aliasDefinitions.setAliasValue("startTime", TimeUtils.mjdToString(this.timeHorizon.getBeginTime().getMJD()));
		if(!this.aliasDefinitions.containsKey("endTime")){
			this.aliasDefinitions.add("endTime", "%", "%", null, null);
		}
		this.aliasDefinitions.setAliasValue("endTime", TimeUtils.mjdToString(this.timeHorizon.getEndTime().getMJD()));
		// Feed the time horizon to the model.
		feedComputationSpanToModel(this.timeHorizon.getBeginTime(), this.timeHorizon.getEndTime());
		
		Results.putMessage("Instance initialization done");
	}

    private void determineTimeHorizon() {
        ITime startTime = bbModelConfig.getStartTime();
        if (startTime == null) {
                // start time not in model config get it from exchange item
                checkForPendingComputeActions();

            //use first found time that is not null.
            String[] startTimeExchangeItemIds = bbModelConfig.getStartTimeExchangeItemIds();
            if (startTimeExchangeItemIds != null) {
                for (int n = 0; n < startTimeExchangeItemIds.length; n++) {
                    startTime = getStartOrEndTime(startTimeExchangeItemIds[n]);
                    if (startTime != null) {
                        break;
                    }
                }
            }
        }
        ITime endTime = bbModelConfig.getEndTime();
        if (endTime == null) {
                // end time not in model config get it from exchange item
                checkForPendingComputeActions();

            //use first found time that is not null.
            String[] endTimeExchangeItemIds = bbModelConfig.getEndTimeExchangeItemIds();
            if (endTimeExchangeItemIds != null) {
                for (int n = 0; n < endTimeExchangeItemIds.length; n++) {
                    endTime = getStartOrEndTime(endTimeExchangeItemIds[n]);
                    if (endTime != null) {
                        break;
                    }
                }
            }
        }
        double timeStepMJD = bbModelConfig.getTimeStepMJD();
        if (Double.isNaN(timeStepMJD)){
            String[] timeStepExchangeItemIds = bbModelConfig.getTimeStepExchangeItemIds();
            if (timeStepExchangeItemIds!=null){
                if (timeStepExchangeItemIds[0] != null){
                    for (int n=0; n < timeStepExchangeItemIds.length; n++){
                        IPrevExchangeItem timeExchangeItem = getExchangeItem(timeStepExchangeItemIds[n]);
                        timeStepMJD = timeExchangeItem.getValuesAsDoubles()[0];
                        if (!Double.isNaN(timeStepMJD)){
                             break;
                        }
                    }
                }
            }
        }
        this.timeHorizon = new Time(startTime, endTime, timeStepMJD);
    }


	
    public boolean isNewDirectory(){
        return this.newInstanceDir;
    }

    public ITime getTimeHorizon() {
        return this.timeHorizon;
    }

    public ITime getCurrentTime() {
        return currentTime;
    }

    public void compute(ITime targetTime) {

        // update instanceNumber in shared config
        this.aliasDefinitions.setAliasValue("instanceNumber", String.valueOf(instanceNumber));

        
        checkForPendingComputeActions();

        // update times
        if (this.aliasDefinitions.containsKey("currentTime")) {
        	this.aliasDefinitions.setAliasValue(
        			"currentTime", TimeUtils.mjdToString(getCurrentTime().getMJD()));
        }
        if (targetTime != null) {
        	if (this.aliasDefinitions.containsKey("targetTime")) {
        		this.aliasDefinitions.setAliasValue(
        				"targetTime", TimeUtils.mjdToString(targetTime.getMJD()));
        	}
        }

        feedComputationSpanToModel(currentTime, targetTime);

        flushAndClearDataObjects(true);

        // perform computation actions
        System.out.println("Start Instance computation, targetTime=" + targetTime);
		if (newInstanceDir | !bbModelConfig.skipModelActionsIfInstanceDirExists()) {
			for (BBAction action : bbModelConfig.getWrapperConfig().getComputeActions()) {
				action.run(instanceFileOrDir,this.aliasDefinitions);
			}
		}
		System.out.println("Instance computation done");

        setCheckForPendingComputeActions();

        if (targetTime != null) {
            currentTime = targetTime;
        }
    }

	/**
	 * Note: this method also works in combination with the option "allElementsFromIoObject".
	 */
	public String[] getExchangeItemIDs() {
		checkForPendingComputeActions();

		List<String> exchangeItemIds = new ArrayList<String>();
		for (BBModelVectorConfig vectorConfig : bbModelConfig.getVectorConfigs()) {
			addAllExchangeItemIdsFromVectorConfig(vectorConfig, exchangeItemIds);
		}

		return exchangeItemIds.toArray(new String[exchangeItemIds.size()]);
	}

	/**
	 * Note: this method also works in combination with the option "allElementsFromIoObject".
	 */
	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		checkForPendingComputeActions();

		List<String> exchangeItemIds = new ArrayList<String>();
		for (BBModelVectorConfig vectorConfig : bbModelConfig.getVectorConfigs()) {
			if (!role.equals(vectorConfig.getRole())) continue;

			addAllExchangeItemIdsFromVectorConfig(vectorConfig, exchangeItemIds);
		}

		return exchangeItemIds.toArray(new String[exchangeItemIds.size()]);
	}

	/**
	 * Adds all exchangeItemIds from the given vectorConfig to the given exchangeItemIds list.
	 */
	private void addAllExchangeItemIdsFromVectorConfig(BBModelVectorConfig vectorConfig, List<String> exchangeItemIds) {
		if (vectorConfig.getId().equalsIgnoreCase(ALL_ELEMENTS_FROM_IO_OBJECT)) {
			//add all exchange item ids from ioObject or dataObject.
			IoObjectInterface ioObject = findOrCreateIoObject(vectorConfig.getIoObjectConfig());
			if (ioObject != null) {
				for (IPrevExchangeItem ioObjectExchangeItem : ioObject.getExchangeItems()) {
					exchangeItemIds.add(ioObjectExchangeItem.getId());
				}
			} else {
				IDataObject dataObject = findOrCreateDataObject(vectorConfig.getIoObjectConfig());
				if (dataObject != null) {
					Collections.addAll(exchangeItemIds, dataObject.getExchangeItemIDs());
				} else {
					throw new IllegalArgumentException("IoObject or DataObject could not be created for \"" + vectorConfig.getIoObjectConfig().getId(this.aliasDefinitions) + "\"");
				}
			}
		} else {
			exchangeItemIds.add(vectorConfig.getId());
		}
	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		IPrevExchangeItem exchangeItem = getExchangeItem(exchangeItemID);
		if (!(exchangeItem instanceof BBExchangeItem)) {
			throw new RuntimeException("Unexpected exchange item type " + exchangeItem.getClass().toString());
		}
		return (BBExchangeItem) exchangeItem;
	}

	public IPrevExchangeItem getExchangeItem(String exchangeItemId) {
        checkForPendingComputeActions();
        BBExchangeItem bbExchangeItem = bbExchangeItems.get(exchangeItemId);
        if (bbExchangeItem == null) {
			List<String> newExchangeItemIDs = new ArrayList<String>();
			BBModelVectorConfig vectorConfig = findVectorConfig(exchangeItemId);
			if (vectorConfig != null) {
                IoObjectInterface ioObject = findOrCreateIoObject(vectorConfig.getIoObjectConfig());
                if (ioObject != null) {
                    for (IPrevExchangeItem ioObjectExchangeItem : ioObject.getExchangeItems()) {
                        if (ioObjectExchangeItem.getId().equalsIgnoreCase(vectorConfig.getSourceId())) {
                            bbExchangeItem = new BBExchangeItem(exchangeItemId, vectorConfig, ioObjectExchangeItem,
                                    selectors, bbModelConfig.getConfigRootDir());
                            break;
                        }
                        newExchangeItemIDs.add(ioObjectExchangeItem.getId());
                    }
                } else {
                    IDataObject dataObject = findOrCreateDataObject(vectorConfig.getIoObjectConfig());
                    if (dataObject != null) {
                        for (String exchangeItemID : dataObject.getExchangeItemIDs()) {
                            if (exchangeItemID.equalsIgnoreCase(vectorConfig.getSourceId())) {
                                bbExchangeItem = new BBExchangeItem(exchangeItemId, vectorConfig,
                                        dataObject.getDataObjectExchangeItem(exchangeItemID),
                                        selectors, bbModelConfig.getConfigRootDir());
                                break;
                            }
                            newExchangeItemIDs.add(exchangeItemID);
                        }
                    } else {
                        throw new IllegalArgumentException("IoObject or DataObject could not be created for \"" +
                                vectorConfig.getIoObjectConfig().getId(this.aliasDefinitions) + "\"");
                    }
                }
			} else {
				List<BBModelVectorConfig> allElementVectorConfigs = findAllElementsVectorConfig();
				if (allElementVectorConfigs.size() == 0) {
					throw new IllegalArgumentException("IO selection subvector not found for \"" + exchangeItemId + "\"");
				}
                for (BBModelVectorConfig allElementVectorConfig : allElementVectorConfigs) {
                    IoObjectInterface ioObject = findOrCreateIoObject(allElementVectorConfig.getIoObjectConfig());
                    if (ioObject != null) {
                        for (IPrevExchangeItem ioObjectExchangeItem : ioObject.getExchangeItems()) {
                            if (ioObjectExchangeItem.getId().equalsIgnoreCase(exchangeItemId)) {
                                bbExchangeItem = new BBExchangeItem(exchangeItemId, allElementVectorConfig,
                                        ioObjectExchangeItem,
                                        selectors, bbModelConfig.getConfigRootDir());
                                break;
                            }
                            newExchangeItemIDs.add(ioObjectExchangeItem.getId());
                        }
                    } else {
                        IDataObject dataObject = findOrCreateDataObject(allElementVectorConfig.getIoObjectConfig());
                        if (dataObject != null) {
                            for (String exchangeItemID : dataObject.getExchangeItemIDs()) {
                                if (exchangeItemID.equalsIgnoreCase(exchangeItemId)) {
                                    bbExchangeItem = new BBExchangeItem(exchangeItemId, allElementVectorConfig,
                                            dataObject.getDataObjectExchangeItem(exchangeItemID),
                                            selectors, bbModelConfig.getConfigRootDir());
                                    break;
                                }
                                newExchangeItemIDs.add(exchangeItemID);
                            }
                        } else {
                            throw new IllegalArgumentException("IoObject or DataObject could not be created for \"" +
                                    allElementVectorConfig.getIoObjectConfig().getId(this.aliasDefinitions) + "\"");
                        }
                    }

                    if (bbExchangeItem != null) {
                        break;
                    }
                }
            }

			if (bbExchangeItem != null) {
				bbExchangeItems.put(exchangeItemId, bbExchangeItem);
				return bbExchangeItem;
			}

			String allItems = "";
			for (String availableExchangeItemID : newExchangeItemIDs) {
				allItems += "   " + availableExchangeItemID + "\n";
			}
            throw new RuntimeException(
                      "BBModelInstance.getExchangeItem(" + exchangeItemId + "):\n" +
                      "Exchange item '" + exchangeItemId + "' not found\n" +
                      "Existing exchange items are:\n" +
                      allItems + "\n");
        }
        return bbExchangeItem;
    }

    public IModelState saveInternalState() {
        //flush and clear ioObjects and dataObjects before saving the state. This is needed so that
        //before the state is saved, the state exchangeItems are finished (if they are used).
        //This way the saved state will represent the latest data from the memory,
        //instead of previous data that happened to be still present in the state files.
        flushAndClearDataObjects(true);

		// copy the model's restart files to a subdirectory for the current time stap,
		// and gather them in a file based model state
		File savedStateDir = checkRestartDir(getCurrentTime(), false);
		for (String restartFileName : bbModelConfig.getRestartFileNames()) {
			File modelStateFile = new File(getModelRunDir(), restartFileName);
			File copyOfModelStateFile = new File(savedStateDir, restartFileName);
			try {
				BBUtils.copyFile(modelStateFile, copyOfModelStateFile);
			} catch (IOException e) {
				throw new RuntimeException("Could not copy " + modelStateFile.getAbsolutePath() + " to " +
				copyOfModelStateFile.getAbsolutePath() + ": " + e.getMessage());
			}
		}
		return new FileBasedModelState(savedStateDir);
	}

    public void restoreInternalState(IModelState savedInternalState) {
		if (!(savedInternalState instanceof FileBasedModelState)) {
			throw new IllegalArgumentException("Unknown state type (" + savedInternalState.getClass().getName() +
					" for " + this.getClass().getName() + ".restoreInternalState");
		}

        //clear ioObjects and dataObjects before restoring a previous state. This is needed so that
        //after a previous state has been restored, the state exchangeItems are initialized again from scratch
        //(when they are used). This way the state exchangeItems will represent the data from the restored state,
        //instead of previous data that happened to be still present in the memory.
        flushAndClearDataObjects(false);

		FileBasedModelState modelState = (FileBasedModelState) savedInternalState;
		File savedStateDir = checkRestartDir(getCurrentTime(), false);
		modelState.setDirContainingModelstateFiles(savedStateDir);
		modelState.restoreState();
		for (String restartFileName : bbModelConfig.getRestartFileNames()) {
			File modelStateFileInModelState = new File(savedStateDir, restartFileName);
			File modelStateFile = new File(getModelRunDir(), restartFileName);
			try {
				BBUtils.copyFile(modelStateFileInModelState, modelStateFile);
			} catch (IOException e) {
				throw new RuntimeException("Could not copy " + modelStateFileInModelState.getAbsolutePath() + " to " +
						modelStateFile.getAbsolutePath() + ": " + e.getMessage());
			}
		}
	}

    public void releaseInternalState(IModelState savedInternalState) {
		if (!(savedInternalState instanceof FileBasedModelState)) {
			throw new IllegalArgumentException("Unknown state type (" + savedInternalState.getClass().getName() +
					" for " + this.getClass().getName() + ".releaseInternalState");
		}
		File dirForRestartFiles = checkRestartDir(getCurrentTime(), false);
		FileBasedModelState modelState = (FileBasedModelState) savedInternalState;
		modelState.releaseState(dirForRestartFiles);
	}

	public IModelState loadPersistentState(File persistentStateFile) {
		File dirForRestartFiles = checkRestartDir(getCurrentTime(), false);
		return FileBasedModelState.loadPersistenState(persistentStateFile, dirForRestartFiles);
	}

   public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance){
		throw new UnsupportedOperationException("org.openda.blackbox.wrapper.BBModelInstance..getObservedLocalization(): Not implemented yet.");
	}

    public File getModelRunDir() {
        return instanceFileOrDir;
    }

    public void finish() {

        checkForPendingComputeActions();

        flushAndClearDataObjects(false);

        System.out.println("Start Instance finalization");
		if (!bbModelConfig.skipModelActionsIfInstanceDirExists()) {
			for (BBAction action : bbModelConfig.getWrapperConfig().getFinalizeActions()) {
				action.run(instanceFileOrDir,this.aliasDefinitions);
			}
		}
		if (bbModelConfig.doCleanUp()) {
            if (instanceFileOrDir.isDirectory()) {
                BBUtils.deleteDirectory(instanceFileOrDir);
            } else {
                if (!instanceFileOrDir.delete()) {

                }
            }
        }
	}

	public static File createDirectoryForSavedState(ITime time, boolean mustExist, File savedStatesRootDir, String savedStatesDirPrefix) {
		String timeString = new SimpleDateFormat("yyyyMMdd_HHmmss").format(Time.timeStampToDate(time));
		File dirForRestartFiles;
        if (new File(savedStatesDirPrefix).isAbsolute()) {
            dirForRestartFiles = new File(savedStatesDirPrefix + timeString);
        } else {
            dirForRestartFiles = new File(savedStatesRootDir, savedStatesDirPrefix + timeString);
        }
        if (mustExist) {
			if (!dirForRestartFiles.exists()) {
				throw new RuntimeException("Dir for restart files not found: " + dirForRestartFiles.getAbsolutePath());
			}
		} else {
			if (dirForRestartFiles.exists()) {
				dirForRestartFiles.delete();
			}
			dirForRestartFiles.mkdirs();
		}
		return dirForRestartFiles;
	}

	private File checkRestartDir(ITime time, boolean mustExist) {
		if (this.bbModelConfig.getSavedStatesDirPrefix() == null) {
			throw new RuntimeException("Dir for restart files not specified in black box model config file on dir. " +
					bbModelConfig.getConfigRootDir().getAbsolutePath());
		}
		return BBModelInstance.createDirectoryForSavedState(time, mustExist,
				getModelRunDir(), this.bbModelConfig.getSavedStatesDirPrefix());
	}

	private BBModelVectorConfig findVectorConfig(String exchangeItemId) {
		for (BBModelVectorConfig vectorConfig : bbModelConfig.getVectorConfigs()) {
			if (vectorConfig.getId().equalsIgnoreCase(exchangeItemId)) {
				return vectorConfig;
			}
		}
		return null;
	}

	private List<BBModelVectorConfig> findAllElementsVectorConfig() {
		List<BBModelVectorConfig> allElementsVectorConfigs = new ArrayList<BBModelVectorConfig>();
		for (BBModelVectorConfig vectorConfig : bbModelConfig.getVectorConfigs()) {
			if (vectorConfig.getId().equalsIgnoreCase(ALL_ELEMENTS_FROM_IO_OBJECT)) {
				allElementsVectorConfigs.add(vectorConfig);
			}
		}
		return allElementsVectorConfigs;
	}


	private IoObjectInterface findOrCreateIoObject(IoObjectConfig ioObjectConfig) {

		// find or create io object
		IoObjectInterface ioObject = ioObjects.get(ioObjectConfig.getId(this.aliasDefinitions));
		if (ioObject == null) {
			File workingDir = null;
			if (instanceFileOrDir != null) {
				workingDir = instanceFileOrDir.isDirectory() ? instanceFileOrDir : instanceFileOrDir.getParentFile();
			}
			ioObject = BBUtils.createIoObjectInstance(
                    workingDir, ioObjectConfig.getClassName(this.aliasDefinitions), ioObjectConfig.getFileName(this.aliasDefinitions), ioObjectConfig.getArguments(this.aliasDefinitions));
            if (ioObject != null) {
                ioObjects.put(ioObjectConfig.getId(this.aliasDefinitions), ioObject);
            }
        }
		return ioObject;
	}

    private IDataObject findOrCreateDataObject(IoObjectConfig ioObjectConfig) {

        // find or create io object
        IDataObject dataObject = dataObjects.get(ioObjectConfig.getId(this.aliasDefinitions));
        if (dataObject == null) {
            File workingDir = null;
            if (instanceFileOrDir != null) {
                workingDir = instanceFileOrDir.isDirectory() ? instanceFileOrDir : instanceFileOrDir.getParentFile();
            }
            dataObject = BBUtils.createDataObject(
                    workingDir, ioObjectConfig.getClassName(this.aliasDefinitions), ioObjectConfig.getFileName(this.aliasDefinitions), ioObjectConfig.getArguments(this.aliasDefinitions));
            if (dataObject != null) {
                dataObjects.put(ioObjectConfig.getId(this.aliasDefinitions), dataObject);
            }
        }
        return dataObject;
    }

	private void setCheckForPendingComputeActions() {
        if (bbModelConfig.getWrapperConfig().getAdditionalComputeActions().size() > 0) {
            doAdditionalComputeActions = true;
        }
    }

    private void checkForPendingComputeActions() {
        if (doAdditionalComputeActions) {
			System.out.println("Start Additional ComputeActions");
			if (!bbModelConfig.skipModelActionsIfInstanceDirExists()) {
				for (BBAction action : bbModelConfig.getWrapperConfig().getAdditionalComputeActions()) {
					action.run(instanceFileOrDir,this.aliasDefinitions);
				}
			}
			System.out.println("Additional ComputeActions done");
			doAdditionalComputeActions = false;
        }
    }

    private void flushAndClearDataObjects(boolean flushDataObjects) {
        if (flushDataObjects) {
            //flush all IoObjects.
            for (IoObjectInterface ioObjectInterface : ioObjects.values()) {
                ioObjectInterface.finish();
            }

            //flush all DataObjects.
            for (IDataObject dataObject : dataObjects.values()) {
                dataObject.finish();
            }
        }

        //remove all IoObjects and DataObjects
        ioObjects.clear();
        dataObjects.clear();
        bbExchangeItems.clear();
    }

    /**
     * Set startTime and endTime in timeInfoExchangeItems and in aliases.
     *
     * @param computationStart
	 * @param computationEnd
     */
    private void feedComputationSpanToModel(ITime computationStart, ITime computationEnd) {

		String[] startTimeExchangeItemIds = bbModelConfig.getStartTimeExchangeItemIds();
		if (startTimeExchangeItemIds != null) {
			for (int n = 0; n < startTimeExchangeItemIds.length; n++) {
				setStartOrEndTime(computationStart, startTimeExchangeItemIds[n]);
			}
		}

		String[] endTimeExchangeItemIds = bbModelConfig.getEndTimeExchangeItemIds();
		if (endTimeExchangeItemIds != null) {
			for (int n = 0; n < endTimeExchangeItemIds.length; n++) {
				setStartOrEndTime(computationEnd, endTimeExchangeItemIds[n]);
			}
		}
	}

    private ITime getStartOrEndTime(String timeExchangeItemId) {
        ITime startOrEndTime = null;
        if (timeExchangeItemId != null) {
            IPrevExchangeItem timeExchangeItem = getExchangeItem(timeExchangeItemId);
            if (timeExchangeItem.getRole() != IPrevExchangeItem.Role.Input) {
                if (timeExchangeItem.getValueType() == Date.class) {
                    startOrEndTime = new Time((Date)timeExchangeItem.getValues());
                } else if (timeExchangeItem.getValueType() == ITime.class) {
                    startOrEndTime = (ITime) timeExchangeItem.getValues();
                } else if (timeExchangeItem.getValueType() == double.class ||
                        timeExchangeItem.getValueType() == Double.class) {
                    startOrEndTime = new Time((Double)timeExchangeItem.getValues());
                }
            }
        }
        return startOrEndTime;
    }

    private void setStartOrEndTime(ITime startOrEndTime, String timeExchangeItemId) {
        if (timeExchangeItemId != null) {
            IPrevExchangeItem timeExchangeItem = getExchangeItem(timeExchangeItemId);
            if (timeExchangeItem.getRole() != IPrevExchangeItem.Role.Output) {
                if (timeExchangeItem.getValueType() == Date.class) {
                    Date startOrEndTimeAsJavaDate = new Date(Time.mjdToMillies(startOrEndTime.getMJD()));
                    timeExchangeItem.setValues(startOrEndTimeAsJavaDate);
                } else if (timeExchangeItem.getValueType() == ITime.class) {
                    timeExchangeItem.setValues(startOrEndTime);
                } else if (timeExchangeItem.getValueType() == double.class ||
                        timeExchangeItem.getValueType() == Double.class) {
                    timeExchangeItem.setValues(startOrEndTime.getMJD());
                }
            }
        }
    }

    public void initialize(File workingDir, String[] arguments) {
        // no action needed (handled by model factory)
    }

	public AliasDefinitions getAliasDefinitions() {
		return aliasDefinitions;
	}
}
