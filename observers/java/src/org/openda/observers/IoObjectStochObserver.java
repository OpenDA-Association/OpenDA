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
package org.openda.observers;

import org.openda.blackbox.config.BBUtils;
import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.exchange.NonMissingStochObserverGridTimeSeriesExchangeItem;
import org.openda.exchange.TimeInfo;
import org.openda.interfaces.*;
import org.openda.uncertainties.UncertaintyEngine;
import org.openda.uncertainties.pdfs.NormalDistribution;
import org.openda.uncertainties.pdfs.PDF;
import org.openda.utils.*;
import org.openda.utils.Vector;
import org.openda.utils.geometry.GeometryUtils;

import java.io.File;
import java.io.Serializable;
import java.util.*;

/**
 * Stochastic Observer based on IOobjects
 */
public class IoObjectStochObserver extends Instance implements IStochObserver {

	IoObjectStochObserverConfig ioObjectStochObserverConfig = null;
	private UncertaintyEngine uncertaintyEngine = null;

	private List<IoObjectInterface> ioObjects = new ArrayList<IoObjectInterface>();
	private List<IDataObject> dataObjects = new ArrayList<IDataObject>();
	//TODO change to LinkedHashMap and remove exchangeItemIds list. AK
	//exchangeItems in map can be wrapped within a NonMissingStochObserverExchangeItem or a NonMissingStochObserverGridTimeSeriesExchangeItem.
	private HashMap<String, IPrevExchangeItem> exchangeItems = null;
	private List<String> exchangeItemIds = null;

	private HashMap<IPrevExchangeItem, SelectedTimeIndexRange> selectedTimeIndices = null;
	private boolean timesEqualForAllItems = false;

	//total number of selected observation values, taking into account all exchangeItems, all selected times and all grid cells.
	private int totalSelectedObservationValueCount = 0;
	//number of selected observation values for each exchangeItem.
	private HashMap<String, Integer> selectedObservationValueCounts = null;

	private IoObjectStochObsDescriptions observationDescriptions;
	//only used to cache values, in case method getValues is called multiple times.
	private TreeVector valuesAsTreeVector = null;
	private static final int maxStorageSizeForValuesAsTreeVector = 4096;
	private boolean atLeastOneStdDevIsFactor = false;
    private double beginTimeAsMJD = Double.MIN_VALUE;
    private double endTimeAsMJD = Double.MAX_VALUE;

    // IConfigurable methods

	public void initialize(File workingDir, String[] arguments) {

		// Read the configuration
		File configFile;
		configFile = new File(workingDir, arguments[0]);
		if (!configFile.exists()) {
			throw new RuntimeException("IoObjectStochObserver config file not found: " + configFile.getAbsolutePath());
		}
		IoObjectStochObserverConfigReader configReader = new IoObjectStochObserverConfigReader(configFile);
		ioObjectStochObserverConfig = configReader.getIoObjectStochObserverConfig();

		// Create the uncertainty module.
		// For the time being, we always expect the openda UncertaintyEngine.
		// This may change if we introduce IUncertainty.
		Class<UncertaintyEngine> expectedClassType = UncertaintyEngine.class;
		IoObjectStochObserverConfig.IoStochObsUncertaintyConfig uncertaintyModuleConfig = ioObjectStochObserverConfig.getUncertaintyModuleConfig();
		String className = (uncertaintyModuleConfig.getClassName() != null) ?
				uncertaintyModuleConfig.getClassName() : UncertaintyEngine.class.getName();
		uncertaintyEngine = (UncertaintyEngine) ObjectSupport.createNewInstance(className, expectedClassType);
		uncertaintyEngine.initialize(uncertaintyModuleConfig.getWorkingDir(), uncertaintyModuleConfig.getArguments());
		uncertaintyEngine.startCheckingUncertainItems();

		// Create the ioObjects, and gather the (output) exchange items.
		this.ioObjects.clear();
		this.dataObjects.clear();
		exchangeItems = new HashMap<String, IPrevExchangeItem>();
		exchangeItemIds = new ArrayList<String>();
		boolean timeTypeKnown = false;
		boolean isTimeDependent = false;
		//for each ioObject store the uncertain exchangeItems in this.exchangeItems map and check if these are all of the same type (either all time dependent or all time independent).
		//Important note: this code assumes that all uncertain exchangeItemIds are unique, even if exchangeItems from multiple ioObjects are used.
		for (IoObjectStochObserverConfig.IoStochObsIoObjectConfig ioObjectConfig : ioObjectStochObserverConfig.getIoObjectConfigs()) {
			IoObjectInterface ioObject = BBUtils.createIoObjectInstance(ioObjectConfig.getWorkingDir(), ioObjectConfig.getClassName(),
					ioObjectConfig.getFileName(), ioObjectConfig.getArguments());

            IPrevExchangeItem[] ioExchangeItems;
            if (ioObject != null) {
            	this.ioObjects.add(ioObject);
			    ioExchangeItems = ioObject.getExchangeItems();
            } else {
                IDataObject iDataObject = BBUtils.createDataObject(ioObjectConfig.getWorkingDir(), ioObjectConfig.getClassName(),
					ioObjectConfig.getFileName(), ioObjectConfig.getArguments());
                this.dataObjects.add(iDataObject);
                String[] iDOexchangeItemIDs = iDataObject.getExchangeItemIDs();
                ioExchangeItems = new IPrevExchangeItem[iDOexchangeItemIDs.length];
                for (int i=0; i<iDOexchangeItemIDs.length; i++){
					ioExchangeItems[i] = iDataObject.getDataObjectExchangeItem(iDOexchangeItemIDs[i]);
                }
            }

			//default missingValue is Double.NaN.
            double missingValue = ioObjectConfig.getMissingValue();
			for (IPrevExchangeItem ioExchangeItem : ioExchangeItems) {
				//only items that are configured to be uncertain are used here.
				if (uncertaintyEngine.checkIfItemIsUncertain(ioExchangeItem.getId())) {
					boolean checkTimeDependence;
					if (ioExchangeItem instanceof IGridTimeSeriesExchangeItem) {//if grid.
						checkTimeDependence = true;
					} else {//if not a grid.
						//can only determine whether depends on time if ioExchangeItem has values,
						//in other words for exchangeItems with no values at all, do not perform the check on time-dependence.
						double[] values = ioExchangeItem.getValuesAsDoubles();
						checkTimeDependence = values != null && values.length > 0;
					}

					if (checkTimeDependence) {
					if (ioExchangeItem.getTimes() != null && ioExchangeItem.getTimes().length > 0) {
						//if current item is time dependent.
						if (timeTypeKnown && !isTimeDependent) throw createInconsistentTimeSettingException(configFile);
						isTimeDependent = true;
					} else {
						//if current item is not time dependent.
						if (timeTypeKnown && isTimeDependent) throw createInconsistentTimeSettingException(configFile);
						isTimeDependent = false;
					}
					timeTypeKnown = true;
					}

                    // get rid off missing values.
                    // WAS: if (ioExchangeItem instanceof SwanResults.SwanResult){
					boolean removeMissingValues = ioObjectStochObserverConfig.removeMissingValues();
                    	// NonMissingStochObserverExchangeItem does not work if times are missing
                    	if(ioExchangeItem.getTimes()==null){removeMissingValues=false;}

					if (removeMissingValues) {
						IExchangeItem nonMissingExchangeItem;
						if (ioExchangeItem instanceof IGridTimeSeriesExchangeItem) {
							nonMissingExchangeItem = new NonMissingStochObserverGridTimeSeriesExchangeItem((IGridTimeSeriesExchangeItem) ioExchangeItem);
						} else {
							nonMissingExchangeItem = new NonMissingStochObserverExchangeItem(ioExchangeItem, missingValue);
						}
						exchangeItemIds.add(nonMissingExchangeItem.getId());
						exchangeItems.put(nonMissingExchangeItem.getId(), nonMissingExchangeItem);
					} else {
						exchangeItemIds.add(ioExchangeItem.getId());
						exchangeItems.put(ioExchangeItem.getId(), ioExchangeItem);
					}
				}
			}
		}
		if (exchangeItems.isEmpty()) {
				throw new RuntimeException(
					"The stoch observer is empty. Did you specify the right uncertain items? Stoch obs config file: " +
							configFile.getAbsolutePath());
		}

		if (isTimeDependent) {
			//for each uncertain exchangeItem store the full timeIndex range in selectedTimeIndices map.
			selectedTimeIndices = new HashMap<IPrevExchangeItem, SelectedTimeIndexRange>();
			SelectedTimeIndexRange lastSelectedIndices = null;
			timesEqualForAllItems = true;
			double[] timeSelected = null;
			double[] timeLastSelected = null;
			for (String exchangeItemId : exchangeItemIds) {
				IPrevExchangeItem exchangeItem = exchangeItems.get(exchangeItemId);
				timeLastSelected = timeSelected;
				timeSelected = exchangeItem.getTimes();
				boolean sameTimesAsPreviousExchangeItem = true;
				if (lastSelectedIndices != null) {
					//lastSelectedIndices.getStart() is always 0 here.
					sameTimesAsPreviousExchangeItem = exchangeItem.getTimes().length == lastSelectedIndices.getEnd();
					if (sameTimesAsPreviousExchangeItem){
						for (int iTime=0; iTime<timeSelected.length; iTime++){
							if (timeSelected[iTime]!=timeLastSelected[iTime]){
								sameTimesAsPreviousExchangeItem = false;
								break;
							}
						}
					}
				}
				timesEqualForAllItems &= sameTimesAsPreviousExchangeItem;
				SelectedTimeIndexRange selectedIndices = (sameTimesAsPreviousExchangeItem && lastSelectedIndices != null) ? lastSelectedIndices : new SelectedTimeIndexRange(0, exchangeItem.getTimes().length);
				selectedTimeIndices.put(exchangeItem, selectedIndices);
				lastSelectedIndices = selectedIndices;
			}
		}

		initializeCounts();
		initializeAtLeastOneStdDevIsFactor();
		uncertaintyEngine.endCheckingUncertainItems();
		observationDescriptions = new IoObjectStochObsDescriptions(exchangeItemIds, exchangeItems, null, this);
	}

	private RuntimeException createInconsistentTimeSettingException(File configFile) {
		return new RuntimeException(
				"The IoObject(s) contain(s) a mixture of " +
						" time dependent and time independent exchange items. IoObjectStochObserver config file: " +
						configFile.getAbsolutePath());
	}

	/**
	 * The only purpose of this method is to initialize this.totalSelectedObservationValueCount and this.selectedObservationValueCounts.
	 */
	private void initializeCounts() {
		//assume 1 value per time stamp per exchange item per grid cell.
		//If item is not time dependent, then that is the same as a time series with only one time stamp.
		//If item is not a grid, then that is the same as a grid with only one cell.

		totalSelectedObservationValueCount = 0;
		selectedObservationValueCounts = new HashMap<String, Integer>();
		for (String exchangeItemId : exchangeItemIds) {
			IPrevExchangeItem exchangeItem = exchangeItems.get(exchangeItemId);

			int exchangeItemSelectedValueCount = 0;
			if (selectedTimeIndices != null && selectedTimeIndices.get(exchangeItem) != null) {//if time dependent.
				SelectedTimeIndexRange selectedIndices = selectedTimeIndices.get(exchangeItem);
				for (int timeIndex = selectedIndices.getStart(); timeIndex < selectedIndices.getEnd(); timeIndex++) {
					int gridCellCount = GeometryUtils.getGridCellCount(exchangeItem, timeIndex);
					exchangeItemSelectedValueCount += gridCellCount;
				}
			} else {//if not time dependent.
				int gridCellCount = GeometryUtils.getGridCellCount(exchangeItem, -1);
				exchangeItemSelectedValueCount += gridCellCount;
			}

			selectedObservationValueCounts.put(exchangeItem.getId(), exchangeItemSelectedValueCount);
			totalSelectedObservationValueCount += exchangeItemSelectedValueCount;
		}
	}

	// IStochObserver methods

	public IStochObserver createSelection(String selection) {
		throw new UnsupportedOperationException("org.openda.observers.IoObjectStochObserver.createSelection(String selection): Not implemented yet.");
	}

	/**
	 * @param selectedTime can be a single time or a timeSpan. StartTime and endTime are both inclusive.
	 * @return copy of this stochObserver with only data for the selected time.
	 */
	public IStochObserver createSelection(ITime selectedTime) {

		IoObjectStochObserver child = this.createEmptyChild();

		if (selectedTimeIndices != null) {
			// Time dependent observation values. For each exchange item, select the time indices

			// Time stamp or interval
			boolean isSpan = selectedTime.isSpan();
			if (isSpan) {
				child.beginTimeAsMJD = selectedTime.getBeginTime().getMJD();
				child.endTimeAsMJD = selectedTime.getEndTime().getMJD();
			} else {
				child.beginTimeAsMJD = child.endTimeAsMJD = selectedTime.getMJD();
			}

			child.selectedTimeIndices = new HashMap<IPrevExchangeItem, SelectedTimeIndexRange>();

			//for each exchangeItem select the times within the given timeSpan and put these in child.
			SelectedTimeIndexRange lastSelectedIndices = null;
			child.timesEqualForAllItems = true;
			for (String exchangeItemId : exchangeItemIds) {
				IPrevExchangeItem exchangeItem = exchangeItems.get(exchangeItemId);

				double[] times = exchangeItem.getTimes();
                boolean isObservationAvailableInThisSpan = false;
                for (int iTime=0; iTime<times.length; iTime++){
                    if (times[iTime] >= child.beginTimeAsMJD && times[iTime] <= child.endTimeAsMJD){
                        isObservationAvailableInThisSpan = true;
                        break;
                    }
                }
                if (isObservationAvailableInThisSpan){
                    // skip times before the begin time
                    // TODO: let IPrevExchangeItem.getTimes() return List<Time> instead of double[]
                    double compareEpsilon = 1e-6;
                    int startIndex = Integer.MAX_VALUE;
                    int i = 0;
					//startIndex is inclusive.
                    while (startIndex == Integer.MAX_VALUE && i < times.length) {
                        if (isSpan) {//if time span selected.
                            if ((times[i] > child.beginTimeAsMJD + compareEpsilon) && (times[i] < (child.endTimeAsMJD + compareEpsilon)))startIndex = i;
                        } else {//if single time selected.
                            if ((times[i] > (child.beginTimeAsMJD - compareEpsilon)) && (times[i] < (child.endTimeAsMJD + compareEpsilon))) startIndex = i;
                        }
                        i++;
                    }

                    if (startIndex == Integer.MAX_VALUE) {
                        // this exchange item has no time stamps for the selected times()
                        continue;
                    }

                    // truncate after the end time
					//endIndex is exclusive.
                    int endIndex = startIndex + 1;
                    if (isSpan) {
                        while (endIndex < times.length && !(times[endIndex] > child.endTimeAsMJD))
                            endIndex++;
                    }
                    // store selection indices, reuse if possible
                    boolean sameTimesAsPreviousExchangeItem = true;
                    if (lastSelectedIndices != null) {
						sameTimesAsPreviousExchangeItem = startIndex == lastSelectedIndices.getStart() && endIndex == lastSelectedIndices.getEnd();
                    }
					//TODO this only checks if the time indices are the same, also need to check whether the times itself are the same, in order to initialize timesEqualForAllItems correctly. AK
//                    child.timesEqualForAllItems &= sameTimesAsPreviousExchangeItem;
                    child.timesEqualForAllItems = false;
					//startIndex is inclusive, endIndex is exclusive.
                    SelectedTimeIndexRange selectedIndices = (sameTimesAsPreviousExchangeItem && lastSelectedIndices != null) ? lastSelectedIndices : new SelectedTimeIndexRange(startIndex, endIndex);
                    child.selectedTimeIndices.put(exchangeItem, selectedIndices);
                    child.exchangeItemIds.add(exchangeItem.getId());
                    child.exchangeItems.put(exchangeItem.getId(), exchangeItem);
                    lastSelectedIndices = selectedIndices;
                }
			}
		} else {
			// time independent values, add all items to child
			child.exchangeItemIds = this.exchangeItemIds;
			child.exchangeItems = this.exchangeItems;
		}
		child.initializeCounts();
		child.initializeAtLeastOneStdDevIsFactor();
		child.observationDescriptions = new IoObjectStochObsDescriptions(child.exchangeItemIds, child.exchangeItems, child.selectedTimeIndices, child);
		return child;
	}

	public IStochObserver createSelection(int[] selector){
		throw new UnsupportedOperationException(this.getClass().getName()
				+ ".createSelection() not implemented");
	}

	public IStochObserver createSelection(Type observationType) {

		IoObjectStochObserver child = this.createEmptyChild();
		if (observationType == Type.Assimilation) {
			List<String> selectedObsIds = this.ioObjectStochObserverConfig.getAssimilationObsIds();
			fillObservationTypeChildWithSelectedObservations(child, selectedObsIds);
		} else {
			fillObservationTypeChildWithSelectedObservations(child, this.ioObjectStochObserverConfig.getValidationObsIds());
		}
		child.initializeCounts();
		child.initializeAtLeastOneStdDevIsFactor();
		//TODO not initialized here: child.selectedTimeIndices, child.timesEqualForAllItems, child.beginTimeAsMJD, child.endTimeAsMJD. AK
		child.observationDescriptions = new IoObjectStochObsDescriptions(child.exchangeItemIds, child.exchangeItems, child.selectedTimeIndices, child);
		return child;
	}

    public ISelector createSelector(Type observationType) {
        throw new UnsupportedOperationException("org.openda.observers.IoObjectStochObserver.createSelector(): Not implemented yet.");
    }

    private IoObjectStochObserver createEmptyChild() {

		IoObjectStochObserver child = new IoObjectStochObserver();

		child.ioObjectStochObserverConfig = this.ioObjectStochObserverConfig;
		child.uncertaintyEngine = this.uncertaintyEngine;
		uncertaintyEngine.increaseTimeStepSeed();

		child.exchangeItemIds = new ArrayList<String>();
		child.exchangeItems = new HashMap<String,IPrevExchangeItem>();

		child.selectedTimeIndices = this.selectedTimeIndices;
		child.timesEqualForAllItems = this.timesEqualForAllItems;

		return child;
	}

	private void fillObservationTypeChildWithSelectedObservations(IoObjectStochObserver child, List<String> selectedObsIds) {
		if (selectedObsIds != null) {
			for (String selectedObsId : selectedObsIds) {
				child.exchangeItemIds.add(selectedObsId);
				child.exchangeItems.put(selectedObsId, this.exchangeItems.get(selectedObsId));
			}
		} else {
			child.exchangeItemIds = this.exchangeItemIds;
			child.exchangeItems = this.exchangeItems;
		}
	}

	public int getCount() {
		return totalSelectedObservationValueCount;
	}

	public IVector getRealizations() {
		return uncertaintyEngine.getRealization(getValues());
	}

	public IVector getExpectations() {
		// For now, we assume that all PDF's are normal, so the expectations are the values
		// TODO: use expectations of other PFD
		return getValues();
	}

	public double evaluatePDF(IVector values) {
        IVector mean = this.getExpectations();
        IVector std = this.getStandardDeviations();
        IStochVector sv = new StochVector(mean,std);
        return sv.evaluatePdf(values);
	}

	public IVector evaluateMarginalPDFs(IVector values) {
		throw new UnsupportedOperationException("org.openda.observers.IoObjectStochObserver.evaluateMarginalPDFs(): Not implemented yet.");
	}

	public ISqrtCovariance getSqrtCovariance() {
		IVector mean = this.getExpectations();
		IVector std = this.getStandardDeviations();
		IStochVector stochVector = new StochVector(mean, std);
		return stochVector.getSqrtCovariance();
	}

	public IVector getStandardDeviations() {
		double[] standardDeviations;

		initializeAtLeastOneStdDevIsFactor();  //added (VORtech)
		if (atLeastOneStdDevIsFactor) {
			standardDeviations = uncertaintyEngine.getStandardDeviations(exchangeItemIds, selectedObservationValueCounts, totalSelectedObservationValueCount, getValues().getValues());
		} else {
			standardDeviations = uncertaintyEngine.getStandardDeviations(exchangeItemIds, selectedObservationValueCounts, totalSelectedObservationValueCount);
		}
		return new Vector(standardDeviations);
	}

	/**
	 * Returns all unique times, i.e. each time that is present in one or more exchangeItems.
	 */
	public ITime[] getTimes() {

		if (selectedTimeIndices == null) {
			return null;
		}

		if (timesEqualForAllItems) {
			// Time dependent, but the selected exchange items have the same selected times.
			if (selectedTimeIndices.size() > 0) {
				IPrevExchangeItem firstEI = exchangeItems.get(exchangeItemIds.get(0));
				//firstEI contains all times.
				double[] timesAsMJD = firstEI.getTimes();
				ArrayList<ITime> selectedTimes = new ArrayList<ITime>();
				for (int i = 0; i < timesAsMJD.length; i++) {
					if (timesAsMJD[i] >= beginTimeAsMJD && timesAsMJD[i] <= endTimeAsMJD) {
						selectedTimes.add(new Time(timesAsMJD[i]));
					}
				}
				//this code assumes that the times in exchangeItem firstEI are unique and are sorted chronologically.
				return selectedTimes.toArray(new ITime[selectedTimes.size()]);
			}
			return null;

		} else {
            // Time dependent, differences in #selected times between the exchange items
			//TODO Julius: here use a Set instead of an ArrayList. AK
            ArrayList<Double> timesArrDbl = new ArrayList<Double>();
			//for each exchangeItem add all times to timesArrDbl list.
            for (int iExch = 0; iExch<exchangeItemIds.size(); iExch++) {
                IPrevExchangeItem exchangeItem = exchangeItems.get(exchangeItemIds.get(iExch));
                double[] thisTimes = exchangeItem.getTimes();
                for (int iTime=0; iTime<thisTimes.length; iTime++){
                    if (thisTimes[iTime]>=beginTimeAsMJD && thisTimes[iTime]<=endTimeAsMJD){
                        if (timesArrDbl.size()==0){
                            timesArrDbl.add(thisTimes[iTime]);
                        } else if (!(timesArrDbl.contains((Double)thisTimes[iTime]))){
                            timesArrDbl.add(thisTimes[iTime]);
                        }
                    }
                }
            }
            Collections.sort(timesArrDbl);
            ITime[] times = new ITime[timesArrDbl.size()];
            for (int i = 0; i < times.length; i++) {
                times[i] = new Time(timesArrDbl.get(i));
            }
            return times;
        }
	}

	public void free() {
		for (IoObjectInterface ioObject : this.ioObjects) {
			ioObject.finish();
		}
		for (IDataObject dataObject : this.dataObjects) {
			dataObject.finish();
		}
	}

	public IObservationDescriptions getObservationDescriptions() {
		return observationDescriptions;
	}

	/**
	 * Returns a treeVector that contains vectors with all selected values for all exchangeItems in this stochObserver.
	 * Note that the time information of the values is lost in this step.
	 */
	public ITreeVector getValues() {
		TreeVector treeVector;
		if (valuesAsTreeVector == null) {
			treeVector = new TreeVector("ObsValues");
			for (String exchangeItemId : exchangeItemIds) {
				IPrevExchangeItem exchangeItem = exchangeItems.get(exchangeItemId);

				Vector childVector;
				if (selectedTimeIndices == null) {//if not time dependent.
					childVector = new Vector(exchangeItem.getValuesAsDoubles());

				} else {//if time dependent.
					SelectedTimeIndexRange selectedIndices = selectedTimeIndices.get(exchangeItem);

					if (exchangeItem instanceof IGridTimeSeriesExchangeItem) {//if grid.
						//this code assumes that this method is only called after the time selection has been made and that only one time step is selected.
						if (selectedIndices.getSize() != 1) {
							throw new UnsupportedOperationException(getClass().getSimpleName()
									+ ": getValues only works for exchangeItems of type IGridTimeSeriesExchangeItem if only one time is selected.");
						}
						double[] values = ((IGridTimeSeriesExchangeItem) exchangeItem).getValuesAsDoublesForSingleTimeIndex(selectedIndices.getStart());
						childVector = new Vector(values);

					} else {//if not a grid.
						childVector = new Vector(selectedIndices.getSize());
						double[] values = exchangeItem.getValuesAsDoubles();
						int indexInVector = 0;
						for (int i = selectedIndices.getStart(); i < selectedIndices.getEnd(); i++) {
							childVector.setValue(indexInVector++, values[i]);
						}
					}
				}

				treeVector.addChild(new TreeVector(exchangeItem.getId(), childVector));
			}
			//do not cache values if too large.
			if (treeVector.getSize() <= maxStorageSizeForValuesAsTreeVector) {
				valuesAsTreeVector = treeVector;
			}
		} else {
			treeVector = valuesAsTreeVector.clone();
		}
		return treeVector;
	}

	// set flag if at least one StdDev is a factor of the actual value
	private void initializeAtLeastOneStdDevIsFactor() {
		for (String uncertaintyId : exchangeItemIds) {
			PDF pdf = uncertaintyEngine.getPdf(uncertaintyId);
			if (pdf instanceof NormalDistribution) {
				atLeastOneStdDevIsFactor |= ((NormalDistribution) pdf).isStdFactor();
			}
		}
	}

	/**
	 * Stores begin and end index of the times that are selected for a certain exchangeItem.
	 */
	private class SelectedTimeIndexRange implements Serializable {
		private int start;
		private int end;

		/**
		 * @param start index (inclusive).
		 * @param end index (exclusive).
		 */
		public SelectedTimeIndexRange(int start, int end) {
			this.start = start;
			this.end = end;
		}

		public int getStart() {
			return start;
		}

		public int getEnd() {
			return end;
		}

		public int getSize() {
			return end - start;
		}
	}

	private class IoObjectStochObsDescriptions implements IObservationDescriptions, Serializable {

		private final List<String> exchangeItemIds;
		//exchangeItems in map can be wrapped within an IoObjectStochObsTimeSelectionExchangeItem.
		private final HashMap<String, IPrevExchangeItem> exchangeItems;
		private final IoObjectStochObserver stochObserver;
		private final HashMap<IPrevExchangeItem, SelectedTimeIndexRange> selectedTimeIndices;

		private IoObjectStochObsDescriptions(List<String> exchangeItemIds,
											HashMap<String, IPrevExchangeItem> exchangeItems,
											HashMap<IPrevExchangeItem, SelectedTimeIndexRange> selectionIndices,
											IoObjectStochObserver stochObserver) {
			this.stochObserver = stochObserver;
			this.selectedTimeIndices = selectionIndices;

			if (selectionIndices == null) {//if time independent.
				if (selectionIndices != null && selectionIndices.size() > 0) {
					throw new RuntimeException(
							"org.openda.observers.IoObjectStochObsDescriptions.IoObjectStochObsDescriptions() " +
							": selection times cannot be specified for time independent exchange items.");
				}
				this.exchangeItemIds = exchangeItemIds;
				this.exchangeItems = exchangeItems;
				return;
			}

			//if time dependent.
			if (selectionIndices == null || selectionIndices.size() == 0) {
				// no selection on times, take all exchange items
				this.exchangeItemIds = exchangeItemIds;
				this.exchangeItems = exchangeItems;
			} else {//if selection on times.
				// create time selection exchange item around exchange item
				SelectedTimeIndexRange sharedSelectedIndices = null;
				double[] sharedTimes = null;
				this.exchangeItemIds = new ArrayList<String>();
				this.exchangeItems = new HashMap<String, IPrevExchangeItem>();
				//loop over selected exchangeItems.
				for (String exchangeItemId : exchangeItemIds) {
					IPrevExchangeItem exchangeItem = exchangeItems.get(exchangeItemId);
					SelectedTimeIndexRange selectedIndices = selectionIndices.get(exchangeItem);
					double[] selectedTimes;
					if (timesEqualForAllItems) {
						if (sharedSelectedIndices == null) {
							sharedSelectedIndices = selectedIndices;
							sharedTimes = getSelectedTimes(exchangeItem.getTimes(), selectedIndices);
						}
						selectedIndices = sharedSelectedIndices;
						selectedTimes = sharedTimes;
					} else {
						selectedTimes = getSelectedTimes(exchangeItem.getTimes(), selectedIndices);
					}
					IoObjectStochObsTimeSelectionExchangeItem timeSelectionExchangeItem =
							new IoObjectStochObsTimeSelectionExchangeItem(exchangeItem, selectedIndices, selectedTimes);
					this.exchangeItemIds.add(timeSelectionExchangeItem.getId());
					this.exchangeItems.put(timeSelectionExchangeItem.getId(), timeSelectionExchangeItem);
				}
			}
		}

		private double[] getSelectedTimes(double[] times, SelectedTimeIndexRange selectedIndices) {
			double[] selection = new double[selectedIndices.getSize()];
			int index = 0;
			for (int i = selectedIndices.getStart(); i < selectedIndices.getEnd(); i++) {
				selection[index++] = times[i];
			}
			return selection;
		}

		//Note: this should return the exchangeItems in the same order as the ids in this.exchangeItemIds.
		//This is important for the code in method BBStochModelInstance.getObservedModelValuesForGrid.
		public List<IPrevExchangeItem> getExchangeItems() {
			List<IPrevExchangeItem> exchangeItems = new ArrayList<IPrevExchangeItem>();
			for (String exchangeItemId : exchangeItemIds) {
				exchangeItems.add(this.exchangeItems.get(exchangeItemId));
			}
			return exchangeItems;
		}

		//Note: this should return the properties in the same order as the ids in this.exchangeItemIds.
		//This is important for the code in method BBStochModelInstance.getObservedModelValuesForGrid.
		public IVector getValueProperties(String key) {

			IVector properties;
			if (keyIsXcoord(key) || keyIsYcoord(key)) {
				List<Double> coordinatesList = new ArrayList<Double>();
				boolean isY = keyIsYcoord(key);
				//if more than one exchangeItem, then add coordinates for each exchangeItem in sequence.
				for (String exchangeItemId : exchangeItemIds) {
					IPrevExchangeItem prevExchangeItem = exchangeItems.get(exchangeItemId);
					// x/y currently only known in case of Swan.
					// TODO: add X/Y to IPrevExchangeItem
					// TODO MVL fix properly through IGeometry
					//SwanResults.SwanResult.SwanResultLocation swanResultLocation = ((SwanResults.SwanResult) exchangeItem).getLocation();
					//properties.setValue(index++, isY ? swanResultLocation.getY() : swanResultLocation.getX());
					if(exchangeItemId.indexOf("@")>=0){
						int indAt=exchangeItemId.indexOf("@");
						int indComma=exchangeItemId.lastIndexOf(",");
						double pos;
						if(isY){ //Y
						   pos=Double.parseDouble(exchangeItemId.substring(indComma+1));
						}else{ //X
						   pos=Double.parseDouble(exchangeItemId.substring(indAt+1, indComma));							
						}
						coordinatesList.add(pos);
					}else{
						//get geometryInfo.
						IGeometryInfo geometryInfo;
						if (!(prevExchangeItem instanceof IExchangeItem)) {
							//IPrevExchangeItem has no geometryInfo, so can never be a grid.
							geometryInfo = null;
						} else {
							geometryInfo = ((IExchangeItem) prevExchangeItem).getGeometryInfo();
						}

						//get geometry.
						if (GeometryUtils.isScalar(geometryInfo)) {
							throw new RuntimeException("No coordinates available for scalar exchangeItem " + exchangeItemId);
						}

						//get coordinates.
						IVector coordinates;
						if (isY) {//if y.
							//this code assumes that the coordinates are stored in the same order as the values in the exchangeItem.
							//need one coordinate for each grid cell.
							coordinates = GeometryUtils.getYCoordinates(geometryInfo);
						} else {//if x.
							//this code assumes that the coordinates are stored in the same order as the values in the exchangeItem.
							//need one coordinate for each grid cell.
							coordinates = GeometryUtils.getXCoordinates(geometryInfo);
						}
						//if more than one exchangeItem, then add coordinates for each exchangeItem in sequence.
						for (int n = 0; n < coordinates.getSize(); n++) {
							coordinatesList.add(coordinates.getValue(n));
						}
					}
				}
				properties = new Vector(BBUtils.unbox(coordinatesList.toArray(new Double[coordinatesList.size()])));

			} else if (keyIsTime(key)) {
				List<Double> timesList = new ArrayList<Double>();
				if (this.selectedTimeIndices == null) {
					// Time independent. Return empty list.

				} else {//if time dependent.
					//if more than one exchangeItem, then add times for each exchangeItem in sequence.
					for (String exchangeItemId : exchangeItemIds) {
						IPrevExchangeItem exchangeItem = exchangeItems.get(exchangeItemId);
						//in case of IoObjectStochObsTimeSelectionExchangeItem then selectedTimeIndices contains the wrapped exchangeItem,
						//so need to use the wrapped exchangeItem to lookup the selected time indices in selectedTimeIndices.
						if (exchangeItem instanceof IoObjectStochObsTimeSelectionExchangeItem) {
							//unwrap exchangeItem.
							exchangeItem = ((IoObjectStochObsTimeSelectionExchangeItem)exchangeItem).exchangeItem;
						}
						SelectedTimeIndexRange selectedIndices = this.selectedTimeIndices.get(exchangeItem);
						//if more than one exchangeItem, then add times for each exchangeItem in sequence.
						for (int i = selectedIndices.getStart(); i < selectedIndices.getEnd(); i++) {
							timesList.add(exchangeItem.getTimes()[i]);
						}
					}
				}
				properties = new Vector(BBUtils.unbox(timesList.toArray(new Double[timesList.size()])));

			} else if (keyIsStdDev(key)) {
				properties = getStandardDeviations();
			} else if (keyIsValues(key)) {
				properties = getStandardDeviations();
			} else {
				throw new RuntimeException("IoObjectStochObserver.getValueProperties(): unhandled property key: " + key);
			}

			return properties;
		}

		public String[] getStringProperties(String key) {
			if (key.equalsIgnoreCase("id")) {
				return exchangeItemIds.toArray(new String[exchangeItemIds.size()]);
			}
			throw new RuntimeException("IoObjectStochObserver.getStringProperties(): unknown property key: " + key);
		}

		public String[] getPropertyKeys() {
			throw new UnsupportedOperationException("org.openda.observers.IoObjectStochObserver.IoObjectStochObsDescriptions.getPropertyKeys(): Not implemented yet.");
		}

		public int getPropertyCount() {
			throw new UnsupportedOperationException("org.openda.observers.IoObjectStochObserver.IoObjectStochObsDescriptions.getPropertyCount(): Not implemented yet.");
		}

		public int getObservationCount() {
			return this.stochObserver.totalSelectedObservationValueCount;
		}

		public ITime[] getTimes() {
			return stochObserver.getTimes();
		}

		private boolean keyIsXcoord(String key) {
			return key.equalsIgnoreCase("x") || key.equalsIgnoreCase("xp") ||
					key.equalsIgnoreCase("xCoord") || key.equalsIgnoreCase("x-Coord");
		}

		private boolean keyIsYcoord(String key) {
			return key.equalsIgnoreCase("y") || key.equalsIgnoreCase("yp") ||
					key.equalsIgnoreCase("yCoord") || key.equalsIgnoreCase("y-Coord");
		}

		private boolean keyIsTime(String key) {
			return key.equalsIgnoreCase("t") || key.equalsIgnoreCase("time");
		}

		private boolean keyIsStdDev(String key) {
			return key.equalsIgnoreCase("stdDev") || key.equalsIgnoreCase("std") ||
					key.equalsIgnoreCase("stDev") || key.equalsIgnoreCase("standardDev") ||
					key.equalsIgnoreCase("standardDeviation");
		}

		private boolean keyIsValues(String key) {
			return key.equalsIgnoreCase("val") || key.equalsIgnoreCase("value") || key.equalsIgnoreCase("values");
		}
	}

	/**
	 * ExchangeItem that selects certain times from another exchangeItem.
	 */
	private class IoObjectStochObsTimeSelectionExchangeItem implements IExchangeItem, Serializable {

		private IPrevExchangeItem exchangeItem;
		private SelectedTimeIndexRange selectedIndices;
		private double[] times;

		private IoObjectStochObsTimeSelectionExchangeItem(IPrevExchangeItem exchangeItem, SelectedTimeIndexRange selectedIndices, double[] times) {
			this.exchangeItem = exchangeItem;
			this.selectedIndices = selectedIndices;
			this.times = times;
		}

		public String getId() {
			return exchangeItem.getId();
		}

		public String getDescription() {
			return exchangeItem.getDescription();
		}

		public Class getValueType() {
			return double[].class;
		}

		public ValueType getValuesType() {
			return ValueType.doublesType;
		}

		public Role getRole() {
			return exchangeItem.getRole();
		}

		public Object getValues() {
			return getValuesAsDoubles();
		}

		//get only the values for the selected times.
		public double[] getValuesAsDoubles() {
			if (this.exchangeItem instanceof IGridTimeSeriesExchangeItem) {
				if (this.selectedIndices.getSize() != 1) {
					throw new UnsupportedOperationException(getClass().getSimpleName()
							+ ": getValuesAsDoubles only works for exchangeItems of type IGridTimeSeriesExchangeItem if only one time is selected.");
				}
				return ((IGridTimeSeriesExchangeItem) this.exchangeItem).getValuesAsDoublesForSingleTimeIndex(this.selectedIndices.getStart());
			}

			double[] allValues = exchangeItem.getValuesAsDoubles();
			double[] values = new double[selectedIndices.getSize()];
			int index = 0;
			for (int i = selectedIndices.getStart(); i < selectedIndices.getEnd(); i++) {
				values[index++] = allValues[i];
			}
			return values;
		}

		//axpy only the values for the selected times.
		public void axpyOnValues(double alpha, double[] axpyValues) {
			if (this.exchangeItem instanceof IGridTimeSeriesExchangeItem) {
				if (this.selectedIndices.getSize() != 1) {
					throw new UnsupportedOperationException(getClass().getSimpleName()
							+ ": axpyOnValues only works for exchangeItems of type IGridTimeSeriesExchangeItem if only one time is selected.");
				}
				((IGridTimeSeriesExchangeItem) this.exchangeItem).axpyOnValuesForSingleTimeIndex(this.selectedIndices.getStart(), alpha, axpyValues);
			}

			double[] allValues = exchangeItem.getValuesAsDoubles();
			int index = 0;
			for (int i = selectedIndices.getStart(); i < selectedIndices.getEnd(); i++) {
				allValues[i] += alpha * axpyValues[index++];
			}
			exchangeItem.setValuesAsDoubles(allValues);
		}

		//mutiply only the values for the selected times.
		public void multiplyValues(double[] multiplicationFactors) {
			if (this.exchangeItem instanceof IGridTimeSeriesExchangeItem) {
				if (this.selectedIndices.getSize() != 1) {
					throw new UnsupportedOperationException(getClass().getSimpleName()
							+ ": multiplyValues only works for exchangeItems of type IGridTimeSeriesExchangeItem if only one time is selected.");
				}
				((IGridTimeSeriesExchangeItem) this.exchangeItem).multiplyValuesForSingleTimeIndex(this.selectedIndices.getStart(), multiplicationFactors);
			}

			double[] allValues = exchangeItem.getValuesAsDoubles();
			int index = 0;
			for (int i = selectedIndices.getStart(); i < selectedIndices.getEnd(); i++) {
				allValues[i] *= multiplicationFactors[index++];
			}
			exchangeItem.setValuesAsDoubles(allValues);
		}

		public void setValues(Object values) {
			if (!(values instanceof double[])) {
				throw new RuntimeException(this.getClass().getName() +
						"setValues: unknown object type: " + values.getClass().getName());
			}
			setValuesAsDoubles((double[]) values);
		}

		//set only the values for the selected times.
		public void setValuesAsDoubles(double[] values) {
			if (this.exchangeItem instanceof IGridTimeSeriesExchangeItem) {
				if (this.selectedIndices.getSize() != 1) {
					throw new UnsupportedOperationException(getClass().getSimpleName()
							+ ": multiplyValues only works for exchangeItems of type IGridTimeSeriesExchangeItem if only one time is selected.");
				}
				((IGridTimeSeriesExchangeItem) this.exchangeItem).setValuesAsDoublesForSingleTimeIndex(this.selectedIndices.getStart(), values);
			}

			//overwrite only the values for the selected times.
			double[] allValues = exchangeItem.getValuesAsDoubles();
			int index = 0;
			for (int i = selectedIndices.getStart(); i < selectedIndices.getEnd(); i++) {
				allValues[i] = values[index++];
			}
			exchangeItem.setValuesAsDoubles(allValues);
		}

		//get the geometryInfo for the selected times.
		public IGeometryInfo getGeometryInfo() {
			if (this.exchangeItem instanceof IGridTimeSeriesExchangeItem) {
				if (this.selectedIndices.getSize() != 1) {
					throw new UnsupportedOperationException(getClass().getSimpleName() + ": getGeometryInfo only works for exchangeItems of type IGridTimeSeriesExchangeItem if only one time is selected.");
				}
				return ((IGridTimeSeriesExchangeItem) this.exchangeItem).getGeometryInfoForSingleTimeIndex(this.selectedIndices.getStart());
			}

			//TODO all exchangeItems should implement IExchangeItem. AK
			if (exchangeItem instanceof IExchangeItem) {
				return ((IExchangeItem) exchangeItem).getGeometryInfo();
			}
			return null;
		}

		//return only the selected times.
		public double[] getTimes() {
			return times;
		}

		public ITimeInfo getTimeInfo() {
			return new TimeInfo(getTimes());
		}

		public void setTimes(double[] times) {
			throw new UnsupportedOperationException(getClass().getSimpleName() + ".setTimes(): Setting times not allowed.");
		}

		public IQuantityInfo getQuantityInfo() {
			if (exchangeItem instanceof IExchangeItem) {
				return ((IExchangeItem) exchangeItem).getQuantityInfo();
			}
			return null;
		}

		public void copyValuesFromItem(IExchangeItem sourceItem) {
			throw new UnsupportedOperationException(getClass().getSimpleName() + ".copyValuesFromItem() not implemented.");
		}
	}
}
