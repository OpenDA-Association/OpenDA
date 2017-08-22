/* OpenDA v2.4.1 
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

package org.openda.exchange;

import java.util.ArrayList;
import java.util.List;

import org.openda.blackbox.config.BBUtils;
import org.openda.exchange.dataobjects.NetcdfDataObject;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.*;
import org.openda.utils.DimensionIndex;
import org.openda.utils.TreeVector;
import org.openda.utils.Vector;

/**
 * Exchange item representing values for a time series for a single location
 * that are stored in a netcdf file and can be read from there (but not written).
 *
 * @author Arno Kockx
 */
public class NetcdfScalarTimeSeriesExchangeItem implements IExchangeItem { //TODO Please extend TimeSeries in the future or perhaps replace by that class
	//TODO Martin: we cannot use TimeSeries here, because a TimeSeries stores all data in memory. The Netcdf exchangeItems have been added to make
	//it possible to store data in a NetCDF file and only read/write part of the data when it is needed. AK
	//TODO remove hacks for writing per timeStep by using NetcdfScalarExchangeItemWriter. AK	
	//TODO NetcdfScalarTimeSeriesExchangeItem should only be created by NetcdfDataObject itself, either in method NetcdfDataObject.createExchangeItems or in method NetcdfDataObject.addExchangeItem.
	//Outside of NetcdfDataObject, use other exchangeItems. AK

	private final int locationDimensionIndex;
	private int locationIndex;
	private final int realizationDimensionIndex;
	private int realizationIndex;
	private final String id;
	private final Role role;
	/**
	* ITimeInfo that stores all times.
	*/
	private final IQuantityInfo quantityInfo;
	private final NetcdfDataObject netcdfDataObject;

    /**
	 * ITimeInfo that stores only the times for which there are non-missing values.
	 * This is needed because the algorithms cannot cope with missing values.
	 * This object needs to be updated whenever any missing values are changed to
	 * non-missing values or vice versa.
	 */
	private ITimeInfo timesWithNonMissingValuesInfo;

	public NetcdfScalarTimeSeriesExchangeItem(int locationDimensionIndex, int locationIndex,
			String locationId, String parameterId, int realizationDimensionIndex, int realizationIndex, Role role, ITimeInfo allTimesInfo, NetcdfDataObject netcdfDataObject) {
		this.locationDimensionIndex = locationDimensionIndex;
		this.locationIndex = locationIndex;
		//id = "locationId.parameterId"
		this.id = locationId + "." + parameterId;
		this.realizationDimensionIndex = realizationDimensionIndex;
		this.realizationIndex = realizationIndex;
		this.role = role;
		this.quantityInfo = new QuantityInfo(parameterId, "unknown");
		this.netcdfDataObject = netcdfDataObject;

        if (role==Role.InOut || role==Role.Input){
            //determine times for which there are non-missing values for this scalar time series.
            //This is needed because the algorithms cannot cope with missing values.
            double[] allTimes = allTimesInfo.getTimes();
			double[] allValues = getAllValues();
            List<Double> timesWithNonMissingValues = new ArrayList<Double>();
            for (int n = 0; n < allValues.length; n++) {
                if (!Double.isNaN(allValues[n])) {
                    timesWithNonMissingValues.add(allTimes[n]);
                }
            }
            this.timesWithNonMissingValuesInfo = new TimeInfo(BBUtils.unbox(
                    timesWithNonMissingValues.toArray(new Double[timesWithNonMissingValues.size()])));
        } else {
            this.timesWithNonMissingValuesInfo = new TimeInfo(allTimesInfo.getTimes());
        }
	}

    public String getId() {
		return this.id;
	}

	public String getDescription() {
		return null;
	}

	public Role getRole() {
		return this.role;
	}

	/**
	 * Returns all times for which there are non-missing values for this scalar time series.
	 */
	public ITimeInfo getTimeInfo() {
		return this.timesWithNonMissingValuesInfo;
	}

	//TODO this method is only present for backwards compatibility. This method should be removed
	//once all exchange items have been migrated to the new IExchangeItem approach. AK
	@Deprecated
	public double[] getTimes() {
		return getTimeInfo().getTimes();
	}

	//TODO this method is only present for backwards compatibility. This method should be removed
	//once all exchange items have been migrated to the new IExchangeItem approach. AK
	@Deprecated
	public void setTimes(double[] times) {
		throw new UnsupportedOperationException(getClass().getSimpleName() + ": setTimes not implemented.");
	}

	public IQuantityInfo getQuantityInfo() {
		return this.quantityInfo;
	}

	public IGeometryInfo getGeometryInfo() {
		return null;
	}

	public ValueType getValuesType() {
		return ValueType.IVectorType;
	}

	public Class<?> getValueType() {
		return IVector.class;
	}

	/**
	 * Returns all non-missing values for this scalar time series.
	 */
	public Object getValues() {
		double[] values = getValuesAsDoubles();
		IVector vector = new Vector(values);
		return vector;
	}

	/**
	 * Returns all non-missing values for this scalar time series.
	 */
	public double[] getValuesAsDoubles() {
		double[] allValues = getAllValues();
		int numberOfTimesWithNonMissingValues = this.timesWithNonMissingValuesInfo.getTimes().length;
		if (numberOfTimesWithNonMissingValues == allValues.length) {//if all values are non-missing.
			return allValues;
		}

		//filter only the non-missing values.
		double[] values = new double[numberOfTimesWithNonMissingValues];
		int index = 0;
		for (int n = 0; n < allValues.length; n++) {
			if (!Double.isNaN(allValues[n])) {
				values[index++] = allValues[n];
			}
		}
		return values;
	}

	/**
	 * Returns all values for this scalar time series (also missing values).
	 */
	private double[] getAllValues() {
		if (this.realizationDimensionIndex == -1) {
			return this.netcdfDataObject.readDataForExchangeItemForSingleLocation(this, this.locationDimensionIndex, this.locationIndex);
		} else {//if ensemble exchange item.
			return this.netcdfDataObject.readDataForExchangeItemForSingleLocationSingleRealization(this, this.locationDimensionIndex, this.locationIndex, this.realizationDimensionIndex, this.realizationIndex);
		}
	}

	public void axpyOnValues(double alpha, double[] axpyValues) {
		throw new UnsupportedOperationException(getClass().getSimpleName() + ": axpyOnValues not implemented.");
	}

	public void multiplyValues(double[] multiplicationFactors) {
		throw new UnsupportedOperationException(getClass().getSimpleName() + ": multiplyValues not implemented.");
	}

	public void setValues(Object vector) {
		throw new UnsupportedOperationException(getClass().getSimpleName() + ": setValues not implemented.");
	}

	public void setValuesAsDoubles(double[] values) {
		throw new UnsupportedOperationException(getClass().getSimpleName() + ": setValuesAsDoubles not implemented.");
	}

    public void setValuesForSingleTime(double sourceTime, double valueForSourceTime) {
        //get index of sourceTime in this exchangeItem.
        double[] times = this.getTimeInfo().getTimes();
        int sourceTimeIndex = TimeUtils.findMatchingTimeIndex(times, sourceTime, 1e-5);
        if (sourceTimeIndex == -1) {//if this exchangeItem does not contain sourceTime.
            //no data to copy.
            return;
        }

        //write all values for current time.
		this.netcdfDataObject.makeSureFileHasBeenCreated();
		if (this.realizationDimensionIndex != -1) {//if ensemble exchange item.
			this.netcdfDataObject.writeDataForExchangeItemForSingleTimeSingleLocationSingleRealization(this, sourceTimeIndex, realizationDimensionIndex, realizationIndex,
					locationDimensionIndex, locationIndex, new double[]{valueForSourceTime});
		} else {
			this.netcdfDataObject.writeDataForExchangeItemForSingleTimeSingleLocation(this, sourceTimeIndex, locationDimensionIndex, locationIndex, new double[]{valueForSourceTime});
		}
	}

    /**
     * From the given sourceItem copies only the values for the times
     * that are both present in the given sourceItem and in this exchangeItem.
     *
     * Currently this method only works for source exchangeItems that store values for only one time.
     */
    public void copyValuesFromItem(IExchangeItem sourceItem) {
		if (sourceItem.getTimeInfo() == null || sourceItem.getTimeInfo().getTimes() == null) {
				//|| sourceItem.getTimeInfo().getTimes().length != this.getTimeInfo().getTimes().length) {
			throw new RuntimeException(getClass().getSimpleName() + ": cannot copy data from sourceExchangeItem '"
					+ sourceItem.getId() + "' of type " + sourceItem.getClass().getSimpleName()
					+ " because it contains no time info " //or the number of values stored is not identical with the target"
					+ " exchange item: NetcdfScalarTimeSeriesExchangeItem.");
		}

		double[] sourceValues;
		if (sourceItem.getValuesType() == ValueType.IVectorType) {
			sourceValues = ((IVector) sourceItem.getValues()).getValues();
		} else if (sourceItem.getValuesType() == ValueType.IArrayType) {
			sourceValues = ((IArray) sourceItem.getValues()).getValuesAsDoubles();
		} else {
			throw new RuntimeException(getClass().getSimpleName() + ": cannot copy data from sourceExchangeItem '"
					+ sourceItem.getId() + "' of type " + sourceItem.getClass().getSimpleName()
					+ " because its value type is not " + ValueType.IVectorType + " or " + ValueType.IArrayType);
		}

		double[] sourceTimes = sourceItem.getTimeInfo().getTimes();
		double[] targetTimes = this.getTimeInfo().getTimes();
		if (sourceTimes.length != targetTimes.length) {
			//TODO Julius: merge this tolerance with the tolerance that is already present in method setValuesForSingleTime. AK
			double tolerance = 0.1*(targetTimes[1]-targetTimes[0]);
			for (int stIndex=0; stIndex<sourceTimes.length; stIndex++) {
				double sourceTime = sourceTimes[stIndex];
				for (int ttIndex=0; ttIndex<targetTimes.length; ttIndex++) {
					double targetTime = targetTimes[ttIndex];
					if (Math.abs(sourceTime-targetTime) < tolerance) {
						setValuesForSingleTime(sourceTimes[stIndex], sourceValues[stIndex]);
					}
				}
			}
		} else {
			//TODO implement method setValuesAsDoubles and use that here. AK
			for (int iTime = 0; iTime < sourceTimes.length; iTime++) {
				// find matching time and copy value at the matching time; no value is set if no time matches.
				setValuesForSingleTime(sourceTimes[iTime], sourceValues[iTime]);
			}
		}
    }

    public void copyValuesFromItem(IExchangeItem sourceItem, int selectedIndex) {
        if (sourceItem.getTimeInfo() == null || sourceItem.getTimeInfo().getTimes() == null
                || sourceItem.getTimeInfo().getTimes().length != 1) {
            throw new RuntimeException(getClass().getSimpleName() + ": cannot copy data from sourceExchangeItem '"
                    + sourceItem.getId() + "' of type " + sourceItem.getClass().getSimpleName()
                    + " because it contains no time info or stores values for more than one time.");
        }
        if (sourceItem.getValuesType() != ValueType.IVectorType) {
            throw new RuntimeException(getClass().getSimpleName() + ": cannot copy data from sourceExchangeItem '"
                    + sourceItem.getId() + "' of type " + sourceItem.getClass().getSimpleName()
                    + " because its value type is not " + ValueType.IVectorType);
        }

        //get active values for current time.
        double[] valuesForSourceTime = ((IVector) sourceItem.getValues()).getValues();

        double sourceTime = sourceItem.getTimeInfo().getTimes()[0];
        setValuesForSingleTime(sourceTime, valuesForSourceTime[selectedIndex]);
    }

    public void copyValuesFromItem(IExchangeItem sourceItem, IDimensionIndex[] selectionIndices) {
        if (sourceItem.getTimeInfo() == null || sourceItem.getTimeInfo().getTimes() == null
                || sourceItem.getTimeInfo().getTimes().length != 1) {
            throw new RuntimeException(getClass().getSimpleName() + ": cannot copy data from sourceExchangeItem '"
                    + sourceItem.getId() + "' of type " + sourceItem.getClass().getSimpleName()
                    + " because it contains no time info or stores values for more than one time.");
        }
        if (sourceItem.getValuesType() != ValueType.IVectorType) {
            throw new RuntimeException(getClass().getSimpleName() + ": cannot copy data from sourceExchangeItem '"
                    + sourceItem.getId() + "' of type " + sourceItem.getClass().getSimpleName()
                    + " because its value type is not " + ValueType.IVectorType);
        }

        //get active values for current time.
//        double[] valuesForSourceTime = ((IVector) sourceItem.getValues()).getValues();
        double[] valuesForSourceTime = getSelectedValues(sourceItem,selectionIndices);

        double sourceTime = sourceItem.getTimeInfo().getTimes()[0];
//        setValuesForSingleTime(sourceTime, valuesForSourceTime[selectedIndex]);
        setValuesForSingleTime(sourceTime, valuesForSourceTime[0]);
    }

    private double[] getSelectedValues(IExchangeItem sourceItem, IDimensionIndex[] selectionIndices) {

        Class valueType = sourceItem.getValueType();

        Object valuesObject = sourceItem.getValues();

        if (selectionIndices != null) {
            if ( valueType != IVector.class &&
                    valueType != ITreeVector.class &&
                    valueType != double[].class) {
                throw new RuntimeException("Index selection can not be applied on values of type " +
                        valueType);
            }
//            IDimensionIndex[] selectionIndices = vectorConfig.getSelectionIndices();
            int[] dimSizes;
            double[] orgValues;
            if (valueType == double[].class) {
                dimSizes = new int[1];
                orgValues = (double[])valuesObject;
                dimSizes[0] = orgValues.length;
            } else {
                dimSizes = getDimensionSizes((IVector)valuesObject);
                orgValues = ((IVector) valuesObject).getValues();
            }
            if (selectionIndices.length != dimSizes.length) {
                throw new RuntimeException("NetcdfScalarTimeSeriesExchangeItem.getSelectedValues(" + id +
                        "): unexpected #dimensions in IoElement " + sourceItem.getId());
            }
            String treeVectorId = valuesObject instanceof ITreeVector ? ((ITreeVector)valuesObject).getId() + "(...)" : null;
            double[] subValues = DimensionIndex.accessSubArray(id, orgValues, null, selectionIndices, dimSizes);
            IVector subVector = new Vector(subValues);
            if (selectionIndices.length == 1) {
                if (treeVectorId != null) {
                    return new TreeVector(treeVectorId, subVector).getValues();
                } else {
                    return subVector.getValues();
                }
            } else if (selectionIndices.length == 2) {
                return new TreeVector(treeVectorId, subVector,
                        selectionIndices[0].getEnd()-selectionIndices[0].getStart(),
                        selectionIndices[1].getEnd()-selectionIndices[1].getStart()).getValues();
            } else if (selectionIndices.length == 3) {
                return new TreeVector(treeVectorId, subVector,
                        selectionIndices[0].getEnd()-selectionIndices[0].getStart(),
                        selectionIndices[1].getEnd()-selectionIndices[1].getStart(),
                        selectionIndices[2].getEnd()-selectionIndices[2].getStart()).getValues();
            } else {
                throw new RuntimeException("NetcdfScalarTimeSeriesExchangeItem.getSelectedValues(" + id + "): " +
                        "more then 3 dimensions");
            }
        } else {
            return (double[]) valuesObject;
        }
    }

    private static int[] getDimensionSizes(IVector vector) {
        int[] dimSizes;
        if (vector instanceof ITreeVector) {
            IDimensionIndex[] dimensionIndices = ((ITreeVector)vector).getDimensionIndices();
            dimSizes = new int[dimensionIndices.length];
            for (int i = 0; i < dimensionIndices.length; i++) {
                dimSizes[i] = dimensionIndices[i].getSize();
            }
        } else {
            dimSizes = new int[]{vector.getSize()};
        }
        return dimSizes;
    }

	/**
	 * @param locationIndex the index of the location of this exchange item in the stations dimension in the netcdf file.
	 */
	public void setLocationIndex(int locationIndex) {
		this.locationIndex = locationIndex;
	}

	/**
	 * @param realizationIndex the index of the ensemble member of this exchange item in the realization dimension in the netcdf file.
	 */
	public void setRealizationIndex(int realizationIndex) {
		this.realizationIndex = realizationIndex;
	}
}
