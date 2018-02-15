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

package org.openda.blackbox.wrapper;

import org.openda.blackbox.config.BBStochModelVectorConfig;
import org.openda.blackbox.config.BBUtils;
import org.openda.blackbox.interfaces.SelectorInterface;
import org.openda.exchange.TimeInfo;
import org.openda.interfaces.*;
import org.openda.utils.DimensionIndex;
import org.openda.utils.ObjectSupport;
import org.openda.utils.TreeVector;
import org.openda.utils.Vector;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;

/**
 * BBWrapper ExchangeItem
 */
public class BBExchangeItem implements IExchangeItem {

    private String id;
    private String description = null;
    private BBStochModelVectorConfig vectorConfig;
    private IPrevExchangeItem ioObjectExchangeItem;
	private HashMap<String, SelectorInterface> selectors;
	private File configRootDir;

	public BBExchangeItem(String id, BBStochModelVectorConfig vectorConfig,
                          IPrevExchangeItem ioObjectExchangeItem,
                          HashMap<String, SelectorInterface> selectors, File configRootDir) {
        this.id = id;
        this.vectorConfig = vectorConfig;
        this.ioObjectExchangeItem = ioObjectExchangeItem;
        this.selectors = selectors;
        this.configRootDir = configRootDir;
    }

	public String getId() {
        return id;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

    public Class getValueType() {
        return ioObjectExchangeItem.getValueType();
    }

    public ValueType getValuesType() {
    	ValueType result;
    	if(ioObjectExchangeItem.getValueType()==double.class){
    		result=ValueType.doubleType;
    	}else if(ioObjectExchangeItem.getValueType()==double[].class){
    		result=ValueType.doublesType;
    	}else if(ioObjectExchangeItem.getValueType()==float[].class){
    		result=ValueType.floatsType;
    	}else if(ioObjectExchangeItem.getValueType()==double[][].class){
    		result=ValueType.doubles2dType;
    	}else if(ioObjectExchangeItem.getValueType()==int.class){
    		result=ValueType.intType;
    	}else if(ioObjectExchangeItem.getValueType()==String.class){
    		result=ValueType.StringType;
    	}else if(ioObjectExchangeItem.getValueType()==IVector.class){
    		result=ValueType.IVectorType;
    	}else if(ioObjectExchangeItem.getValueType()==IArray.class){
    		result=ValueType.IArrayType;
    	}else{
    		throw new RuntimeException("BBExchangeItem: unsupported ValueType");
    	}
        return result;
    }

    public Role getRole() {
        return ioObjectExchangeItem.getRole();
    }

    public Object getValues() {

		Class valueType = ioObjectExchangeItem.getValueType();

		Object valuesObject = ioObjectExchangeItem.getValues();

		if (vectorConfig.getSelectorConfig() != null) {
			SelectorInterface selector = BBUtils.createSelectorInstance(
                    configRootDir,
                    vectorConfig.getSelectorConfig().getClassName(),
                    vectorConfig.getSelectorConfig().getArguments());
			selectors.put(id, selector);
			Object selectorResult = selector.select(valuesObject);
			if (!(selectorResult instanceof IVector)) {
				throw new RuntimeException("Unknown return type from selector " +
                        vectorConfig.getSelectorConfig().getClassName());
			}
			return selectorResult;
		} else if (vectorConfig.getSelectionIndices() != null) {
			if ( valueType != IVector.class &&
                    valueType != ITreeVector.class &&
                    valueType != double[].class && 
                    valueType != IArray.class) {
				throw new RuntimeException("Index selection can not be applied on values of type " + valueType);
			}

			IDimensionIndex[] selectionIndices = vectorConfig.getSelectionIndices();
			double[] orgValues = ioObjectExchangeItem.getValuesAsDoubles();
            int[] dimSizes;
            if (valueType == double[].class) {
				dimSizes = new int[1];
                dimSizes[0] = orgValues.length;
			} else if (valueType == IArray.class) {
				dimSizes = ((IArray) valuesObject).getDimensions();
            } else if (valueType == IVector.class) {
				dimSizes = getDimensionSizes((IVector) valuesObject);
			} else {
				throw new RuntimeException("BBModelInstance.getValues: method not implemented for type " + valuesObject.getClass().getName());
            }

			// if ioObjectExchangeItem has timeInfo the first dimension is supposed to be the time dimension.
			// selectionIndices are applied only in space.
			int numberOfTimes;
			if (ioObjectExchangeItem.getTimes() != null) {
				numberOfTimes = ioObjectExchangeItem.getTimes().length;
				if (numberOfTimes > 1) {
					// first dimension is assumed to be the time dimension, method returns all values of time, selection in space
					if (dimSizes[0] != numberOfTimes) {
						throw new RuntimeException("BBModelInstance.getValues(" + this.id + "): first dimension does not equal the dimension of time.");
					}
					// remove the time dimension from dimSizes
					dimSizes = Arrays.copyOfRange(dimSizes, 1, dimSizes.length);
				}
			} else {
				numberOfTimes = 1;
			}
			if (selectionIndices.length == 1 && dimSizes.length == 2 && dimSizes[0] == 1) {
				dimSizes = new int[]{dimSizes[1]};
			}
			if (selectionIndices.length != dimSizes.length) {
				throw new RuntimeException("BBModelInstance.getValues(" + this.id + "): unexpected #dimensions in IoElement " + vectorConfig.getSourceId());
			}

			int valuesPerTime = orgValues.length / numberOfTimes;
			String treeVectorId = valuesObject instanceof ITreeVector ? ((ITreeVector)valuesObject).getId() + "(...)" : null;

			int subsize = 1;
			for (IDimensionIndex selectionIndex : selectionIndices) {
				subsize = subsize * selectionIndex.getSize();
			}

            double[] allSubValues = new double[subsize * numberOfTimes];
            for (int i=0; i<numberOfTimes; i++) {
				int [] start = new int[]{i*valuesPerTime};
				int [] eind = new int[]{valuesPerTime - 1 + i*valuesPerTime};
				double[] orgValuesPerTimeStep = DimensionIndex.getSubArray(orgValues, new int[]{1}, start, eind);
	            double[] subValuesPerTimeStep = DimensionIndex.accessSubArray(id, orgValuesPerTimeStep, null, selectionIndices, dimSizes);
				for (int j=0; j<subsize; j++) {
                    allSubValues[i*subsize+j] = subValuesPerTimeStep[j];
                }
            }

            IVector subVector = new Vector(allSubValues);
            if (selectionIndices.length == 1) {
                if (treeVectorId != null) {
                    return new TreeVector(treeVectorId, subVector);
                } else {
                    return subVector;
                }
            } else if (selectionIndices.length == 2) {
                return new TreeVector(treeVectorId, subVector,
                        selectionIndices[0].getEnd()-selectionIndices[0].getStart(),
                        selectionIndices[1].getEnd()-selectionIndices[1].getStart());
            } else if (selectionIndices.length == 3) {
                return new TreeVector(treeVectorId, subVector,
                        selectionIndices[0].getEnd()-selectionIndices[0].getStart(),
                        selectionIndices[1].getEnd()-selectionIndices[1].getStart(),
                        selectionIndices[2].getEnd()-selectionIndices[2].getStart());
            } else {
                throw new RuntimeException("BBExchangeItem.getValues(" + id + "): " +
                        "more then 3 dimensions");
            }
        } else {
            return valuesObject;
        }
    }

    public double[] getValuesAsDoubles() {
        boolean isselected = ((vectorConfig.getSelectorConfig() != null) ||
                (vectorConfig.getSelectionIndices() != null));
        if (!isselected) {
			return ioObjectExchangeItem.getValuesAsDoubles();
		} else {
            Object valuesObject = getValues();
            if (valuesObject instanceof IVector) {
                return ((IVector) valuesObject).getValues();
            } else if (valuesObject instanceof Double) {
                return new double[]{(Double) valuesObject};
            } else if (valuesObject instanceof double[]) {
                return (double[]) valuesObject;
            } else {
                throw new RuntimeException("BBExchangeItem.getValuesAsDoubles(" + id + "): " +
                        "unknown object type: " + valuesObject.getClass().getName());
            }
        }
    }

    public void axpyOnValues(double alpha, double[] axpyValues) {
        if (vectorConfig.getSelectorConfig() != null || vectorConfig.getSelectionIndices() != null) {
            double[] values = getValuesAsDoubles();
            for (int i = 0; i < values.length; i++) {
                values[i] += alpha * axpyValues[i];
            }
            setValuesAsDoubles(values);
        } else {
			ioObjectExchangeItem.axpyOnValues(alpha, axpyValues);
        }
    }

	public void multiplyValues(double[] multiplicationFactors) {
		if (vectorConfig.getSelectorConfig() != null || vectorConfig.getSelectionIndices() != null) {
			double[] values = getValuesAsDoubles();
			for (int i = 0; i < values.length; i++) {
				values[i] *= multiplicationFactors[i];
			}
			setValuesAsDoubles(values);
		} else {
			ioObjectExchangeItem.multiplyValues(multiplicationFactors);
		}
	}

	public void setValues(Object values) {
        if (vectorConfig.getSelectorConfig() != null) {
            SelectorInterface selector = selectors.get(id);
            if (selector == null) {
                throw new IllegalStateException("BBExchangeItem.setValues(" +
                        id + "): " +
                        "de-selection not possible, no selector created");
            }
			setBaseOrOrgExchangeItemValues(selector.deselect(values));
			selectors.remove(id);
        } else {
            IDimensionIndex[] selectionIndices = vectorConfig.getSelectionIndices();
            if (selectionIndices == null) {
                // setvalues without selections
                if (values instanceof  double[]){
                    ioObjectExchangeItem.setValuesAsDoubles((double[]) values);
                } else if (ioObjectExchangeItem.getValueType() == double.class) {
                    if (values instanceof Double) {
						setBaseOrOrgExchangeItemValues(values);
                    } else if (values instanceof IVector) {
                        IVector vector = (IVector) values;
                        if (vector.getSize() != 1) {
                            throw new IllegalArgumentException("Invalid length (" + vector.getSize() +
                                    " for exchange item \"" + id + "\" (ValueType Double, so lengt must be 1)");
                        }
						setBaseOrOrgExchangeItemValues(vector.getValue(0));
                    }
                } else {
					ObjectSupport.checkCompatibility(values, ioObjectExchangeItem.getValueType(),
							this.getClass().getName() + ".setValues()");
					setBaseOrOrgExchangeItemValues(values);
                }
            } else {
                double[] newValues;
                if (values instanceof double[]) {
                    newValues = (double[]) values;
                } else if (values instanceof IVector) {
                    newValues = ((IVector) values).getValues();
                } else {
                    throw new IllegalArgumentException("BBExchangeItem.setValues(" + id +
                            "): index de-selection can only be applied to vectors or arrays of doubles");
                }
                int[] dimSizes;
                IVector orgVector = (IVector) (ioObjectExchangeItem.getValues());
                dimSizes = getDimensionSizes(orgVector);
                if (selectionIndices.length != dimSizes.length) {
                    throw new RuntimeException("BBExchangeItem.setValues(" + id +
                            "): unexpected #dimensions in IoElement " + vectorConfig.getSourceId());
                }
                double[] adjustedValues = DimensionIndex.accessSubArray(id, orgVector.getValues(),
                        newValues, selectionIndices, dimSizes);
				ioObjectExchangeItem.setValuesAsDoubles(adjustedValues);
			}
        }
    }

	private void setBaseOrOrgExchangeItemValues(Object values) {
		ioObjectExchangeItem.setValues(values);
	}

	public void copyValuesFromItem(IExchangeItem sourceItem) {
		if (sourceItem.getValueType() != getValueType()) {
			throw new RuntimeException("Incompatible value types in copy action from " + sourceItem.getId() +
					" to " + getId() + "(" + sourceItem.getValueType().toString() + "/=" + getValueType().toString());
		}
		setValues(sourceItem.getValues());
	}

	public void setValuesAsDoubles(double[] values) {
        setValues(values);
    }

    public double[] getTimes() {
		return ioObjectExchangeItem.getTimes();
    }

    public void setTimes(double[] times) {
		ioObjectExchangeItem.setTimes(times);
	}

	public ITimeInfo getTimeInfo() {
		if (ioObjectExchangeItem instanceof IExchangeItem) {
			return ((IExchangeItem) ioObjectExchangeItem).getTimeInfo();
		}
		return new TimeInfo(ioObjectExchangeItem.getTimes());
	}

	public IQuantityInfo getQuantityInfo() {
		throw new UnsupportedOperationException("org.openda.exchange.DoubleExchangeItem.getQuantityInfo(): Not implemented yet.");
	}

	public IGeometryInfo getGeometryInfo() {
		//TODO all exchangeItems should implement IExchangeItem. AK
		if (this.ioObjectExchangeItem instanceof IExchangeItem) {
			return ((IExchangeItem) this.ioObjectExchangeItem).getGeometryInfo();
		}
		return null;
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
}
