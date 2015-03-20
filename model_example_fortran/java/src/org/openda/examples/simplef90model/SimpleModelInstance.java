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
package org.openda.examples.simplef90model;

import org.openda.interfaces.*;
import org.openda.utils.Time;

import java.io.File;
import java.util.HashMap;

/**
 * Example of an OpenDa model that is accessing a model DLL
 */
public class SimpleModelInstance implements IModelInstance, IModelAdjoint {

    private IInstance parent;
    private SimpleModelDLL simpleModelDLL;
    private HashMap<String, SimpleExchangeItem> exchangeItems = new HashMap<String, SimpleExchangeItem>();
    private File instanceDir;

    public static void initialize(File simpleFortranDll, File modelInstanceParentDir, File modelTemplateDir, String schemFileName) {
        SimpleModelDLL.initialize(simpleFortranDll, modelInstanceParentDir, modelTemplateDir, schemFileName);
    }

    public void initialize(File workingDir, String[] arguments) {
        // no action needed (handled by above method)
    }

    public static IModelInstance getInstance(File instanceDir, IInstance parent) {
        // return new instance
        return new SimpleModelInstance(instanceDir, parent);
    }

    private SimpleModelInstance(File instanceDir, IInstance parent) {

        this.instanceDir = instanceDir;
        this.simpleModelDLL = SimpleModelDLL.getInstance(instanceDir);
        this.parent = parent;

        // Create the exchange items
        String exchangeItemId = "Gravity";
        SimpleExchangeItem SimpleExchangeItem =
                new SimpleExchangeItem(exchangeItemId, SimpleModelDLL.gravity, double.class, this);
        exchangeItems.put(exchangeItemId, SimpleExchangeItem);
        exchangeItemId = "GridPoints.WaterLevel";
        SimpleExchangeItem =
                new SimpleExchangeItem(exchangeItemId, SimpleModelDLL.waterlevel_on_grid, double[].class, this);
        exchangeItems.put(exchangeItemId, SimpleExchangeItem);
        exchangeItemId = "GridPoints.Friction";
        SimpleExchangeItem =
                new SimpleExchangeItem(exchangeItemId, SimpleModelDLL.friction_on_grid, double[].class, this);
        exchangeItems.put(exchangeItemId, SimpleExchangeItem);
        exchangeItemId = "Laterals.Discharge";
        SimpleExchangeItem =
                new SimpleExchangeItem(exchangeItemId, SimpleModelDLL.discharge_on_laterals, double[].class, this);
        exchangeItems.put(exchangeItemId, SimpleExchangeItem);
    }

    public IInstance getParent() {
        return parent;
    }

    public ITime getTimeHorizon() {
        return new Time(simpleModelDLL.getStartTime(), simpleModelDLL.getEndTime());
    }

    public ITime getCurrentTime() {
        return new Time(simpleModelDLL.getCurrentTime());
    }

    public void compute(ITime targetTime) {
        ITime currentTime = getCurrentTime();
        while (targetTime.after(currentTime)) {
            simpleModelDLL.compute(currentTime, targetTime);
        }
    }

    public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance) {
        return new IVector[0];  //To change body of implemented methods use File | Settings | File Templates.
    }

    public String[] getExchangeItemIDs() {
        String[] exchangeItemIDs = new String[exchangeItems.size()];
        for (int i = 0; i < exchangeItemIDs.length; i++) {
            exchangeItemIDs[i] = exchangeItems.get(String.valueOf(i)).getId();
        }
        return exchangeItemIDs;
    }

    public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
        if (role == IPrevExchangeItem.Role.InOut) {
            return  getExchangeItemIDs();
        }
        throw new UnsupportedOperationException("getExchangeItemIDs(role = in or out): Selection not yet implemented yet.");
    }

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		throw new UnsupportedOperationException("org.openda.examples.simplef90model.SimpleModelInstance.getDataObjectExchangeItem(): Not implemented yet.");
	}

	public IPrevExchangeItem getExchangeItem(String exchangeItemId) {
        IPrevExchangeItem exchangeItem = exchangeItems.get(exchangeItemId);
        if (exchangeItem == null) {
            throw new RuntimeException("Invalid exchange item id: " + exchangeItemId);
        }
        return exchangeItem;
    }

    public IModelState saveInternalState() {
        throw new UnsupportedOperationException("org.openda.models.examples.dllmodel.SimpleModelInstance.saveInternalState(): Not implemented yet.");
    }

    public void restoreInternalState(IModelState o) {
        throw new UnsupportedOperationException("org.openda.models.examples.dllmodel.SimpleModelInstance.restoreInternalState(): Not implemented yet.");
    }

    public void releaseInternalState(IModelState o) {
        throw new UnsupportedOperationException("org.openda.models.examples.dllmodel.SimpleModelInstance.releaseInternalState(): Not implemented yet.");
    }

	public IModelState loadPersistentState(File persistentStateFile) {
		throw new UnsupportedOperationException("org.openda.examples.simplef90model.SimpleModelInstance.loadPersistentState(): Not implemented yet.");
	}

	public File getModelRunDir() {
        throw new UnsupportedOperationException("org.openda.models.examples.dllmodel.SimpleModelInstance.getModelRunDir(): Not implemented yet.");
    }

    public void finish() {
		// no action needed (yet)
	}

	public IVector applyObservationTangent(IVector deltaState, IObservationDescriptions observationMetaData) {
		throw new UnsupportedOperationException("org.openda.examples.simplef90model: applyObservationTangent");
//        if (!(observationMetaData instanceof ITreeVector)) {
//            throw new RuntimeException(
//                    "SimpleModelInstance.applyObservationTangent(): observationMetaData must be a TreeVector");
//        }
//        ITreeVector treeVector = (ITreeVector) observationMetaData;
//
//        int[] arrayIdentifiersForObsPoints = new int[observationMetaData.getSize()];
//        int[] pointIndicesInArray = new int[observationMetaData.getSize()];
//        translateObservationMetaDataToArrayIndices(treeVector, arrayIdentifiersForObsPoints, pointIndicesInArray, null);
//        double[] result = simpleModelDLL.applyObservationTangent(deltaState.getValues(),
//                arrayIdentifiersForObsPoints,
//                pointIndicesInArray
//                );
//        return new Vector(result);
    }

	public IVector applyObservationAdjoint(IVector lambaY,IObservationDescriptions observationMetaData) {
		throw new UnsupportedOperationException("org.openda.examples.simplef90model: applyObservationTangent");
//        if (!(lambaY instanceof ITreeVector)) {
//            throw new RuntimeException(
//                    "SimpleModelInstance.applyObservationAdjoint(): observationMetaData must be a TreeVector");
//        }
//        ITreeVector treeVector = (ITreeVector) lambaY;
//
//        int[] arrayIdentifiersForObsPoints = new int[lambaY.getSize()];
//        int[] pointIndicesInArray = new int[lambaY.getSize()];
//        double[] observationValues = new double[lambaY.getSize()];
//        translateObservationMetaDataToArrayIndices(treeVector, arrayIdentifiersForObsPoints, pointIndicesInArray, observationValues);
//        double[] lambdaX = simpleModelDLL.applyObservationAdjoint(arrayIdentifiersForObsPoints,
//                pointIndicesInArray,
//                observationValues
//                );
//        return new Vector(lambdaX);
    }

    public double[] getTimes() {
        double startTime = simpleModelDLL.getStartTime();
        double endTime = simpleModelDLL.getEndTime();
        double deltaT = simpleModelDLL.getDeltaT();
        int timesCount = (int) Math.round((endTime - startTime) / deltaT);
        double[] times = new double[timesCount+1];
        for (int t = 0; t <= times.length; t++) {
            times[t] = startTime + t * deltaT;
        }

        final double timeEpsilon = 1e-8;
        if (Math.abs(endTime - times[timesCount]) > timeEpsilon ) {
            throw new RuntimeException("Times to inaccurate in " + this.getClass().getName() +
            " on instance directory " + instanceDir.getAbsolutePath());
        }
        return times;
    }

    public double[] getValuesForExchangeItem(int indexInDLL) {
        return simpleModelDLL.getValues(indexInDLL);
    }

    public double getValueForExchangeItem(int indexInDLL) {
        return simpleModelDLL.getValue(indexInDLL);
    }

    public void setValuesForExchangeItem(int indexInDLL, double[] values) {
        simpleModelDLL.setValues(indexInDLL, values);
    }

    public void setValueForExchangeItem(int indexInDLL, double value) {
        simpleModelDLL.setValue(indexInDLL, value);
    }

    private void translateObservationMetaDataToArrayIndices(
            ITreeVector treeVector, int[] arrayIdentifiersForObsPoints, int[] pointIndicesInArray, double[] result) {

        // note: code intentionally kept 'ugly', to show how observation quantity and location id's
        // are translated into the related computational result array and the related index

        int i = 0;
        for (String subVectorId : treeVector.getSubTreeVectorIds()) {
            ITreeVector subVector = treeVector.getSubTreeVector(subVectorId);
            if (result!=null) {
                result[i] = subVector.getValue(0);  // assume one value per location
            }
            if (subVector.getId().equals("grid point 1.water level")) {
                arrayIdentifiersForObsPoints[i]=SimpleModelDLL.waterlevel_on_grid;
                pointIndicesInArray[i] = 1;
            } else if (subVector.getId().equals("grid point 2.water level")) {
                arrayIdentifiersForObsPoints[i]=SimpleModelDLL.waterlevel_on_grid;
                pointIndicesInArray[i] = 2;
            } else if (subVector.getId().equals("grid point 3.water level")) {
                arrayIdentifiersForObsPoints[i]=SimpleModelDLL.waterlevel_on_grid;
                pointIndicesInArray[i] = 3;
            } else if (subVector.getId().equals("grid point 4.water level")) {
                arrayIdentifiersForObsPoints[i]=SimpleModelDLL.waterlevel_on_grid;
                pointIndicesInArray[i] = 4;
            }
        }
    }
}
