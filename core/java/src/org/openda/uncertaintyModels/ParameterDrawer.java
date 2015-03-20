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
package org.openda.uncertaintyModels;


import org.openda.interfaces.*;
import org.openda.utils.TreeVector;
import org.openda.utils.io.CsvReader;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashMap;
import java.util.Random;

public class ParameterDrawer implements IStochModelInstance {

    GlueStochVector glueStochVector;

    @Override
    public IVector getState() {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.getState(): Not implemented yet.");
    }

    @Override
    public void axpyOnState(double alpha, IVector vector) {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.axpyOnState(): Not implemented yet.");
    }

    @Override
    public IVector getParameters() {
        return glueStochVector.getExpectations();
    }

    @Override
    public void setParameters(IVector parameters) {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.setParameters(): Not implemented yet.");
    }

    @Override
    public void axpyOnParameters(double alpha, IVector vector) {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.axpyOnParameters(): Not implemented yet.");
    }

    @Override
    public IStochVector getStateUncertainty() {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.getStateUncertainty(): Not implemented yet.");
    }

    @Override
    public IStochVector getParameterUncertainty() {
        return glueStochVector;
    }

    @Override
    public IStochVector[] getWhiteNoiseUncertainty(ITime time) {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.getWhiteNoiseUncertainty(): Not implemented yet.");
    }

    @Override
    public boolean isWhiteNoiseStationary() {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.isWhiteNoiseStationary(): Not implemented yet.");
    }

    @Override
    public ITime[] getWhiteNoiseTimes(ITime timeSpan) {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.getWhiteNoiseTimes(): Not implemented yet.");
    }

    @Override
    public IVector[] getWhiteNoise(ITime timeSpan) {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.getWhiteNoise(): Not implemented yet.");
    }

    @Override
    public void setWhiteNoise(IVector[] whiteNoise) {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.setWhiteNoise(): Not implemented yet.");
    }

    @Override
    public void axpyOnWhiteNoise(double alpha, IVector[] vector) {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.axpyOnWhiteNoise(): Not implemented yet.");
    }

    @Override
    public void setAutomaticNoiseGeneration(boolean value) {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.setAutomaticNoiseGeneration(): Not implemented yet.");
    }

    @Override
    public IVector getObservedValues(IObservationDescriptions observationDescriptions) {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.getObservedValues(): Not implemented yet.");
    }

    @Override
    public void announceObservedValues(IObservationDescriptions observationDescriptions) {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.announceObservedValues(): Not implemented yet.");
    }

    @Override
    public IVector getStateScaling() {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.getStateScaling(): Not implemented yet.");
    }

    @Override
    public IVector[] getStateScaling(IObservationDescriptions observationDescriptions) {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.getStateScaling(): Not implemented yet.");
    }

    @Override
    public IPrevExchangeItem getExchangeItem(String exchangeItemID) {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.getExchangeItem(): Not implemented yet.");
    }

    @Override
    public ITime getTimeHorizon() {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.getTimeHorizon(): Not implemented yet.");
    }

    @Override
    public ITime getCurrentTime() {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.getCurrentTime(): Not implemented yet.");
    }

    @Override
    public void compute(ITime targetTime) {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.compute(): Not implemented yet.");
    }

    @Override
    public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance) {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.getObservedLocalization(): Not implemented yet.");
    }

    @Override
    public IModelState saveInternalState() {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.saveInternalState(): Not implemented yet.");
    }

    @Override
    public void restoreInternalState(IModelState savedInternalState) {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.restoreInternalState(): Not implemented yet.");
    }

    @Override
    public void releaseInternalState(IModelState savedInternalState) {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.releaseInternalState(): Not implemented yet.");
    }

    @Override
    public IModelState loadPersistentState(File persistentStateFile) {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.loadPersistentState(): Not implemented yet.");
    }

    @Override
    public File getModelRunDir() {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.getModelRunDir(): Not implemented yet.");
    }

    @Override
    public String[] getExchangeItemIDs() {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.getExchangeItemIDs(): Not implemented yet.");
    }

    @Override
    public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.getExchangeItemIDs(): Not implemented yet.");
    }

    @Override
    public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.getDataObjectExchangeItem(): Not implemented yet.");
    }

    @Override
    public void finish() {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.finish(): Not implemented yet.");
    }

    @Override
    public void initialize(File workingDir, String[] arguments) {
		File paramsFile = new File(workingDir, arguments[0]);
		try {
			HashMap<Integer, String[]> rowValues = new HashMap<Integer,String[]>();
			CsvReader glueParamsReader = new CsvReader(paramsFile);

            // read parameter-sample file:
            glueParamsReader.setColumnSeparatorChar(',');
            String[] thisRow;
            int j=0;
            while ((thisRow = glueParamsReader.readCSVLineTrimElements()) != null) {
                rowValues.put(j,thisRow);
                j++;
            }
            glueParamsReader.close();

            // create TreeVectors of parameter-sample:
            int nParamsSample = rowValues.size()-1;
			TreeVector[] paramsSample = new TreeVector[nParamsSample];
            int nParams = rowValues.get(0).length-1;
            for (int i=1; i<=nParamsSample; i++){
                paramsSample[i-1] = new TreeVector("parameterSet"+i);
                for (int k=1; k<=nParams; k++){
                    String childId = rowValues.get(0)[k];
                    double childValue = Double.valueOf(rowValues.get(i)[k]);
                    paramsSample[i-1].addChild(childId,new double[]{childValue});
                }
            }
			TreeVector paramsZero = new TreeVector("parameters");
			for (int k = 1; k <= nParams; k++) {
				String childId = rowValues.get(0)[k];
				paramsZero.addChild(childId, new double[]{0});
			}
			glueStochVector = new GlueStochVector(paramsZero, paramsSample);

        } catch (FileNotFoundException e) {
            throw new RuntimeException(this.getClass().getName()+", file not found: " +
					paramsFile.getAbsolutePath());
        } catch (IOException e) {
			throw new RuntimeException(this.getClass().getName() + ", could not read from file: " +
					paramsFile.getAbsolutePath());
		}
    }

    @Override
    public IInstance getParent() {
        throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.getParent(): Not implemented yet.");
    }

    private class GlueStochVector implements IStochVector {

		private TreeVector paramsZero;
		private TreeVector[] paramsRealizations;
        Random rIndex;

        public GlueStochVector(TreeVector paramsZero, TreeVector[] paramsRealizations){
			this.paramsZero = paramsZero;
			this.paramsRealizations = paramsRealizations.clone();
        }
        @Override
        public IVector createRealization() {
            rIndex = new Random();
            int selectedIndex = rIndex.nextInt(paramsRealizations.length);
            return paramsRealizations[selectedIndex];
        }

        @Override
        public double evaluatePdf(IVector tv) {
            throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.GlueStochVector.evaluatePdf(): Not implemented yet.");
        }

        @Override
        public IVector getExpectations() {
			return paramsZero;
        }

        @Override
        public ISqrtCovariance getSqrtCovariance() {
            throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.GlueStochVector.getSqrtCovariance(): Not implemented yet.");
        }

        @Override
        public boolean hasCorrelatedElements() {
            throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.GlueStochVector.hasCorrelatedElements(): Not implemented yet.");
        }

        @Override
        public IVector getStandardDeviations() {
            throw new UnsupportedOperationException("org.openda.uncertaintyModels.ParameterDrawer.GlueStochVector.getStandardDeviations(): Not implemented yet.");
        }

		public String toString() {
			return "{" + this.paramsZero + "+" + this.paramsRealizations.length + "real.}";
		}
    }
}
