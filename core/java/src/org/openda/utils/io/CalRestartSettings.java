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


package org.openda.utils.io;

import org.openda.interfaces.IModelState;
import org.openda.interfaces.ITreeVector;
import org.openda.interfaces.IVector;
import org.openda.core.io.castorgenerated.*;
import org.openda.core.io.castorgenerated.types.MethodNameXML;
import org.openda.utils.Vector;

import java.io.File;

/**
 * Restart settings for an OpenDA algorithm
 */
public class CalRestartSettings implements IModelState {

    CalibrationRestartXML calibrationRestartXML;
    MethodNameXML methodNameXML;

    public CalRestartSettings(String methodName) {
        calibrationRestartXML = new CalibrationRestartXML();
        methodNameXML = MethodNameXML.valueOf(methodName);
        calibrationRestartXML.setMethodName(methodNameXML);
        calibrationRestartXML.setCalibrationRestartXMLChoice(new CalibrationRestartXMLChoice());
        switch (methodNameXML.getType()) {
            case MethodNameXML.DUD_TYPE:
                calibrationRestartXML.getCalibrationRestartXMLChoice().setDud(new DudXML());
                break;
            case MethodNameXML.POWELL_TYPE:
                PowellXML powellXML = new PowellXML();
                powellXML.setCostValue(Double.NaN);
                calibrationRestartXML.getCalibrationRestartXMLChoice().setPowell(powellXML);
                break;
            case MethodNameXML.SIMPLEX_TYPE:
                calibrationRestartXML.getCalibrationRestartXMLChoice().setSimplex(new SimplexXML());
                break;
        }
    }

	public void savePersistentState(File savedStateFile) {
		writeToFile(savedStateFile);
	}

    public void setParameters(IVector[] parameters) {
        switch (methodNameXML.getType()) {
            case MethodNameXML.DUD_TYPE:
                DudXML dudXML = calibrationRestartXML.getCalibrationRestartXMLChoice().getDud();
                for (IVector parameterVector : parameters) {
                    DudXMLChoiceItem paramVectorOrTreeVectorXML = new DudXMLChoiceItem();
                    if (parameterVector instanceof ITreeVector) {
                        paramVectorOrTreeVectorXML.setParams(TreeVectorWriter.createTreeVectorXML((ITreeVector) parameterVector));
                    } else {
                        paramVectorOrTreeVectorXML.setParamVector(TreeVectorWriter.createVectorXML(parameterVector));
                    }
                    DudXMLChoice parametersXML = new DudXMLChoice();
                    parametersXML.setDudXMLChoiceItem(paramVectorOrTreeVectorXML);
                    dudXML.addDudXMLChoice(parametersXML);
                }
                break;
            case MethodNameXML.SIMPLEX_TYPE:
                SimplexXML simplexXML = calibrationRestartXML.getCalibrationRestartXMLChoice().getSimplex();
                for (IVector parameterVector : parameters) {
                    SimplexXMLChoiceItem paramVectorOrTreeVectorXML = new SimplexXMLChoiceItem();
                    if (parameterVector instanceof ITreeVector) {
                        paramVectorOrTreeVectorXML.setParams(TreeVectorWriter.createTreeVectorXML((ITreeVector) parameterVector));
                    } else {
                        paramVectorOrTreeVectorXML.setParamVector(TreeVectorWriter.createVectorXML(parameterVector));
                    }
                    SimplexXMLChoice parametersXML = new SimplexXMLChoice();
                    parametersXML.setSimplexXMLChoiceItem(paramVectorOrTreeVectorXML);
                    simplexXML.addSimplexXMLChoice(parametersXML);
                }
                break;
            case MethodNameXML.POWELL_TYPE:
                throw new IllegalArgumentException("Parameters can not be set for a Powell algorithm");
        }
    }

    public void setParameters(IVector baseParameters, IVector[] searchDirectionParams) {

        PowellXML powellXML = calibrationRestartXML.getCalibrationRestartXMLChoice().getPowell();

        PowellXMLChoice baseParamVectorOrTreeVectorXML = new PowellXMLChoice();
        if (baseParameters instanceof ITreeVector) {
            baseParamVectorOrTreeVectorXML.setBaseParams(TreeVectorWriter.createTreeVectorXML((ITreeVector) baseParameters));
        } else {
            baseParamVectorOrTreeVectorXML.setBaseParamVector(TreeVectorWriter.createVectorXML(baseParameters));
        }
        powellXML.setPowellXMLChoice(baseParamVectorOrTreeVectorXML);

        for (IVector parameterVector : searchDirectionParams) {
            PowellXMLChoice2Item paramVectorOrTreeVector = new PowellXMLChoice2Item();
            if (parameterVector instanceof ITreeVector) {
                paramVectorOrTreeVector.setSearchDirParams(TreeVectorWriter.createTreeVectorXML((ITreeVector) parameterVector));
            } else {
                paramVectorOrTreeVector.setSearchDirParamVector(TreeVectorWriter.createVectorXML(parameterVector));
            }
            PowellXMLChoice2 searchDirParamXML = new PowellXMLChoice2();
            searchDirParamXML.setPowellXMLChoice2Item(paramVectorOrTreeVector);
            powellXML.addPowellXMLChoice2(searchDirParamXML);
        }
    }

    public void setCostValues(double[] costValues) {
        switch (methodNameXML.getType()) {
            case MethodNameXML.DUD_TYPE:
                DudXML dudXML = calibrationRestartXML.getCalibrationRestartXMLChoice().getDud();
                for (double costValue : costValues) {
                    dudXML.addCostValue(costValue);
                }
                break;
            case MethodNameXML.SIMPLEX_TYPE:
                SimplexXML simplexXML = calibrationRestartXML.getCalibrationRestartXMLChoice().getSimplex();
                for (double costValue : costValues) {
                    simplexXML.addCostValue(costValue);
                }
                break;
            case MethodNameXML.POWELL_TYPE:
                throw new IllegalArgumentException("Array of cost values a can not be set for a Powell algorithm");
        }
    }

    public void setCostValue(double costValue) {
        switch (methodNameXML.getType()) {
            case MethodNameXML.POWELL_TYPE:
                calibrationRestartXML.getCalibrationRestartXMLChoice().getPowell().setCostValue(costValue);
                break;
            case MethodNameXML.DUD_TYPE:
            case MethodNameXML.SIMPLEX_TYPE:
                throw new IllegalArgumentException("Single cost values a can only be set for a Powell algorithm");
        }
    }

    public void setPredictions(IVector[] predictions) {
        switch (methodNameXML.getType()) {
            case MethodNameXML.DUD_TYPE:
                DudXML dudXML = calibrationRestartXML.getCalibrationRestartXMLChoice().getDud();
                for (IVector predictionVector : predictions) {
                    DudXMLChoice2Item predVectorOrTreeVectorXML = new DudXMLChoice2Item();
                    if (predictionVector instanceof ITreeVector) {
                        predVectorOrTreeVectorXML.setPredictions(TreeVectorWriter.createTreeVectorXML((ITreeVector) predictionVector));
                    } else {
                        predVectorOrTreeVectorXML.setPredictionVector(TreeVectorWriter.createVectorXML(predictionVector));
                    }
                    DudXMLChoice2 predictionXML = new DudXMLChoice2();
                    predictionXML.setDudXMLChoice2Item(predVectorOrTreeVectorXML);
                    dudXML.addDudXMLChoice2(predictionXML);
                }
                break;
            case MethodNameXML.SIMPLEX_TYPE:
            case MethodNameXML.POWELL_TYPE:
                throw new IllegalArgumentException("Predictions can only be set for a Powell algorithm");
        }
    }

    public void setComment(String comment) {
        calibrationRestartXML.setComment(comment);
    }

    public String getComment() {
        return calibrationRestartXML.getComment();
    }

    public CalRestartSettings(String methodName, File restartFile) {
        if (restartFile == null) {
            throw new IllegalArgumentException("argument parameterSettingsFile == null");
        }
        if (!restartFile.exists()) {
            throw new IllegalArgumentException("file " + restartFile.getAbsolutePath() + " does not exist: ");
        }
        calibrationRestartXML = 
        	 (CalibrationRestartXML) CastorUtils.parse(restartFile, CalibrationRestartXML.class);
        
        if (methodName != null && 
            !methodName.equalsIgnoreCase(calibrationRestartXML.getMethodName().toString())) {
            throw new RuntimeException("Invalid calibration methodName in file " + 
            		                    restartFile.getAbsolutePath());
        }
        methodNameXML = calibrationRestartXML.getMethodName();
    }

    public String getMethodName() {
        return calibrationRestartXML.getMethodName().toString();
    }

    public IVector[] getParameters() {

        int parameterVectorCount = 0;

        DudXML dudXML = calibrationRestartXML.getCalibrationRestartXMLChoice().getDud();
        SimplexXML simplexXML = calibrationRestartXML.getCalibrationRestartXMLChoice().getSimplex();

        switch (methodNameXML.getType()) {
            case MethodNameXML.DUD_TYPE:
                parameterVectorCount = dudXML.getDudXMLChoice2Count();
                break;
            case MethodNameXML.SIMPLEX_TYPE:
                parameterVectorCount = simplexXML.getSimplexXMLChoiceCount();
                break;
            case MethodNameXML.POWELL_TYPE:
                throw new IllegalArgumentException("Parameters can not be retreived for a Powell algorithm");
        }

        IVector[] parameters = new IVector[parameterVectorCount];
        for (int i = 0; i < parameters.length; i++) {
            String vectorXML;
            TreeVectorXML treeVectorXML;
            if (methodNameXML.getType() == MethodNameXML.DUD_TYPE) {
                vectorXML = dudXML.getDudXMLChoice(i).getDudXMLChoiceItem().getParamVector();
                treeVectorXML = dudXML.getDudXMLChoice(i).getDudXMLChoiceItem().getParams();
            } else {  // Simplex
                vectorXML = simplexXML.getSimplexXMLChoice(i).getSimplexXMLChoiceItem().getParamVector();
                treeVectorXML = simplexXML.getSimplexXMLChoice(i).getSimplexXMLChoiceItem().getParams();
            }
            if (vectorXML != null) {
                // vector
                parameters[i] = new Vector(TreeVectorReader.parseValuesFromSpaceSeparatedString(vectorXML));
            } else {
                parameters[i] = TreeVectorReader.parseTreeVector(null, treeVectorXML);
            }
        }
        return parameters;
    }

    public IVector[] getPredictions() {

        int parameterVectorCount = 0;

        DudXML dudXML = calibrationRestartXML.getCalibrationRestartXMLChoice().getDud();

        switch (methodNameXML.getType()) {
            case MethodNameXML.DUD_TYPE:
                parameterVectorCount = dudXML.getDudXMLChoice2Count();
                break;
            case MethodNameXML.SIMPLEX_TYPE:
            case MethodNameXML.POWELL_TYPE:
                throw new IllegalArgumentException("Predictions can only be retreived for a Dud algorithm");
        }

        IVector[] parameters = new IVector[parameterVectorCount];
        for (int i = 0; i < parameters.length; i++) {
            String vectorXML = dudXML.getDudXMLChoice2(i).getDudXMLChoice2Item().getPredictionVector();
            TreeVectorXML treeVectorXML = dudXML.getDudXMLChoice2(i).getDudXMLChoice2Item().getPredictions();
            if (vectorXML != null) {
                // vector
                parameters[i] = new Vector(TreeVectorReader.parseValuesFromSpaceSeparatedString(vectorXML));
            } else {
                parameters[i] = TreeVectorReader.parseTreeVector(null, treeVectorXML);
            }
        }
        return parameters;
    }

    public IVector getBaseParameters() {

        PowellXML powellXML = calibrationRestartXML.getCalibrationRestartXMLChoice().getPowell();
        IVector baseParameters = null;
        switch (methodNameXML.getType()) {
            case MethodNameXML.POWELL_TYPE:
                String vectorXML = powellXML.getPowellXMLChoice().getBaseParamVector();
                TreeVectorXML treeVectorXML = powellXML.getPowellXMLChoice().getBaseParams();

                if (vectorXML != null) {
                    // vector
                    baseParameters = new Vector(TreeVectorReader.parseValuesFromSpaceSeparatedString(vectorXML));
                } else {
                    baseParameters = TreeVectorReader.parseTreeVector(null, treeVectorXML);
                }
                break;
            case MethodNameXML.SIMPLEX_TYPE:
            case MethodNameXML.DUD_TYPE:
                throw new IllegalArgumentException("Parameters can only be retreived for a Powell algorithm");
        }
        return baseParameters;
    }

    public IVector[] getSearchDirParameters() {

        int searchDirParameterVectorCount = 0;

        PowellXML powellXML = calibrationRestartXML.getCalibrationRestartXMLChoice().getPowell();

        switch (methodNameXML.getType()) {
            case MethodNameXML.POWELL_TYPE:
                searchDirParameterVectorCount = powellXML.getPowellXMLChoice2Count();
                break;
            case MethodNameXML.SIMPLEX_TYPE:
            case MethodNameXML.DUD_TYPE:
                throw new IllegalArgumentException("Parameters can only be retreived for a Powell algorithm");
        }

        IVector[] searchDirParameters = new IVector[searchDirParameterVectorCount];
        for (int i = 0; i < searchDirParameters.length; i++) {
            String vectorXML = powellXML.getPowellXMLChoice2(i).getPowellXMLChoice2Item().getSearchDirParamVector();
            TreeVectorXML treeVectorXML = powellXML.getPowellXMLChoice2(i).getPowellXMLChoice2Item().getSearchDirParams();
            if (vectorXML != null) {
                // vector
                searchDirParameters[i] = new Vector(TreeVectorReader.parseValuesFromSpaceSeparatedString(vectorXML));
            } else {
                searchDirParameters[i] = TreeVectorReader.parseTreeVector(null, treeVectorXML);
            }
        }
        return searchDirParameters;
    }

    public double[] getCosts() {

        double[] costs = null;
        switch (methodNameXML.getType()) {
            case MethodNameXML.DUD_TYPE:
                costs = calibrationRestartXML.getCalibrationRestartXMLChoice().getDud().getCostValue();
                break;
            case MethodNameXML.SIMPLEX_TYPE:
                costs = calibrationRestartXML.getCalibrationRestartXMLChoice().getSimplex().getCostValue();
                break;
            case MethodNameXML.POWELL_TYPE:
                throw new IllegalArgumentException("Array of costs can not be retreived for a Powell algorithm");
        }
        return costs;
    }

    public double getCost() {

        double cost = Double.NaN;
        switch (methodNameXML.getType()) {
            case MethodNameXML.POWELL_TYPE:
                cost = calibrationRestartXML.getCalibrationRestartXMLChoice().getPowell().getCostValue();
                break;
            case MethodNameXML.DUD_TYPE:
            case MethodNameXML.SIMPLEX_TYPE:
                throw new IllegalArgumentException("Array of cost values a can only be retreived for a Powell algorithm");
        }
        return cost;
    }

	public void writeToFile(File file) {
		// check consistency.
		switch (methodNameXML.getType()) {
			case MethodNameXML.DUD_TYPE:
				DudXML dudXML = calibrationRestartXML.getCalibrationRestartXMLChoice().getDud();
				if (dudXML.getCostValueCount() != dudXML.getDudXMLChoiceCount() ||
						dudXML.getCostValueCount() != dudXML.getDudXMLChoice2Count() ||
						dudXML.getDudXMLChoiceCount() != dudXML.getDudXMLChoice2Count()) {
					throw new IllegalArgumentException("Inconsistent #parameters/costs/predictions");
				}
				if (dudXML.getCostValueCount() < 2) {
					throw new IllegalArgumentException("At least 2 parameter vectors are required");
				}
				break;
			case MethodNameXML.POWELL_TYPE:
				PowellXML powellXML = calibrationRestartXML.getCalibrationRestartXMLChoice().getPowell();
				if (powellXML.getPowellXMLChoice2Count() == 0) {
					throw new IllegalArgumentException("At least 1 search direction is required");
				}
				if (Double.compare(powellXML.getCostValue(), Double.NaN) == 0) {
					throw new IllegalArgumentException("Cost value not set");
				}
				break;
			case MethodNameXML.SIMPLEX_TYPE:
				SimplexXML simplexXML = calibrationRestartXML.getCalibrationRestartXMLChoice().getSimplex();
				if (simplexXML.getCostValueCount() != simplexXML.getSimplexXMLChoiceCount()) {
					throw new IllegalArgumentException("Inconsistent #parameters/costs");
				}
				if (simplexXML.getCostValueCount() < 2) {
					throw new IllegalArgumentException("At least 2 parameter vectors are required");
				}
				break;
		}
		CastorUtils.write(calibrationRestartXML, file, "calibrationRestart", null, null);
	}
}
