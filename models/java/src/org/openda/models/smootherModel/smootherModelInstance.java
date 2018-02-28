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
package org.openda.models.smootherModel;
import org.openda.interfaces.*;
import org.openda.utils.TreeVector;

import java.io.File;

/**
 * Created by IntelliJ IDEA.
 * User: nils
 * Date: 8/20/12
 * Time: 12:07 PM
 * To change this template use File | Settings | File Templates.
 */
public class smootherModelInstance implements IStochModelInstance {

	IStochModelInstance childModel=null;


	public smootherModelInstance(IStochModelInstance child){
		childModel=child;
	}

	public IVector getState() {

		TreeVector x0 =new TreeVector("state","state",childModel.getState());
		TreeVector x1 =new TreeVector("param","param",childModel.getParameters());

	    TreeVector x =new TreeVector("smoothState","smoothState");
		x.addChild(x0);
		x.addChild(x1);
		return x;
	}

	
	public void axpyOnState(double alpha, IVector vector) {

		ITreeVector x= (ITreeVector) vector;

		childModel.axpyOnState(alpha, (IVector) x.getSubTreeVector("state"));
		childModel.axpyOnParameters(alpha, x.getSubTreeVector("param"));
	}

	
	public IVector getParameters() {
		return null;  //To change body of implemented methods use File | Settings | File Templates.
	}

	
	public void setParameters(IVector parameters) {
		//To change body of implemented methods use File | Settings | File Templates.
	}

	
	public void axpyOnParameters(double alpha, IVector vector) {
		childModel.axpyOnParameters(alpha, vector);
		//To change body of implemented methods use File | Settings | File Templates.
	}

	
	public IStochVector getStateUncertainty() {
		return null;  //To change body of implemented methods use File | Settings | File Templates.
	}

	
	public IStochVector getParameterUncertainty() {
		return childModel.getParameterUncertainty();
	}

	
	public IStochVector[] getWhiteNoiseUncertainty(ITime time) {
		return new IStochVector[0];  //To change body of implemented methods use File | Settings | File Templates.
	}

	
	public boolean isWhiteNoiseStationary() {
		return false;  //To change body of implemented methods use File | Settings | File Templates.
	}

	
	public ITime[] getWhiteNoiseTimes(ITime timeSpan) {
		return new ITime[0];  //To change body of implemented methods use File | Settings | File Templates.
	}

	
	public IVector[] getWhiteNoise(ITime timeSpan) {
		return new IVector[0];  //To change body of implemented methods use File | Settings | File Templates.
	}

	
	public void setWhiteNoise(IVector[] whiteNoise) {
		//To change body of implemented methods use File | Settings | File Templates.
	}

	
	public void axpyOnWhiteNoise(double alpha, IVector[] vector) {
		//To change body of implemented methods use File | Settings | File Templates.
	}

	
	public void setAutomaticNoiseGeneration(boolean value) {
        childModel.setAutomaticNoiseGeneration(value);
		//To change body of implemented methods use File | Settings | File Templates.
	}

	
	public IVector getObservedValues(IObservationDescriptions observationDescriptions) {
        return childModel.getObservedValues(observationDescriptions);
	}

	
	public void announceObservedValues(IObservationDescriptions observationDescriptions) {
		childModel.announceObservedValues(observationDescriptions);
		//To change body of implemented methods use File | Settings | File Templates.
	}

	
	public IVector getStateScaling() {
		return null;  //To change body of implemented methods use File | Settings | File Templates.
	}

	
	public IVector[] getStateScaling(IObservationDescriptions observationDescriptions) {
		return new IVector[0];  //To change body of implemented methods use File | Settings | File Templates.
	}

	
	public IPrevExchangeItem getExchangeItem(String exchangeItemID) {
		return null;  //To change body of implemented methods use File | Settings | File Templates.
	}

	
	public ITime getTimeHorizon() {
		return childModel.getTimeHorizon();
	}

	
	public ITime getCurrentTime() {

		return childModel.getCurrentTime();
	}

	
	public void compute(ITime targetTime) {
		IStochVector pu=childModel.getParameterUncertainty();
		IVector noise= pu.createRealization();
		childModel.axpyOnParameters(0.05, noise);
		childModel.compute(targetTime);
	}

	
	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance) {
		return new IVector[0];  //To change body of implemented methods use File | Settings | File Templates.
	}

	
	public IModelState saveInternalState() {
		return null;  //To change body of implemented methods use File | Settings | File Templates.
	}

	
	public void restoreInternalState(IModelState savedInternalState) {
		//To change body of implemented methods use File | Settings | File Templates.
	}

	
	public void releaseInternalState(IModelState savedInternalState) {
		//To change body of implemented methods use File | Settings | File Templates.
	}

	
	public IModelState loadPersistentState(File persistentStateFile) {
		return null;  //To change body of implemented methods use File | Settings | File Templates.
	}

	
	public File getModelRunDir() {
		return null;  //To change body of implemented methods use File | Settings | File Templates.
	}

	
	public String[] getExchangeItemIDs() {
		return new String[0];  //To change body of implemented methods use File | Settings | File Templates.
	}

	
	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		return new String[0];  //To change body of implemented methods use File | Settings | File Templates.
	}

	
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return null;  //To change body of implemented methods use File | Settings | File Templates.
	}

	
	public void finish() {
        childModel.finish();
		//To change body of implemented methods use File | Settings | File Templates.
	}

	
	public void initialize(File workingDir, String[] arguments) {
       childModel.initialize(workingDir,arguments);
	}

	
	public IInstance getParent() {
		return null;  //To change body of implemented methods use File | Settings | File Templates.
	}
}
