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

	@Override
	public void axpyOnState(double alpha, IVector vector) {

		ITreeVector x= (ITreeVector) vector;

		childModel.axpyOnState(alpha, (IVector) x.getSubTreeVector("state"));
		childModel.axpyOnParameters(alpha, x.getSubTreeVector("param"));
	}

	@Override
	public IVector getParameters() {
		return null;  //To change body of implemented methods use File | Settings | File Templates.
	}

	@Override
	public void setParameters(IVector parameters) {
		//To change body of implemented methods use File | Settings | File Templates.
	}

	@Override
	public void axpyOnParameters(double alpha, IVector vector) {
		childModel.axpyOnParameters(alpha, vector);
		//To change body of implemented methods use File | Settings | File Templates.
	}

	@Override
	public IStochVector getStateUncertainty() {
		return null;  //To change body of implemented methods use File | Settings | File Templates.
	}

	@Override
	public IStochVector getParameterUncertainty() {
		return childModel.getParameterUncertainty();
	}

	@Override
	public IStochVector[] getWhiteNoiseUncertainty(ITime time) {
		return new IStochVector[0];  //To change body of implemented methods use File | Settings | File Templates.
	}

	@Override
	public boolean isWhiteNoiseStationary() {
		return false;  //To change body of implemented methods use File | Settings | File Templates.
	}

	@Override
	public ITime[] getWhiteNoiseTimes(ITime timeSpan) {
		return new ITime[0];  //To change body of implemented methods use File | Settings | File Templates.
	}

	@Override
	public IVector[] getWhiteNoise(ITime timeSpan) {
		return new IVector[0];  //To change body of implemented methods use File | Settings | File Templates.
	}

	@Override
	public void setWhiteNoise(IVector[] whiteNoise) {
		//To change body of implemented methods use File | Settings | File Templates.
	}

	@Override
	public void axpyOnWhiteNoise(double alpha, IVector[] vector) {
		//To change body of implemented methods use File | Settings | File Templates.
	}

	@Override
	public void setAutomaticNoiseGeneration(boolean value) {
        childModel.setAutomaticNoiseGeneration(value);
		//To change body of implemented methods use File | Settings | File Templates.
	}

	@Override
	public IVector getObservedValues(IObservationDescriptions observationDescriptions) {
        return childModel.getObservedValues(observationDescriptions);
	}

	@Override
	public void announceObservedValues(IObservationDescriptions observationDescriptions) {
		childModel.announceObservedValues(observationDescriptions);
		//To change body of implemented methods use File | Settings | File Templates.
	}

	@Override
	public IVector getStateScaling() {
		return null;  //To change body of implemented methods use File | Settings | File Templates.
	}

	@Override
	public IVector[] getStateScaling(IObservationDescriptions observationDescriptions) {
		return new IVector[0];  //To change body of implemented methods use File | Settings | File Templates.
	}

	@Override
	public IPrevExchangeItem getExchangeItem(String exchangeItemID) {
		return null;  //To change body of implemented methods use File | Settings | File Templates.
	}

	@Override
	public ITime getTimeHorizon() {
		return childModel.getTimeHorizon();
	}

	@Override
	public ITime getCurrentTime() {

		return childModel.getCurrentTime();
	}

	@Override
	public void compute(ITime targetTime) {
		IStochVector pu=childModel.getParameterUncertainty();
		IVector noise= pu.createRealization();
		childModel.axpyOnParameters(0.05, noise);
		childModel.compute(targetTime);
	}

	@Override
	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance) {
		return new IVector[0];  //To change body of implemented methods use File | Settings | File Templates.
	}

	@Override
	public IModelState saveInternalState() {
		return null;  //To change body of implemented methods use File | Settings | File Templates.
	}

	@Override
	public void restoreInternalState(IModelState savedInternalState) {
		//To change body of implemented methods use File | Settings | File Templates.
	}

	@Override
	public void releaseInternalState(IModelState savedInternalState) {
		//To change body of implemented methods use File | Settings | File Templates.
	}

	@Override
	public IModelState loadPersistentState(File persistentStateFile) {
		return null;  //To change body of implemented methods use File | Settings | File Templates.
	}

	@Override
	public File getModelRunDir() {
		return null;  //To change body of implemented methods use File | Settings | File Templates.
	}

	@Override
	public String[] getExchangeItemIDs() {
		return new String[0];  //To change body of implemented methods use File | Settings | File Templates.
	}

	@Override
	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		return new String[0];  //To change body of implemented methods use File | Settings | File Templates.
	}

	@Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return null;  //To change body of implemented methods use File | Settings | File Templates.
	}

	@Override
	public void finish() {
        childModel.finish();
		//To change body of implemented methods use File | Settings | File Templates.
	}

	@Override
	public void initialize(File workingDir, String[] arguments) {
       childModel.initialize(workingDir,arguments);
	}

	@Override
	public IInstance getParent() {
		return null;  //To change body of implemented methods use File | Settings | File Templates.
	}
}
