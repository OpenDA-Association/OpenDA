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
package org.openda.models.simultaneousGroupModel;

/**
* The oscillator model
*
* simple linear oscilator (e.g. mass-spring system with friction)
* d(x)/d(t) = u
* d(u)/d(t) = - omega^2 * x - (2/t_damp) u
*/

import org.openda.interfaces.*;
import org.openda.interfaces.IPrevExchangeItem.Role;
import org.openda.observers.GroupObservationDesrciptions;
import org.openda.utils.Instance;
import org.openda.utils.StochVector;
import org.openda.utils.Time;
import org.openda.utils.TreeVector;

import java.io.File;
import java.util.ArrayList;
import java.util.List;



/**
 * Realisation of a Stochastic model.
 */
public class SimultaneousGroupStochModelInstance extends Instance implements IStochModelInstance {

    // Counter for keeping track of instances
    private static int NextinstanceNumber = 1;
    private int thisInstanceNumber=0;
    // some model parts like states, parameters etc, can be combined in several ways
    // IDENTICAL - assumes identical vectors. Throws an exception on a mismatch
    // CONCAT    - assumes independent values for submodels. Values are concatenated
    // NAMED     - assumes a TreeVector with identical ids for identical parts
    // AUTOMATIC - try to make an educated guess from the previous types
    private enum combineType { IDENTICAL, CONCAT}; //, NAMED, AUTOMATIC };
    private combineType parameterCombineType = combineType.IDENTICAL;
    private combineType stateCombineType = combineType.CONCAT;
    private final static double eps = 1e-6; //maximum acceptable difference to be seen as IDENTICAL

    // Configuration
    File workingDir=null;
    String configString =null;

	ArrayList<IStochModelInstance> children = new ArrayList<IStochModelInstance>();
	ArrayList<String> childIds = new ArrayList<String>();

	ITime currentTime = null;

	public SimultaneousGroupStochModelInstance(){
		this.thisInstanceNumber=NextinstanceNumber;
		NextinstanceNumber++;
	}

	public SimultaneousGroupStochModelInstance(List<IStochModelInstance> children, List<String> partIds){
		// no cloning is used because StochObservers are not usually modified in place
		for(int i=0;i<children.size();i++){
			this.children.add(children.get(i));
			this.childIds.add(partIds.get(i));
		}
		this.thisInstanceNumber=NextinstanceNumber;
		NextinstanceNumber++;
		this.initializeCurrentTime();
	}

	public SimultaneousGroupStochModelInstance(IStochModelInstance children[], String partIds[]){
		// no cloning is used because StochObservers are not usually modified in place
		for(int i=0;i<children.length;i++){
			this.children.add(children[i]);
			this.childIds.add(partIds[i]);
		}
		this.thisInstanceNumber=NextinstanceNumber;
		NextinstanceNumber++;
		this.initializeCurrentTime();
	}

	/*
	 *  Querying an existing Group
	 */

    public void initialize(File workingDir, String[] arguments) {
        // no action needed (handled by constructors)
    }


	public String[] getIds(){
		String[] result = this.childIds.toArray(new String[2]);
		return result;
	}

	public IStochModelInstance getChild(String id){
		IStochModelInstance result=null;
		int index = this.getIndexFromId(id);
		if(index>=0){ // expect -1 if Id was not found
			result = this.children.get(index);
		}
		return result;
	}

	public IStochModelInstance getChild(int index){
		IStochModelInstance result=null;
		if((index>=0)&(index<=this.children.size())){
			result = this.children.get(index);
		}
		return result;
	}

	@Override
	public void announceObservedValues(
			IObservationDescriptions observationDescriptions) {
		// split descriptions and pair models vs descriptions
		int nPart = this.childIds.size();
		if(!(observationDescriptions instanceof GroupObservationDesrciptions)){
			throw new RuntimeException("Observations need to be grouped with a GroupStochObserver");
		}
		for(int i=0;i<nPart;i++){
			String id = this.childIds.get(i);
			IObservationDescriptions obsPart = ((GroupObservationDesrciptions)observationDescriptions).getChild(id);
			this.children.get(i).announceObservedValues(obsPart);
		}
	}

	@Override
	public void axpyOnParameters(double alpha, IVector change) {
		IVector currentPars = this.getParameters();
		currentPars.axpy(alpha, change);
		this.setParameters(currentPars);
	}

	@Override
	public void axpyOnState(double alpha, IVector change) {
		int n=this.childIds.size();
		IVector[] stateParts = new IVector[n];
		for(int i=0;i<n;i++){
			stateParts[i] = this.children.get(i).getState();
		}
		IVector[] parts = splitVectors(this.childIds, change, this.stateCombineType, stateParts);
		for(int i=0;i<n;i++){ // forward axpy to each of the models
			this.children.get(i).axpyOnState(alpha,parts[i]);
		}
	}

	@Override
	public void axpyOnWhiteNoise(double alpha, IVector[] vector) {
		// TODO Auto-generated method stub

	}

	@Override
	public IVector getObservedValues(
			IObservationDescriptions observationDescriptions) {
		TreeVector result = new TreeVector("combined");
		// split descriptions and pair models vs descriptions
		int nPart = this.childIds.size();
		if(!(observationDescriptions instanceof GroupObservationDesrciptions)){
			throw new RuntimeException("Observations need to be grouped with a GroupStochObserver");
		}
		for(int i=0;i<nPart;i++){
			String id = this.childIds.get(i);
			IObservationDescriptions obsPart = ((GroupObservationDesrciptions)observationDescriptions).getChild(id);
			IVector prdPart = this.children.get(i).getObservedValues(obsPart);
			ITreeVector prdTreePart = new TreeVector(id,prdPart);
			result.addChild(prdTreePart);
		}
		return result;
	}

	@Override
	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance){
		throw new UnsupportedOperationException("org.openda.models.simpleModel.simultaneousGroupStochModelInstance.getObservedLocalization(): Not implemented yet.");
	}

	@Override
	public IStochVector getParameterUncertainty() {
		IStochVector result = null;
		IStochVector stochParts[] = new IStochVector[this.childIds.size()];
		IVector meanParts[] = new IVector[this.childIds.size()];
		IVector stdParts[] = new IVector[this.childIds.size()];
		for(int i=0;i<this.childIds.size();i++){
			stochParts[i] = this.children.get(i).getParameterUncertainty();
			if(stochParts[i].hasCorrelatedElements()){
				throw new RuntimeException("Grouping of parameterUncertainty was only implemented for independent parameters.");
			}
			meanParts[i]  = stochParts[i].getExpectations();
			stdParts[i]   = stochParts[i].getStandardDeviations();
		}
		IVector mean = combineVectors(this.childIds,meanParts,this.parameterCombineType);
		IVector std = combineVectors(this.childIds, stdParts,this.parameterCombineType);
		result = new StochVector(mean,std);
		return result;
	}

	@Override
	public IVector getParameters() {
		IVector result = null;
		IVector parts[] = new IVector[this.childIds.size()];
		for(int i=0;i<this.childIds.size();i++){
			parts[i] = this.children.get(i).getParameters();
		}
		result = combineVectors(this.childIds,parts,this.parameterCombineType);
		return result;
	}

	@Override
	public IVector getState() {
		IVector result = null;
		IVector parts[] = new IVector[this.childIds.size()];
		for(int i=0;i<this.childIds.size();i++){
			parts[i] = this.children.get(i).getState();
		}
		result = combineVectors(this.childIds,parts,this.stateCombineType);
		return result;
	}

	@Override
	public IVector getStateScaling() {
		// TODO Auto-generated method stub
		return null;
	}
	@Override
	public IVector[] getStateScaling(
			IObservationDescriptions observationDescriptions) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public IStochVector getStateUncertainty() {
		IStochVector result = null;
		IStochVector stochParts[] = new IStochVector[this.childIds.size()];
		IVector meanParts[] = new IVector[this.childIds.size()];
		IVector stdParts[] = new IVector[this.childIds.size()];
		for(int i=0;i<this.childIds.size();i++){
			stochParts[i] = this.children.get(i).getStateUncertainty();
			if(stochParts[i].hasCorrelatedElements()){
				throw new RuntimeException("Grouping of stateUncertainty was only implemented for independent parameters.");
			}
			meanParts[i]  = stochParts[i].getExpectations();
			stdParts[i]   = stochParts[i].getStandardDeviations();
		}
		IVector mean = combineVectors(this.childIds,meanParts,this.stateCombineType);
		IVector std = combineVectors(this.childIds, stdParts,this.stateCombineType);
		result = new StochVector(mean,std);
		return result;
	}

	@Override
	public IVector[] getWhiteNoise(ITime timeSpan) {
		throw new UnsupportedOperationException("org.openda.models.simultaneousGroupModel.SimultaneousGroupStochModelInstance.getWhiteNoise: Not implemented yet.");
	}
	@Override
	public ITime[] getWhiteNoiseTimes(ITime timeSpan) {
		throw new UnsupportedOperationException("org.openda.models.simultaneousGroupModel.SimultaneousGroupStochModelInstance.getWhiteNoiseTimes(ITime timeSpan): Not implemented yet.");
	}
	@Override
	public IStochVector[] getWhiteNoiseUncertainty(ITime time) {
		throw new UnsupportedOperationException("org.openda.models.simultaneousGroupModel.SimultaneousGroupStochModelInstance.getWhiteNoiseUncertainty(ITime time): Not implemented yet.");
	}
	@Override
	public boolean isWhiteNoiseStationary() {
		throw new UnsupportedOperationException("org.openda.models.simultaneousGroupModel.SimultaneousGroupStochModelInstance.isWhiteNoiseStationary(): Not implemented yet.");
	}
	@Override
	public void setAutomaticNoiseGeneration(boolean value) {
		int n=this.childIds.size();
		for(int i=0;i<n;i++){
			this.children.get(i).setAutomaticNoiseGeneration(value);
		}
	}

	@Override
	public void setParameters(IVector parameters) {
		int n=this.childIds.size();
		IVector[] parParts = new IVector[n];
		for(int i=0;i<n;i++){
			parParts[i] = this.children.get(i).getParameters();
		}
		IVector[] parts = splitVectors(this.childIds, parameters, this.parameterCombineType, parParts);
		for(int i=0;i<n;i++){
			this.children.get(i).setParameters(parts[i]);
		}
	}

	@Override
	public void setWhiteNoise(IVector[] whiteNoise) {
		throw new UnsupportedOperationException("org.openda.models.simultaneousGroupModel.SimultaneousGroupStochModelInstance.setWhiteNoise(Vector[] whiteNoise): Not implemented yet.");
	}

	@Override
	public void compute(ITime targetTime) {
		ITime groupTime = this.getCurrentTime();
		if(targetTime.after(groupTime)){ //potentially there is something to do
			int n=this.childIds.size();
			for(int i=0;i<n;i++){
				ITime tempT = this.children.get(i).getTimeHorizon();
				ITime firstTime = tempT.getBeginTime();
				ITime lastTime = tempT.getEndTime();
				if(  (firstTime.beforeEquals(targetTime)) & (groupTime.beforeEquals(lastTime))  ){
					// is there really something to do for this model
					ITime thisTargetT = new Time(targetTime);
					if(targetTime.after(lastTime)){
						thisTargetT = new Time(lastTime);
					} // no need to compute after end
					this.children.get(i).compute(thisTargetT);
				}
			}
			this.currentTime = new Time(targetTime);
		}

	}

	@Override
	public ITime getCurrentTime() {
		ITime result = new Time(this.currentTime);
		return result;
	}

	/**
	 * Initialize the currentTime to the first time seen in the models
	 * use first time for the group, eg.
	 * model1.getCurrentTime() == 1.0
	 * model2.getCurrentTime() == 2.0
	 * then we set this.currentTime = 1.0
	 */
	private void initializeCurrentTime(){
		/*
		 * use first time for the group, eg.
		 * model1.getCurrentTime() == 1.0
		 * model2.getCurrentTime() == 2.0
		 * then we return 1.0
		 */
		ITime result = null;
		int n=this.childIds.size();
		if(n<1){
			throw new RuntimeException("This groupmodel has nog parts - problem in getCurrentTime");
		}
		ITime tempT = this.children.get(0).getCurrentTime();
		ITime firstTime = tempT;
		for(int i=1;i<n;i++){
			tempT = this.children.get(i).getCurrentTime();
			if(tempT.beforeEquals(firstTime)){
				firstTime = tempT;
			}
		}
		result = new Time(firstTime,firstTime);
		this.currentTime = result;

	}

	@Override
	public ITime getTimeHorizon() {
		ITime result = null;
		int n=this.childIds.size();
		if(n<1){
			throw new RuntimeException("This groupmodel has nog parts - problem in getTimeZorizon");
		}
		ITime tempT = this.children.get(0).getTimeHorizon().getBeginTime();
		ITime firstTime = tempT.getBeginTime();
		ITime lastTime = tempT.getEndTime();
		for(int i=1;i<n;i++){
			tempT = this.children.get(i).getTimeHorizon();
			if(tempT.getEndTime().after(lastTime)){
				lastTime = tempT.getEndTime();
			}
			if(tempT.getBeginTime().beforeEquals(firstTime)){
				firstTime = tempT.getBeginTime();
			}
		}
		result = new Time(firstTime,lastTime);
		return result;
	}


	@Override
	public IPrevExchangeItem getExchangeItem(String exchangeItemID) {
		throw new UnsupportedOperationException("org.openda.models.simultaneousGroupModel.SimultaneousGroupStochModelInstance.getExchangeItem(String exchangeItemID): Not implemented yet.");
	}
	@Override
	public String[] getExchangeItemIDs() {
		throw new UnsupportedOperationException("org.openda.models.simultaneousGroupModel.SimultaneousGroupStochModelInstance.getExchangeItemIDs(): Not implemented yet.");
	}
	@Override
	public String[] getExchangeItemIDs(Role role) {
		throw new UnsupportedOperationException("org.openda.models.simultaneousGroupModel.SimultaneousGroupStochModelInstance.getExchangeItemIDs(Role role): Not implemented yet.");
	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		throw new UnsupportedOperationException("org.openda.models.simultaneousGroupModel.SimultaneousGroupStochModelInstance.getDataObjectExchangeItem(): Not implemented yet.");
	}

	@Override
	public File getModelRunDir() {
		return null;
	}

    @Override
	public void finish() {
		// no action needed (yet)
	}

	@Override
	public void releaseInternalState(IModelState savedInternalState) {
		throw new UnsupportedOperationException("org.openda.models.simultaneousGroupModel.SimultaneousGroupStochModelInstance.releaseInternalState(Object savedInternalState) : Not implemented yet.");
	}

	public IModelState loadPersistentState(File persistentStateFile) {
		throw new UnsupportedOperationException("org.openda.models.simultaneousGroupModel.SimultaneousGroupStochModelInstance.loadPersistentState(): Not implemented yet.");
	}

	@Override
	public void restoreInternalState(IModelState savedInternalState) {
		throw new UnsupportedOperationException("org.openda.models.simultaneousGroupModel.SimultaneousGroupStochModelInstance.restoreInternalState(Object savedInternalState): Not implemented yet.");
	}
	@Override
	public IModelState saveInternalState() {
		throw new UnsupportedOperationException("org.openda.models.simultaneousGroupModel.SimultaneousGroupStochModelInstance.saveInternalState(): Not implemented yet.");
	}

	public String toString(){
		String result = "SimultaneousGroupStochmodelInstance{\n";
		for(int i=0;i<this.childIds.size();i++){
			result += "  "+this.childIds.get(i)+" => "+this.children.get(i).toString()+"\n";
		}
		result += "}\n";
		return result;
	}



	/*
	 *  Helper methods
	 */

	private int getIndexFromId(String Id){
		int result = -1;
		for(int i=0;i<this.childIds.size();i++){
			if(this.childIds.get(i).equalsIgnoreCase(Id)){
				result=i;
				break;
			}
		}
		return result;
	}


	/* =====================================================================================================
	 *
	 * Internal support routines
	 *
	 * =====================================================================================================
	 */

	/**
	 * Check that two vectors are equal
	 * @return string==null if the vectors are equal, else it contains a message with the differences found
	 */
	private static String checkIdentical(IVector v1, IVector v2){
		String result = null;
		if(v1.getSize()!=v2.getSize()){
			result = "Vectors do not have equal lengths v1.getSize()="+v1.getSize()+"v2.getSize()="+v2.getSize();
		}else {
			// check norm of difference
			IVector diff = v1.clone();
			diff.axpy(-1.0, v2);
			double normDiff = diff.norm2();
			if(normDiff>eps){
				if(v1.getSize()<8){
					result = "Vectors are different v1="+v1.toString()+" v2="+v2.toString();
				}else{
					result = "Vectors are different |v1-v2|="+normDiff;
				}
			}
		}
		return result;
	}

	/**
	 * Concatenate several vectors into a TreeVector
	 * @param ids labels for the parts
	 * @param parts subvectors
	 * @return concatenated TreeVector
	 */
	private static IVector concatVectors(List<String> ids, IVector[] parts){
		TreeVector result= new TreeVector("combined_vector");
		for(int i=0;i<ids.size();i++){
			TreeVector part=new TreeVector(ids.get(i),parts[i]);
			result.addChild(part);
		}
		return result;
	}

	/**
	 * Combine vectors that should be identical, i.e. make a copy of the first part and check
	 * that the other parts are identical
	 * @param ids - not used
	 * @param parts - subvectors to use
	 * @return
	 */
	private static IVector identicalGetVectors(List<String> ids, IVector[] parts){
		IVector result=null;
		for(int i=0;i<ids.size();i++){
			IVector part = parts[i];
			if(result==null){
				result=part.clone();
			}else{
				String ident = checkIdentical(part,result);
				if(ident!=null){
					throw new RuntimeException(ident);
				}
			}
		}
		return result;
	}

	/**
	 * For identical combined vectors each part is just a copy of the combined vector
	 * @param ids not used
	 * @param combinedVector source
	 * @return several copied of the combined vector
	 */
	private static IVector[] identicalPutVectors(List<String> ids, IVector combinedVector, IVector[] splitReference){
		int n=ids.size();
		IVector[] result = new IVector[n];
		for(int i=0;i<n;i++){
			result[i]=combinedVector.clone();
		}
		return result;
	}

	private static IVector[] unconcatVectors(List<String> ids, IVector combinedVector, IVector[] splitReference){
		int n=ids.size();
		IVector[] result = new IVector[n];
		if(combinedVector instanceof ITreeVector){ //can we extract by id?
			for(int i=0;i<n;i++){
				result[i]=((ITreeVector)combinedVector).getSubTreeVector(ids.get(i));
			}
		}else{
			int combinedLength = combinedVector.getSize();
			int sumSizes = 0;
			int nextIndex=0;
			for(int i=0;i<n;i++){
				int nPart = splitReference[i].getSize();
				result[i]= splitReference[i].clone();
				sumSizes +=splitReference[i].getSize();
				if(sumSizes>combinedLength){
					throw new RuntimeException("There are too few values in the combined vector");
				}
				for(int j=0;j<nPart;j++){
					result[i].setValue(j, combinedVector.getValue(nextIndex));
					nextIndex++;
				}
			}
		}
		return result;
	}


	/**
	 * Create a combination of a list of vectors. The precise actions depend on the type of combination and
	 * the type of Vector
	 * @param ids label to use eg. for concatenation into a treeVector
	 * @param parts subvectors to combine
	 * @param vectorCombineType choice for type of combine action
	 * @return
	 */
	private static IVector combineVectors(List<String> ids, IVector[] parts,combineType vectorCombineType){
		IVector result=null;
		switch (vectorCombineType) {
		case IDENTICAL:
			result = identicalGetVectors(ids, parts);
			break;
		case CONCAT:
			result = concatVectors(ids,parts);
			break;
		default:
			throw new RuntimeException("Error combining state vectors: type not implemented "+vectorCombineType);
		}
		return result;
	}

	/**
	 * Split a combined vector into parts. The precise actions depend on the selected combineType
	 * @param ids - labels used for extraction
	 * @param combinedVector - combined vector, used as source for copies
	 * @param vectorCombineType - type of actions
	 * @param splitReference vector used for lookup of metadata during split operation
	 * @return subvectors
	 */
	private static IVector[] splitVectors(List<String> ids, IVector combinedVector,combineType vectorCombineType,
			IVector[] splitReference){
		int n=ids.size();
		IVector result[]=null;
		switch (vectorCombineType) {
		case IDENTICAL:
			result = identicalPutVectors(ids, combinedVector, splitReference);
			break;
		case CONCAT:
			result = unconcatVectors(ids,combinedVector, splitReference);
			break;
		default:
			throw new RuntimeException("Error combining state vectors: type not implemented "+vectorCombineType);
		}
		return result;
	}
}
