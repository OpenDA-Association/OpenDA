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
import org.openda.interfaces.*;
import org.openda.utils.ConfigTree;
import org.openda.utils.IdentitySelector;
import org.openda.utils.Instance;
import org.openda.utils.TreeVector;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class GroupStochObserver extends Instance implements IStochObserver {
	ArrayList<IStochObserver> children = new ArrayList<IStochObserver>();
	ArrayList<String> childIds = new ArrayList<String>();

	public GroupStochObserver(){
	}

	public GroupStochObserver(List<IStochObserver> children, List<String> partIds){
		// no cloning is used because StochObservers are not usually modified in place
		for(int i=0;i<children.size();i++){
			this.children.add(children.get(i));
			this.childIds.add(partIds.get(i));
		}
	}

	public GroupStochObserver(IStochObserver children[], String partIds[]){
		// no cloning is used because StochObservers are not usually modified in place
		for(int i=0;i<children.length;i++){
			this.children.add(children[i]);
			this.childIds.add(partIds[i]);
		}
	}

	@Override
	public void initialize(File workingDir, String[] arguments) {
		/* initialize from xml with sample format like:
		 * <stochObserver id="set1" className="org.openda.observers.NoosTimeSeriesStochObserver">
		 *       <workingDirectory>./stochobserver</workingDirectory>
		 *       <configFile>noosGeneratedObservations.xml</configFile>
		 * </stochObserver>
		 * <stochObserver id="set2" className="org.openda.observers.NoosTimeSeriesStochObserver">
		 *       <workingDirectory>./stochobserver</workingDirectory>
		 *       <configFile>noosGeneratedObservations2.xml</configFile>
		 * </stochObserver>
		 */
		// stop when initialize non-empty observer
		if(this.childIds.size()>0){
			String info="";
			if(arguments.length>0){
				info = arguments[0];
			}
			throw new RuntimeException("GroupStochObserver: attempt to initialize more than once: "
					+info);
		}
		// check for config file
        String fileName = arguments[0];
        File file = new File(workingDir,fileName);
		// read config file
		ConfigTree config = null;
		if(file.exists()|arguments[0].contains("<")){
			config = new ConfigTree(workingDir,fileName);
		}else{
			throw new RuntimeException("GroupStochObserver: Could not find file "+file.getAbsolutePath()
					+" and "+arguments[0]+" is no xml.");
		}
		ConfigTree seriesTrees[] = config.getSubTrees("stochObserver");
		for(int i=0;i<seriesTrees.length;i++){
			// read content
			String workingDirectoryName = seriesTrees[i].getContentString("workingDirectory");
			String fileNamePart = seriesTrees[i].getContentString("configFile");
			File extendedWorkingDir = null;
			File seriesFile = null;
			if(fileNamePart==null){ //try content directly as arg[0]
				fileNamePart = seriesTrees[i].getContentString("");
			}else{
				//prepare input strings
				workingDirectoryName=workingDirectoryName.trim();
				fileNamePart=fileNamePart.trim();
				// connect parts of workingDir
				extendedWorkingDir = new File(workingDir,workingDirectoryName);
				seriesFile = new File(extendedWorkingDir,fileNamePart);
				if(!seriesFile.exists()){
					throw new RuntimeException("GroupStochobserver: file for part "+i+" with filename "
							+seriesFile.getAbsolutePath()+" does not exist");
				}
			}
			//create CsvStochObserver for this part
			String className = seriesTrees[i].getAsString("@className", "");

			String messageString =  "part of GroupStochObserver" +
			"\n\tclassName: " + className +
			"\n\tdir.: " + extendedWorkingDir +
			"\n\tconfig.: " + fileNamePart;
			Object partObserverObject=null;
			try {
				Class aClass = Class.forName(className);
				partObserverObject = aClass.newInstance();
			} catch (ClassNotFoundException e) {
				throw new RuntimeException("ClassNotFoundException creating " + messageString);
			} catch (InstantiationException e) {
				throw new RuntimeException("InstantiationException creating " + messageString);
			} catch (IllegalAccessException e) {
				throw new RuntimeException("IllegalAccessException creating " + messageString);
			} catch (RuntimeException e) {
				throw new RuntimeException("Got RuntimeException\n\t" + e.getMessage() +
						"\nwhen creating " + messageString);
			}
			if (!(partObserverObject instanceof IStochObserver)) {
				throw new RuntimeException(partObserverObject.getClass().getName() +
						" is not implementing the IStochObserver interface");
			}
			IStochObserver partObserver = (IStochObserver)partObserverObject;
			String args[] = {null}; args[0]= fileNamePart;
			partObserver.initialize(extendedWorkingDir,args);
			// push to grouplist
			String id = seriesTrees[i].getAsString("@id", "part"+i);
			this.children.add(partObserver);
			this.childIds.add(id);			
		}

	}
	
	/*
	 *  Querying an existing Group
	 */
	
	
	public String[] getIds(){
		String[] result = this.childIds.toArray(new String[2]);
		return result;
	}
	
	public IStochObserver getChild(String id){
		IStochObserver result=null;
		int index = this.getIndexFromId(id);
		if(index>=0){ // expect -1 if Id was not found
			result = this.children.get(index);
		}
		return result;
	}
	
	public IStochObserver getChild(int index){
		IStochObserver result=null;
		if((index>=0)&(index<=this.children.size())){
			result = this.children.get(index);
		}
		return result;
	}
	
	

	/*
	 * Regular CsvStochObserver methods
	 * 
	 * These all use their children to get the work done
	 * 
	 */
	
	@Override
	public IStochObserver createSelection(String selection) {
		throw new RuntimeException("GroupStochObserver: method not implemented yet, createSelection");
	}

	@Override
	public IStochObserver createSelection(ITime selectionTimes) {
		IStochObserver result = null;
		ArrayList<IStochObserver> selectionParts = new ArrayList<IStochObserver>();
		ArrayList<String> copyIds = new ArrayList<String>();
		for(int i=0;i<children.size();i++){
			IStochObserver part = this.children.get(i).createSelection(selectionTimes);
			selectionParts.add(part);
			copyIds.add(this.childIds.get(i));
		}
		result = new GroupStochObserver(selectionParts,copyIds);
		return result;
	}

	public IStochObserver createSelection(Type observationType) {
		if (observationType == Type.Assimilation) {
			return this;
		}
		throw new UnsupportedOperationException("org.openda.observers.GroupStochObserver.createSelection(): Not implemented yet.");
	}

    public ISelector createSelector(Type observationType) {
        return new IdentitySelector();        
    }

    @Override
	public IVector evaluateMarginalPDFs(IVector values) {
		throw new RuntimeException("GroupStochObserver: method not implemented yet, evaluatePDF");
	}

	@Override
	public double evaluatePDF(IVector values) {
		throw new RuntimeException("GroupStochObserver: method not implemented yet, evaluatePDF");
	}

	@Override
	public void free() {
		for (IStochObserver childStochObserver : this.children){
			childStochObserver.free();
		}
	}

	@Override
	public int getCount() {
		int result=0;
		for(int i=0;i<this.children.size();i++){
			result+=this.children.get(i).getCount();
		}
		return result;
	}

	@Override
	public IVector getExpectations(){
		TreeVector result = new TreeVector("combined");
		IVector part = null;
		for(int i=0;i<this.childIds.size();i++){
			String id=this.childIds.get(i);
			part = this.children.get(i).getExpectations();
			TreeVector treePart = new TreeVector(id,part);
			result.addChild(treePart);
		}
		return result;
	}

	@Override
	public IObservationDescriptions getObservationDescriptions() {
		GroupObservationDesrciptions result = new GroupObservationDesrciptions(this);
		return result;
	}

	@Override
	public IVector getRealizations() {
		throw new RuntimeException("GroupStochObserver: method not implemented yet, getRealizations");
	}

	@Override
	public ISqrtCovariance getSqrtCovariance() {
		throw new RuntimeException("GroupStochObserver: method not implemented yet, getSqrtCovariance");
	}

	@Override
	public IVector getStandardDeviations() {
		TreeVector result = new TreeVector("combined");
		IVector part = null;
		for(int i=0;i<this.childIds.size();i++){
			String id=this.childIds.get(i);
			part = this.children.get(i).getStandardDeviations();
			TreeVector treePart = new TreeVector(id,part);
			result.addChild(treePart);
		}
		return result;
	}

	@Override
	public ITime[] getTimes() {
		throw new RuntimeException("GroupStochObserver: method not implemented yet, getTimes");
	}

	@Override
	public IVector getValues() {
		TreeVector result = new TreeVector("combined");
		IVector part = null;
		for(int i=0;i<this.childIds.size();i++){
			String id=this.childIds.get(i);
			part = this.children.get(i).getValues();
			TreeVector treePart = new TreeVector(id,part);
			result.addChild(treePart);
		}
		return result;
	}
	
	
	public String toString(){
		String result = "GroupStochObserver{\n";
		for(int i=0;i<this.childIds.size();i++){
			result += "  "+this.childIds.get(i)+" => "+this.children.get(i).toString()+"\n";
		}
		result += "}\n";
		return result;
	}
	
	/*
	 *  Helper methods
	 */
	
	public int getIndexFromId(String Id){
		int result = -1;
		for(int i=0;i<this.childIds.size();i++){
			if(this.childIds.get(i).equalsIgnoreCase(Id)){
				result=i;
				break;
			}
		}
		return result;
	}
	

}
