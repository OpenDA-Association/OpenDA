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
package org.openda.models.simultaneousGroupModel;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.IStochModelPostProcessor;
import org.openda.utils.ConfigTree;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

/* ================================================================
 * OpenDa interfaces
 * ================================================================
 *
 * Project Info:  http://www.openda.org
 *
 * ----------------------------------------------------------------
 */




/**
 * This factory can create instances of a model that groups several models together. The main purpose 
 * is to represent multiple models as one, e.g. for calibration of multiple scenarios at once.
 */
public class SimultaneousGroupStochModelFactory implements IStochModelFactory {
	protected File workingDir=null;
	protected String[] arguments=null;

	ArrayList<IStochModelFactory> children = new ArrayList<IStochModelFactory>();
	ArrayList<String> childIds = new ArrayList<String>();

	public SimultaneousGroupStochModelFactory(){
	}

	public SimultaneousGroupStochModelFactory(List<IStochModelFactory> children, List<String> partIds){
		// no cloning is used because StochObservers are not usually modified in place
		for(int i=0;i<children.size();i++){
			this.children.add(children.get(i));
			this.childIds.add(partIds.get(i));
		}
	}

	public SimultaneousGroupStochModelFactory(IStochModelFactory children[], String partIds[]){
		// no cloning is used because StochObservers are not usually modified in place
		for(int i=0;i<children.length;i++){
			this.children.add(children[i]);
			this.childIds.add(partIds[i]);
		}
	}

	/*
	 *  Querying an existing Group
	 */
	
	
	public String[] getIds(){
		String[] result = this.childIds.toArray(new String[2]);
		return result;
	}
	
	public IStochModelFactory getChild(String id){
		IStochModelFactory result=null;
		int index = this.getIndexFromId(id);
		if(index>=0){ // expect -1 if Id was not found
			result = this.children.get(index);
		}
		return result;
	}
	
	public IStochModelFactory getChild(int index){
		IStochModelFactory result=null;
		if((index>=0)&(index<=this.children.size())){
			result = this.children.get(index);
		}
		return result;
	}
	
	
    /**
     * Initialize the factory. 
     * @param workingDir The working directory for the Stoch Model Factory
     * @param arguments One string, either with xml or with the filename of an xml-file
     * For a description of the format, see the simpleStochModelInstance
     */
    public void initialize(File workingDir, String[] arguments) {
       this.workingDir = workingDir;
       this.arguments = arguments;
		/* initialize from xml with sample format like:
		 * <?xml version="1.0" encoding="UTF-8"?>
		 * <stochModelFactory>	   
		 *    <stochModelFactory id="oscillator1" className="org.openda.models.simpleModel.SimpleOscillatorStochModelFactory">
		 *       <workingDirectory>.</workingDirectory>
		 *       <configFile>oscillator1.xml</configFile>
		 *    </stochModelFactory>
		 *    <stochModelFactory id="oscillator2" className="org.openda.models.simpleModel.SimpleOscillatorStochModelFactory">
		 *       <workingDirectory>.</workingDirectory>
		 *       <configFile>oscillator2.xml</configFile>
		 *    </stochModelFactory>
		 * </stochModelFactory>
		 */
		// stop when initialize non-empty stochmodelFactory
		if(this.childIds.size()>0){
			String info="";
			if(arguments.length>0){
				info = arguments[0];
			}
			throw new RuntimeException("SimultaneousGroupStochModelFactory: attempt to initialize more than once: "
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
			throw new RuntimeException("SimultaneousGroupStochModelFactory: Could not find file "+file.getAbsolutePath()
					+" and "+arguments[0]+" is no xml.");
		}
		ConfigTree seriesTrees[] = config.getSubTrees("stochModelFactory");
		for(int i=0;i<seriesTrees.length;i++){
			// read content
			String workingDirectoryName = seriesTrees[i].getContentString("workingDirectory");
			String fileNamePart = seriesTrees[i].getContentString("configFile");
			File extendedWorkingDir = null;
			File seriesFile = null;
			if(fileNamePart==null){ //try content directly as arg[0]
				ConfigTree configTree = seriesTrees[i].getFirstChild();
				if(configTree==null){
					throw new RuntimeException("No configuration found for model \n"+seriesTrees[i].toString());
				}
				fileNamePart = configTree.toString();
			}else{
				//prepare input strings
				workingDirectoryName=workingDirectoryName.trim();
				fileNamePart=fileNamePart.trim();
				// connect parts of workingDir
				extendedWorkingDir = new File(workingDir,workingDirectoryName);
				seriesFile = new File(extendedWorkingDir,fileNamePart);
				if(!seriesFile.exists()){
					throw new RuntimeException("SimultaneousGroupStochModelFactory: file for part "+i+" with filename "
							+seriesFile.getAbsolutePath()+" does not exist");
				}
			}
			//create StochModelFactory for this part
			String className = seriesTrees[i].getAsString("@className", "");

			String messageString =  "part of SimultaneousGroupStochModelFactory" +
			"\n\tclassName: " + className +
			"\n\tdir.: " + extendedWorkingDir +
			"\n\tconfig.: " + fileNamePart;
			Object partModelObject=null;
			try {
				Class aClass = Class.forName(className);
				partModelObject = aClass.newInstance();
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
			if (!(partModelObject instanceof IStochModelFactory)) {
				throw new RuntimeException(partModelObject.getClass().getName() +
						" is not implementing the StochModelFactory interface");
			}
			IStochModelFactory partModel = (IStochModelFactory)partModelObject;
			String args[] = {null}; args[0]= fileNamePart;
			partModel.initialize(extendedWorkingDir,args);
			// push to grouplist
			String id = seriesTrees[i].getAsString("@id", "part"+i);
			this.children.add(partModel);
			this.childIds.add(id);			
		}
    }

    /**
     * Abstract method to create a new instance. Each model that uses this class should override
     * with something like
     * return new MymodelStochModelInstance(this.workingDir, this.configString);
     * here.
	 * @param outputLevel
	 */
    public IStochModelInstance getInstance(OutputLevel outputLevel){
		ArrayList<String> ids = new ArrayList<String>();
		ArrayList<IStochModelInstance> models = new ArrayList<IStochModelInstance>();
		for(int i=0;i<this.childIds.size();i++){
			ids.add(this.childIds.get(i));
			IStochModelInstance tempInstance = this.children.get(i).getInstance(OutputLevel.Suppress);
			models.add(tempInstance);
		}
		IStochModelInstance result = new SimultaneousGroupStochModelInstance(models,ids);
		return result;
    }

	
	public IStochModelPostProcessor getPostprocessorInstance(File instanceDir) {
		// TODO Auto-generated method stub
		return null;
	}

	
	public void finish() {
		// no action needed (yet)
	}

	public String toString(){
		String result = "SimultaneousGroupStochmodelFactory{\n";
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
	

}
