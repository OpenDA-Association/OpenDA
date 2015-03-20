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
package org.openda.exchange.iotools;
import java.io.File;
import java.util.Set;
import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.Reflection;

/**
 * The IoObjectInterface is an important tool for creating file based connections to OpenDA. 
 * This tool can copy the contents of an ioObject to another ioObject. 
 * This can eg be used for format conversion.
 * @author verlaanm
 *
 */
public class ioCopier {
	IoObjectInterface inputIoObject=null;
	String inputClassName = "";
	IoObjectInterface outputIoObject=null;
	String outputClassName = "";
	
	/**
	 * Add the dumper to an existing ioObject
	 * @param ioObject
	 */
	public ioCopier(IoObjectInterface input, IoObjectInterface output){
		this.inputIoObject = input;
		this.inputClassName = input.getClass().getName();
		this.outputIoObject = output;
		this.outputClassName = output.getClass().getName();
	}
	
	/**
	 * Create an ioObject from file and className and add a dumper to it. 
	 * @param inputWorkingDir
	 * @param inputFileName
	 * @param inputClassName
	 */
	public ioCopier(File inputWorkingDir, String inputFileName, String inputClassName,String inputArgs[],
			File outputWorkingDir, String outputFileName, String outputClassName,String outputArgs[]){
		// initialize input
		this.inputClassName = inputClassName;
		Object instance=null;
		try {
			Class aClass = Class.forName(inputClassName);
			instance = aClass.newInstance();
		} catch (ClassNotFoundException e) {
			throw new RuntimeException("ClassNotFoundException creating " + inputClassName);
		} catch (InstantiationException e) {
			throw new RuntimeException("InstantiationException creating " + inputClassName);
		} catch (IllegalAccessException e) {
			throw new RuntimeException("IllegalAccessException creating " + inputClassName);
		}
		if (!(instance instanceof IoObjectInterface)) {
			throw new RuntimeException(inputClassName + " is not implementing the IoObjectInterface");
		}
		this.inputIoObject = (IoObjectInterface)instance;
		this.inputIoObject.initialize(inputWorkingDir, inputFileName, inputArgs);
		// initialize output
		this.outputClassName = outputClassName;
		instance=null;
		try {
			Class aClass = Class.forName(outputClassName);
			instance = aClass.newInstance();
		} catch (ClassNotFoundException e) {
			throw new RuntimeException("ClassNotFoundException creating " + outputClassName);
		} catch (InstantiationException e) {
			throw new RuntimeException("InstantiationException creating " + outputClassName);
		} catch (IllegalAccessException e) {
			throw new RuntimeException("IllegalAccessException creating " + outputClassName);
		}
		if (!(instance instanceof IoObjectInterface)) {
			throw new RuntimeException(outputClassName + " is not implementing the IoObjectInterface");
		}
		this.outputIoObject = (IoObjectInterface)instance;
		this.outputIoObject.initialize(outputWorkingDir, outputFileName, outputArgs);
	}

	/**
	 * Copy the values for all exchangeItems with named ids
	 */
	public void copyValuesForNamedItems(String inputId, String outputId){
		IPrevExchangeItem[] inputItems = this.inputIoObject.getExchangeItems();
		IPrevExchangeItem[] outputItems = this.outputIoObject.getExchangeItems();
		IPrevExchangeItem inputItem = findItem(inputItems,inputId);
		if(inputItem==null){
			throw new RuntimeException("Id not found in input object");
		}
		IPrevExchangeItem outputItem = findItem(outputItems,outputId);
		if(outputItem==null){
			throw new RuntimeException("Id not found in output object");
		}
		System.out.println("#================================");
		System.out.println("Input:");
		System.out.println("   ioObject="+this.inputClassName);
		System.out.println("   id="+inputId);
		System.out.println("Output:");
		System.out.println("   ioObject="+this.outputClassName);
		System.out.println("   id="+outputId);
		System.out.println("#================================");
		//
		// TODO handle special items such as series
		//
		double values[] = inputItem.getValuesAsDoubles();
		int noValuesInput  = values.length; 
		int noValuesOutput = outputItem.getValuesAsDoubles().length;
		if(noValuesInput!=noValuesOutput){
			throw new RuntimeException("Number of values different for input and output items. "
					+"#input="+noValuesInput+" #output="+noValuesOutput);
		}
		outputItem.setValuesAsDoubles(values);
	}	

	/**
	 * Find an exchangeItem in a an array by name
	 * @param exchangeItems
	 * @param id
	 * @return
	 */
	private IPrevExchangeItem findItem(IPrevExchangeItem[] exchangeItems, String id){
		IPrevExchangeItem result = null;
		for(IPrevExchangeItem item : exchangeItems){
			if(item.getId().equalsIgnoreCase(id)){
				result = item;
			}
		}
		if(result==null){
			System.out.println("Id was not found: "+id);
			System.out.println("Available id's are:");
			for(IPrevExchangeItem item : exchangeItems){
				System.out.println("   id="+item.getId());
			}			
		}
		return result;
	}
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		if(args.length<1){
			throw new RuntimeException("Arguments: filename [classname]");
		}
		if(args.length==1){
			System.out.println("No class name was provided to read the file with.");
			System.out.println("The possible ioObject names are:");
			Reflection ref = new Reflection();
			Set<String> selection = ref.getClassNames("org.openda.blackbox.interfaces.IoObjectInterface");
			for(String classname : selection){
				System.out.println("   class name:  "+classname);
			}
			throw new RuntimeException("Please try again with class name.");
		}
		//filename
		String longFileName = args[0];
		File file = new File(longFileName);
		File workingDir = file.getParentFile();
		String fileName = file.getName();
		//classname
		String className = args[1];
		String ioArgs[] = new String[0];
		if(args.length>=2){
			ioArgs = new String[args.length-2]; // pass remaining arguments to module
			for(int i=2;i<args.length;i++){
				ioArgs[i-2]=args[i];
			}
		}
		//create Dumper and dump
		//ioCopier dumper = new ioCopier(workingDir,fileName,className,ioArgs);
		//dumper.dump();
	}

}
