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
import org.openda.exchange.timeseries.NoosTimeSeriesFormatter;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.Reflection;
import org.openda.utils.Vector;

/**
 * The IoObjectInterface is an important tool for creating file based connections to OpenDA. 
 * This tool can show the contents of an ioObject.
 * @author verlaanm
 *
 */
public class ioDumper {
	IoObjectInterface ioObject=null;
	String className = "";
	
	/**
	 * Add the dumper to an existing ioObject
	 * @param ioObject
	 */
	public ioDumper(IoObjectInterface ioObject){
		this.ioObject = ioObject;
	}
	
	/**
	 * Create an ioObject from file and className and add a dumper to it. 
	 * @param workingDir
	 * @param fileName
	 * @param className
	 */
	public ioDumper(File workingDir, String fileName, String className, String args[]){
		this.className = className;
		Object instance=null;
		try {
			Class aClass = Class.forName(className);
			instance = aClass.newInstance();
		} catch (ClassNotFoundException e) {
			throw new RuntimeException("ClassNotFoundException creating " + className);
		} catch (InstantiationException e) {
			throw new RuntimeException("InstantiationException creating " + className);
		} catch (IllegalAccessException e) {
			throw new RuntimeException("IllegalAccessException creating " + className);
		}
		if (!(instance instanceof IoObjectInterface)) {
			throw new RuntimeException(className + " is not implementing the IoObjectInterface");
		}
		this.ioObject = (IoObjectInterface)instance;
		this.ioObject.initialize(workingDir, fileName, args);
	}
	
	/**
	 * Dump contents to stdout.
	 */
	public void dump(){
		IPrevExchangeItem[] items = this.ioObject.getExchangeItems();
		for(IPrevExchangeItem item : items){
			System.out.println("#================================");
			if(item instanceof TimeSeries){
				System.out.println("TimeSeries id="+item.getId());
				System.out.println("role ="+item.getRole());
				System.out.println(((TimeSeries)item).toString());
				NoosTimeSeriesFormatter f =new NoosTimeSeriesFormatter();
				f.writeFile(item.getId()+".noos", (TimeSeries)item, false);
			}else{
				System.out.println("id ="+item.getId());
				System.out.println("description ="+item.getDescription());
				System.out.println("role ="+item.getRole());
				double times[] = item.getTimes();
				if(times!=null){
					System.out.println("times ="+new Vector(times));
					System.out.println("times.length ="+times.length);
				}
				double values[] = item.getValuesAsDoubles();
				if(values!=null){
					System.out.println("values ="+new Vector(values));
					System.out.println("values.length ="+values.length);
				}
				
			}
		}
		System.out.println("#================================");
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
		ioDumper dumper = new ioDumper(workingDir,fileName,className,ioArgs);
		dumper.dump();
	}

}
