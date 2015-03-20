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
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.Set;

import org.openda.exchange.timeseries.NoosTimeSeriesFormatter;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.Reflection;
import org.openda.utils.Vector;

/**
 * The IDataObject is an important tool for creating file based connections to OpenDA.
 * This DataDumper class can show the contents of an IDataObject.
 *
 * @author Arno Kockx
 */
public class DataDumper {
	private final IDataObject dataObject;
	private File outputDir=new File("."); //dump to current directory by default

	/**
	 * Creates a dumper for an existing IDataObject.
	 *
	 * @param dataObject
	 */
	public DataDumper(IDataObject dataObject){
		if (dataObject == null) {
			throw new IllegalArgumentException(getClass().getName() + ": dataObject is null.");
		}

		this.dataObject = dataObject;
	}

	/**
	 * Creates a DataDumper with an IDataObject as specified by the given arguments.
	 *
	 * @param filePath full pathname of input file.
	 * @param className of IDataObject to use to read data from the input file.
	 * @param arguments optional one or more arguments that are passed to the IDataObject initialize method.
	 */
	public DataDumper(String filePath, String className, String[] arguments){
		this.dataObject = IoUtils.initializeDataObject(filePath, className, arguments);
	}

	public void setOutputDir(File outputDir){
		this.outputDir=outputDir;
		if(!outputDir.exists()){
			throw new RuntimeException("Ouput directory for DataDumper does not exist:"+outputDir.getPath());
		}
	}
	/**
	 * Dump data contents to System.out.
	 */
	public void dump(){
		String[] ids = this.dataObject.getExchangeItemIDs();
		IExchangeItem[] items = new IExchangeItem[ids.length];
		for (int n = 0; n < ids.length; n++) {
			items[n] = this.dataObject.getDataObjectExchangeItem(ids[n]);
		}

		for(IExchangeItem item : items){
			System.out.println("#================================");

			if(item instanceof TimeSeries) {
				System.out.println("TimeSeries id=" + item.getId());
				System.out.println("role =" + item.getRole());
				System.out.println(((TimeSeries) item).toString());
				//NoosTimeSeriesFormatter f = new NoosTimeSeriesFormatter();
				//String fileName=this.outputDir+File.separator+item.getId() + ".noos";
				//f.writeFile(fileName, (TimeSeries) item, false);

			} else if (item instanceof IExchangeItem) {
				try {
					Writer writer = new OutputStreamWriter(System.out);
					try {
						IoUtils.writeExchangeItem(writer, ((IExchangeItem) item));
					} finally {
						//do not close System.out.
						writer.flush();
					}
				} catch (IOException e){
					throw new RuntimeException("Problem while writing data for exchange item with id '" + item.getId() + "'.", e);
				}

			} else {
				System.out.println("id =" + item.getId());
				System.out.println("description =" + item.getDescription());
				System.out.println("role =" + item.getRole());
				double times[] = item.getTimes();
				if(times != null){
					System.out.println("times =" + new Vector(times));
					System.out.println("times.length =" + times.length);
				}
				double values[] = item.getValuesAsDoubles();
				if(values != null){
					System.out.println("values =" + new Vector(values));
					System.out.println("values.length =" + values.length);
				}
			}
		}

		System.out.println("#================================");
	}

	/**
	 * Help text for the command line
	 * @return
	 */
	public static String getUsageMessage() {
		StringBuffer message = new StringBuffer();
		message.append("NAME\n"
				+"\t oda_dump.sh - a tool for showing content of a OpenDA data-objects\n");
		message.append("SYNOPSIS\n"
				+"\t oda_dump.sh [SRCOPTION...] SRC [VARIABLEOPTION...]\n"
				+"\t or on windows: oda_dump.bat [SRCOPTION...] SRC [VARIABLEOPTION...]\n"
				+"\t dumps content for known types: oda_dump.sh SRC \n"
				+"\t dump for named class: oda_dump.sh -c SRC_CLASSNAME SRC\n");
		message.append("DESCRIPTION\n"
				+"\t The IDataObject is the central interface for connecting files in different format to OpenDA.\n"
				+"\t This tools uses the reading and writing routines of these classes to show the contents of files.\n"
				+"\t Various options control the selection of data to shown.\n");
		message.append("SRCOPTIONS\n"
				+"-c CLASSNAME \t Select class of dataobject to use. Without this option, an attempt is made to select a default.\n"
				+"-a ARGS \t Arguments to pass on to the DataObject, eg some metadata not available in the file itself.\n");
		return message.toString();
	}

	/**
	 * Dumps all data from the inputFile to System.out in text format.
	 * This can be used to investigate the contents of a particular file.
	 *
	 * @param arguments filename (required)
	 *                  className (required)
	 *                  arguments (optional) one or more arguments that are passed to the inputClass initialize method.
	 */
	public static void main(String[] arguments) {
		//
		//read arguments.
		//
		// check for -h (help) option
		if(arguments.length==0 || arguments[0].trim().equalsIgnoreCase("-h")){
			System.out.println(getUsageMessage());
			return;
		}

		int argIndex=0;
		String nextArg=(arguments[argIndex]).trim();
		// 1) SRC OPTIONS
		String inputClassName=null;
		String inputArgs[]=new String[0];
		while(nextArg.startsWith("-")){
			String argValue=null;
			if((argIndex+1)<arguments.length){
				argValue=arguments[argIndex+1];
			}else{
				throw new RuntimeException("Was expecting a value for option: "+nextArg);
			}
			if(nextArg.toLowerCase().startsWith("-c")){
				inputClassName=argValue;
			}else if(nextArg.toLowerCase().startsWith("-a")){
				String inputArgsAsOne=argValue;
				inputArgs=inputArgsAsOne.split(" ");
			}
			argIndex+=2;
			if(argIndex<arguments.length){
				nextArg=arguments[argIndex];
			}else{
				throw new RuntimeException("Was expecting more arguments.");
			}
		}
		// 2) SRC file
		String inputFileName=nextArg;
		argIndex++;
		// 3) ITEM OPTIONS
		if(argIndex<arguments.length){
			//TODO more options were given
		}
		
		//
		// Check input
		//
		if(inputClassName==null){
			inputClassName=IoUtils.getDefaultClass(inputFileName);
			if(inputClassName==null){
				throw new RuntimeException("Could not find code to read input file.");
			}
		}
		System.out.println("input");
		System.out.println("\t file: "+inputFileName);
		System.out.println("\t class: "+inputClassName);
		System.out.print("\t args: ");
		for(int i=0;i<inputArgs.length;i++){System.out.print(inputArgs[i]+" ");}
		System.out.println();
		
		//dump data.
		DataDumper dumper = new DataDumper(inputFileName, inputClassName, inputArgs);
		dumper.dump();
	}
}
