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


import org.openda.blackbox.config.BBStochModelVectorConfig;
import org.openda.exchange.NetcdfScalarTimeSeriesExchangeItem;
import org.openda.interfaces.IComposableDataObject;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IDimensionIndex;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.Results;

import java.util.Collection;

/**
 * The IDataObject is an important tool for creating file based connections to OpenDA.
 * This DataCopier class can copy the contents of one IDataObject to another IDataObject.
 * This can e.g. be used for format conversion.
 * 
 *
 * @author Arno Kockx
 */
public class DataCopier {

	private final IDataObject inputDataObject;
	private final IDataObject outputDataObject;

	/**
	 * Creates a DataCopier for the given input IDataObject and output IDataObject.
	 *
	 * @param input
	 * @param output
	 */
	public DataCopier(IDataObject input, IDataObject output) {
		if (input == null) {
			throw new IllegalArgumentException(getClass().getName() + ": input is null.");
		}
		if (output == null) {
			throw new IllegalArgumentException(getClass().getName() + ": output is null.");
		}

		this.inputDataObject = input;
		this.outputDataObject = output;
	}

	/**
	 * Creates a DataCopier with an input IDataObject and an output IDataObject as specified
	 * by the given arguments.
	 *
	 * @param inputFilePath full pathname of input file.
	 * @param inputClassName of input IDataObject to use to read data from the input file.
	 * @param inputArguments optional one or more arguments that are passed to the input IDataObject initialize method.
	 * @param outputFilePath full pathname of output file.
	 * @param outputClassName of output IDataObject to use to write data to the output file.
	 * @param outputArguments optional one or more arguments that are passed to the output IDataObject initialize method.
	 */
	public DataCopier(String inputFilePath, String inputClassName, String[] inputArguments,
			String outputFilePath, String outputClassName, String[] outputArguments) {
		this.inputDataObject = IoUtils.initializeDataObject(inputFilePath, inputClassName, inputArguments);
		this.outputDataObject = IoUtils.initializeDataObject(outputFilePath, outputClassName, outputArguments);
	}

	/**
	 * Copies the values from the exchangeItem with the given id from the given input IDataObject to the given output IDataObject.
	 */
	private static void copyValuesForNamedItem(String exchangeItemId, IDataObject inputDataObject, IDataObject outputDataObject) {
		System.out.println("copying " + exchangeItemId);

		IExchangeItem inputExchangeItem = inputDataObject.getDataObjectExchangeItem(exchangeItemId);
		if (inputExchangeItem == null) {
			throw new IllegalArgumentException(DataCopier.class.getSimpleName() + ": exchange item with id '" + exchangeItemId + "' not found in input data object.");
		}

		IExchangeItem outputExchangeItem = outputDataObject.getDataObjectExchangeItem(exchangeItemId);
		if (outputExchangeItem != null) {//if outputExchangeItem with the same id already exists, copy values.
			outputExchangeItem.copyValuesFromItem(inputExchangeItem);

		} else {//if outputExchangeItem with the same id does not exist yet, copy exchangeItem.
			if (!(outputDataObject instanceof IComposableDataObject)) {
				throw new RuntimeException(DataCopier.class.getSimpleName() + ": exchange item with id '" + exchangeItemId + "' not found in output data object."
						+ " Also cannot create this exchange item, since " + outputDataObject.getClass().getName() + " does not implement the " + IComposableDataObject.class.getName() + " interface.");
			}
			((IComposableDataObject) outputDataObject).addExchangeItem(inputExchangeItem);
		}
	}

//	/**
//	 * Copies the values from the exchangeItems with the given inputExchangeItemIds from the input IDataObject
//	 * to the exchangeItems with the given outputExchangeItemIds in the output IDataObject.
//	 */
//	private void copyValuesForNamedItems(String[] inputExchangeItemIds, String[] outputExchangeItemIds) {
//		if (inputExchangeItemIds.length != outputExchangeItemIds.length) {
//			throw new RuntimeException(getClass().getSimpleName() + ": inputExchangeItemIds length (" +
//					inputExchangeItemIds.length + ") and outputExchangeItemIds length (" +
//					outputExchangeItemIds.length + ") should be the same.");
//		}
//
//		//copy exchangeItems.
//		for (int n = 0; n < inputExchangeItemIds.length; n++) {
//			copyValuesForNamedItem(inputExchangeItemIds[n], outputExchangeItemIds[n]);
//		}
//	}
//
//	/**
//	 * Copy the values for the exchangeItems with the given ids from input to output IDataObject.
//	 */
//	private void copyValuesForNamedItems(String[] exchangeItemIds) {
//		//copy exchangeItems.
//		for (String exchangeItemId : exchangeItemIds) {
//			copyValuesForNamedItem(exchangeItemId, exchangeItemId);
//		}
//	}

	/**
	 * Copies all exchangeItems from the input dataObject to the output dataObject.
	 */
	public void copyAll() {
		//copy all exchangeItems.
		String[] exchangeItemIds = this.inputDataObject.getExchangeItemIDs();
		for (String exchangeItemId : exchangeItemIds) {
			copyValuesForNamedItem(exchangeItemId, inputDataObject, outputDataObject);
		}

		//write output data.
		this.outputDataObject.finish();
	}

	/**
	 * Help text for the command line
	 * @return
	 */
	private static String getUsageMessage() {
		StringBuffer message = new StringBuffer();
		message.append("NAME\n"
				+"\t oda_copy.sh - a tool for copying data between OpenDA data-objects\n");
		message.append("SYNOPSIS\n"
				+"\t oda_copy.sh [SRCOPTION...] SRC [DESTOPTION...] DEST [COPYOPTION...]\n"
				+"\t or on windows: oda_copy.bat [SRCOPTION...] SRC [DESTOPTION...] DEST [COPYOPTION...]\n"
				+"\t copy everything from known types: oda_copy.sh SRC DEST\n"
				+"\t copy from named classes: oda_copy -c SRC_CLASSNAME SRC -c DEST_CLASSNAME DEST\n"
				+"\t SRC contains a ");
		message.append("DESCRIPTION\n"
				+"\t The IDataObject is the central interface for connecting files in different format to OpenDA.\n"
				+"\t This tools uses the reading and writing routines of these classes to copy data between files/objects\n"
				+"\t Various options control the selection of data to copied\n");
		message.append("SRCOPTIONS\n"
				+"-c CLASSNAME \t Select class of dataobject to use. Without this option, an attempt is made to select a default.\n"
				+"-a ARGS \t Arguments to pass on to the DataObject, eg some metadata not available in the file itself.\n");
		message.append("DESTOPTIONS\n"
				+"-c CLASSNAME \t Select class of dataobject to use. Without this option, an attempt is made to select a default.\n"
				+"-a ARGS \t Arguments to pass on to the DataObject, eg some metadata not available in the file itself.");
		return message.toString();
	}

	/**
	 * Copies all data from the inputFile to the outputFile.
	 *
	 * @param command line arguments arguments: see help text above
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
		if(argIndex<arguments.length){
			nextArg=arguments[argIndex];
		}else{
			throw new RuntimeException("Was expecting more arguments.");
		}
		// 3) DEST OPTIONS
		String outputClassName=null;
		String outputArgs[]=new String[0];
		while(nextArg.startsWith("-")){
			String argValue=null;
			if((argIndex+1)<arguments.length){
				argValue=arguments[argIndex+1];
			}else{
				throw new RuntimeException("Was expecting a value for option: "+nextArg);
			}
			if(nextArg.toLowerCase().startsWith("-c")){
				outputClassName=argValue;
			}else if(nextArg.toLowerCase().startsWith("-a")){
				String outputArgsAsOne=argValue;
				outputArgs=outputArgsAsOne.split(" ");
			}
			argIndex+=2;
			if(argIndex<arguments.length){
				nextArg=arguments[argIndex];
			}else{
				throw new RuntimeException("Was expecting more arguments.");
			}
		}
		// 4) DEST
		String outputFileName=nextArg;
		argIndex++;
		// 5) ITEM OPTIONS
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

		if(outputClassName==null){
			outputClassName=IoUtils.getDefaultClass(outputFileName);
			if(outputClassName==null){
				throw new RuntimeException("Could not find code to read output file.");
			}
		}
		System.out.println("output");
		System.out.println("\t file: "+outputFileName);
		System.out.println("\t class: "+outputClassName);
		System.out.print("\t args: ");
		for(int i=0;i<outputArgs.length;i++){System.out.print(outputArgs[i]+" ");}
		System.out.println();

		//
		// Copy data
		//
		DataCopier copier = new DataCopier(inputFileName, inputClassName, inputArgs, 
				outputFileName, outputClassName, outputArgs);
		copier.copyAll();
	}

	/**
	 * For each exchangeItemId in the given list the outputExchangeItem is retrieved from the given outputDataObject,
	 * and the corresponding inputExchangeItem is retrieved from one of the given inputDataObjects.
	 * Then the outputExchangeItem is asked to copy all value(s) that it currently needs from the corresponding inputExchangeItem.
	 *
	 * Depending on the nature of the input/output exchangeItems, different value(s) are copied.
	 * If e.g an outputExchangeItem can only store value(s) for a single time and the corresponding inputExchangeItem stores
	 * values for an entire time series, then when this method is called the outputExchangeItem only copies the value(s) for the
	 * time for which it stores value(s) (the outputExchangeItem nows which time this is).
	 */
	public static void copyDataFromDataObjectsToDataObject(String[] exchangeItemIds,
			IDataObject[] inputDataObjects, IDataObject outputDataObject) {

		for (String id : exchangeItemIds) {
			//find outputExchangeItem.
			IExchangeItem outputExchangeItem = outputDataObject.getDataObjectExchangeItem(id);
			if (outputExchangeItem == null) {
				throw new RuntimeException("Exchange item with id '" + id + "' not found in given outputDataObject.");
			}

			//find corresponding inputExchangeItem.
			IExchangeItem inputExchangeItem = null;
			for (IDataObject inputDataObject : inputDataObjects) {
				inputExchangeItem = inputDataObject.getDataObjectExchangeItem(id);
				if (inputExchangeItem != null) {
					break;
				}
			}
			if (inputExchangeItem == null) {
				throw new RuntimeException("Exchange item with id '" + id + "' not found in given inputDataObjects.");
			}

			//ask the outputExchangeItem to copy all value(s) that it currently needs from the inputExchangeItem.
	        Results.putMessage(DataCopier.class.getSimpleName() + ": copying data from inputExchangeItem '"
	        		+ id + "' of type " + inputExchangeItem.getClass().getSimpleName()
	        		+ " to outputExchangeItem '" + id + "' of type " + outputExchangeItem.getClass().getSimpleName());
			outputExchangeItem.copyValuesFromItem(inputExchangeItem);
		}
	}

    public static void copyDataFromDataObjectsToDataObjectWithSelection(Collection<BBStochModelVectorConfig> scalarOutputVectorCollection, IDataObject[] inputDataObjects, IDataObject outputDataObject) {

        for (java.util.Iterator<BBStochModelVectorConfig> it = scalarOutputVectorCollection.iterator(); it.hasNext();) {
            BBStochModelVectorConfig vectorConfig = it.next();
            String locId = vectorConfig.getId();
            String sourceExchangeItemId = vectorConfig.getSourceId();
            IDimensionIndex[] selectionIndices = vectorConfig.getSelectionIndices();
            //find outputExchangeItem.
            String id = locId+"."+sourceExchangeItemId;
            IExchangeItem outputExchangeItem = outputDataObject.getDataObjectExchangeItem(id);
            if (outputExchangeItem == null) {
                throw new RuntimeException("Exchange item with id '" + id + "' not found in given outputDataObject.");
            }
            //find corresponding inputExchangeItem.
            IExchangeItem inputExchangeItem = null;
            for (IDataObject inputDataObject : inputDataObjects) {
                inputExchangeItem = inputDataObject.getDataObjectExchangeItem(sourceExchangeItemId);
                if (inputExchangeItem != null) {
                    break;
                }
            }
            if (inputExchangeItem == null) {
                throw new RuntimeException("Exchange item with id '" + sourceExchangeItemId + "' not found in given inputDataObjects.");
            }

            //ask the outputExchangeItem to copy all value(s) that it currently needs from the inputExchangeItem.
            Results.putMessage(DataCopier.class.getSimpleName() + ": copying data from inputExchangeItem '"
                    + id + "' of type " + inputExchangeItem.getClass().getSimpleName()
                    + " to outputExchangeItem '" + id + "' of type " + outputExchangeItem.getClass().getSimpleName());
            ((NetcdfScalarTimeSeriesExchangeItem)outputExchangeItem).copyValuesFromItem(inputExchangeItem,selectionIndices);
        }
    }
}
