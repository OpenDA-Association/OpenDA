/* OpenDA v2.4 
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
package org.openda.exchange.iotools;

import org.openda.blackbox.config.BBStochModelVectorConfig;
import org.openda.blackbox.config.BBUtils;
import org.openda.exchange.NetcdfScalarTimeSeriesExchangeItem;
import org.openda.interfaces.*;
import org.openda.utils.Results;

import java.io.File;
import java.util.*;

/**
 * The IDataObject is an important tool for creating file based connections to OpenDA.
 * This DataCopier class can copy the contents of one IDataObject to another IDataObject.
 * This can e.g. be used for format conversion.
 *
 * @author Arno Kockx
 */
public class DataCopier implements IConfigurable {

	private String inputFileName = null;
	private String inputClassName = null;
	private String[] inputArgs = null;
	private String outputFileName = null;
	private String outputClassName = null;
	private String[] outputArgs = null;

	private IDataObject inputDataObject = null;
	private IDataObject outputDataObject = null;

	/**
	 * Creates a DataCopier for the given input IDataObject and output IDataObject.
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

	public DataCopier() {
	}

	/**
	 * This method is only present to be able to run DataCopier as a BBAction. Also see comments in method BBUtils.runJavaClass.
	 */
	public void initialize(File workingDir, String[] arguments) {
		run(arguments);
	}

	private void initDataObjects() {
		//Note: this code uses the parent folder of the input/outputFilePath as the workingDir for the input/outputDataObject. This does NOT work correctly when running DataCopier as a BBAction.
		inputDataObject = IoUtils.initializeDataObject(inputFileName, inputClassName, inputArgs);
		outputDataObject = IoUtils.initializeDataObject(outputFileName, outputClassName, outputArgs);
	}

	/**
	 * Copies all data from the inputFile to the outputFile.
	 *
	 * @param arguments command line arguments: see help text above
	 */
	public static void main(String[] arguments) {
		DataCopier copier = new DataCopier();
		copier.run(arguments);
	}

	private void run(String[] arguments) {
		processArguments(arguments);

		initDataObjects();

		copyAll();
		finish();
	}

	/**
	 * Returns all exchange items from the given inputDataObject.
	 */
	private static IExchangeItem[] getAllInputExchangeItems(IDataObject inputDataObject) {
		String[] ids = inputDataObject.getExchangeItemIDs();
		IExchangeItem[] items = new IExchangeItem[ids.length];
		for (int n = 0; n < items.length; n++) {
			String id = ids[n];
			IExchangeItem item = inputDataObject.getDataObjectExchangeItem(id);
			if (item == null) {
				throw new IllegalArgumentException(DataCopier.class.getSimpleName() + ": exchange item with id '" + id + "' not found in input data object.");
			}
			items[n] = item;
		}
		return items;
	}

	/**
	 * Returns all ensemble exchange items from the given inputDataObject.
	 */
	private static Map<String, Map<Integer, IExchangeItem>> getAllEnsembleInputExchangeItems(IEnsembleDataObject inputDataObject) {
		Map<String, Map<Integer, IExchangeItem>> ensembles = new LinkedHashMap<>();

		String[] ids = inputDataObject.getEnsembleExchangeItemIds();
		int[] indices = inputDataObject.getEnsembleMemberIndices();
		for (String id : ids) {
			Map<Integer, IExchangeItem> ensemble = new LinkedHashMap<>();
			ensembles.put(id, ensemble);

			for (int index : indices) {
				IExchangeItem item = inputDataObject.getDataObjectExchangeItem(id, index);
				if (item == null) {
					throw new IllegalArgumentException(DataCopier.class.getSimpleName() + ": ensemble exchange item with id '" + id + "' and ensemble member index " + index + " not found in input data object.");
				}
				ensemble.put(index, item);
			}
		}

		return ensembles;
	}

	/**
	 * Checks that the given outputDataObject contains a matching output exchangeItem for the given inputExchangeItem.
	 * If a matching output exchangeItem does not exist yet, then it is created by calling addExchangeItem on the outputDataObject.
	 */
	private static void validateOutputExchangeItem(IExchangeItem inputExchangeItem, IDataObject outputDataObject) {
		String id = inputExchangeItem.getId();
		IExchangeItem outputExchangeItem = outputDataObject.getDataObjectExchangeItem(id);
		if (outputExchangeItem != null) return;

		//output exchangeItem with the same id does not exist yet.
		String message = "Exchange item with id '" + id + "' not found in output data object.";
		System.out.println(message);
		Results.putMessage(DataCopier.class.getSimpleName() + ": " + message);
		if (outputDataObject instanceof IComposableDataObject) {
			//add exchangeItem.
			message = "Adding new exchange item with id '" + id + "' to output data object.";
			System.out.println(message);
			Results.putMessage(DataCopier.class.getSimpleName() + ": " + message);
			((IComposableDataObject) outputDataObject).addExchangeItem(inputExchangeItem);
		} else {
			message = "WARNING: Input data for exchange item with id '" + id + "' will not be copied to output data object.";
			System.out.println(message);
			Results.putMessage(DataCopier.class.getSimpleName() + ": " + message);
		}
	}

	/**
	 * Checks that the given outputDataObject contains a matching ensemble output exchangeItem for the given ensemble inputExchangeItem.
	 * If a matching ensemble output exchangeItem does not exist yet, then it is created by calling addExchangeItem on the outputDataObject.
	 */
	private static void validateEnsembleOutputExchangeItem(IExchangeItem inputExchangeItem, int ensembleMemberIndex, IEnsembleDataObject outputDataObject) {
		String id = inputExchangeItem.getId();
		IExchangeItem outputExchangeItem = outputDataObject.getDataObjectExchangeItem(id, ensembleMemberIndex);
		if (outputExchangeItem != null) return;

		//output exchangeItem with the same id and index does not exist yet.
		String message = "Ensemble exchange item with id '" + id + "' and ensemble member index " + ensembleMemberIndex + " not found in output data object.";
		System.out.println(message);
		Results.putMessage(DataCopier.class.getSimpleName() + ": " + message);
		if (outputDataObject instanceof IComposableEnsembleDataObject) {
			//add exchangeItem.
			message = "Adding new ensemble exchange item with id '" + id + "' and ensemble member index " + ensembleMemberIndex + " to output data object.";
			System.out.println(message);
			Results.putMessage(DataCopier.class.getSimpleName() + ": " + message);
			((IComposableEnsembleDataObject) outputDataObject).addExchangeItem(inputExchangeItem, ensembleMemberIndex);
		} else {
			message = "WARNING: Input data for ensemble exchange item with id '" + id + "' and ensemble member index " + ensembleMemberIndex + " will not be copied to output data object.";
			System.out.println(message);
			Results.putMessage(DataCopier.class.getSimpleName() + ": " + message);
		}
	}

	/**
	 * Copies the values from the given inputExchangeItem to the matching output exchangeItem in the given outputDataObject.
	 */
	private static void copyValuesFromItem(IExchangeItem inputExchangeItem, IDataObject outputDataObject) {
		String id = inputExchangeItem.getId();
		IExchangeItem outputExchangeItem = outputDataObject.getDataObjectExchangeItem(id);
		if (outputExchangeItem == null) {
			return;
		}

		String message = "Copying " + id;
		System.out.println(message);
		//TODO: [LOGGING] move to debug when new logging is avalablle
		//Results.putMessage(DataCopier.class.getSimpleName() + ": " + message);
		outputExchangeItem.copyValuesFromItem(inputExchangeItem);
	}

	/**
	 * Copies the values from the given ensemble inputExchangeItem to the matching ensemble output exchangeItem in the given outputDataObject.
	 */
	private static void copyValuesForNamedEnsembleItem(IExchangeItem inputExchangeItem, int ensembleMemberIndex, IEnsembleDataObject outputDataObject) {
		String id = inputExchangeItem.getId();
		IExchangeItem outputExchangeItem = outputDataObject.getDataObjectExchangeItem(id, ensembleMemberIndex);
		if (outputExchangeItem == null) {
			return;
		}

		String message = "Copying " + id + " for ensemble member index " + ensembleMemberIndex;
		System.out.println(message);
		Results.putMessage(DataCopier.class.getSimpleName() + ": " + message);
		outputExchangeItem.copyValuesFromItem(inputExchangeItem);
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
		if (inputDataObject == null) throw new IllegalStateException(getClass().getSimpleName() + ": inputDataObject not initialized.");
		if (outputDataObject == null) throw new IllegalStateException(getClass().getSimpleName() + ": outputDataObject not initialized.");

		//get non-ensemble exchangeItems.
		IExchangeItem[] inputExchangeItems = getAllInputExchangeItems(inputDataObject);
		String message = "found " + inputExchangeItems.length + " input exchange items.";
		System.out.println(message);
		Results.putMessage(getClass().getSimpleName() + ": " + message);
		//get ensemble exchangeItems.
		Map<String, Map<Integer, IExchangeItem>> ensembles = new LinkedHashMap<>();
		if (inputDataObject instanceof IEnsembleDataObject) {
			ensembles = getAllEnsembleInputExchangeItems((IEnsembleDataObject) inputDataObject);
			if (!ensembles.isEmpty() && !(outputDataObject instanceof IEnsembleDataObject)) {
				throw new RuntimeException(getClass().getSimpleName() + ": Cannot copy ensemble exchange items since output data object does not implement the " + IEnsembleDataObject.class.getName() + " interface.");
			}
			String[] ids = ((IEnsembleDataObject) inputDataObject).getEnsembleExchangeItemIds();
			int[] indices = ((IEnsembleDataObject) inputDataObject).getEnsembleMemberIndices();
			message = "found " + (ids.length * indices.length) + " ensemble input exchange items with ensemble member indices " + new Vector<>(Arrays.asList(BBUtils.box(indices)));
			System.out.println(message);
			Results.putMessage(getClass().getSimpleName() + ": " + message);
		}

		//validate all exchange items before copying any data, since adding new exchange items to an output DataObject may not be possible after writing data.
		message = "Validating output exchange items.";
		System.out.println(message);
		Results.putMessage(getClass().getSimpleName() + ": " + message);
		//validate non-ensemble exchangeItems.
		for (IExchangeItem inputExchangeItem : inputExchangeItems) {
			validateOutputExchangeItem(inputExchangeItem, outputDataObject);
		}
		//validate ensemble exchangeItems.
		for (Map<Integer, IExchangeItem> ensemble : ensembles.values()) {
			for (Map.Entry<Integer, IExchangeItem> entry : ensemble.entrySet()) {
				Integer index = entry.getKey();
				IExchangeItem item = entry.getValue();
				validateEnsembleOutputExchangeItem(item, index, (IEnsembleDataObject) outputDataObject);
			}
		}

		message = "Copying data from input exchange items to output exchange items.";
		System.out.println(message);
		Results.putMessage(getClass().getSimpleName() + ": " + message);
		//copy non-ensemble exchangeItems.
		for (IExchangeItem inputExchangeItem : inputExchangeItems) {
			copyValuesFromItem(inputExchangeItem, outputDataObject);
		}
		//copy ensemble exchangeItems.
		for (Map<Integer, IExchangeItem> ensemble : ensembles.values()) {
			for (Map.Entry<Integer, IExchangeItem> entry : ensemble.entrySet()) {
				Integer index = entry.getKey();
				IExchangeItem item = entry.getValue();
				copyValuesForNamedEnsembleItem(item, index, (IEnsembleDataObject) outputDataObject);
			}
		}
	}

	public void finish() {
		//make sure output data has been written.
		if (outputDataObject != null) outputDataObject.finish();
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
	 * Gathers the following variables from the given arguments.
	 *
	 * inputFilePath: full pathname of input file.
	 * inputClassName: fully qualified class name of input IDataObject to use to read data from the input file.
	 * inputArguments: optional one or more arguments that are passed to the input IDataObject initialize method.
	 * outputFilePath: full pathname of output file.
	 * outputClassName: fully qualified class name of output IDataObject to use to write data to the output file.
	 * outputArguments: optional one or more arguments that are passed to the output IDataObject initialize method.
	 */
	private void processArguments(String[] arguments) {
		//
		//read arguments.
		//
		// check for -h (help) option
		if(arguments.length==0 || arguments[0].trim().equalsIgnoreCase("-h")){
			String message = getUsageMessage();
			System.out.println(message);
			Results.putMessage(getClass().getSimpleName() + ":\n" + message);
			return;
		}

		int argIndex=0;
		String nextArg=(arguments[argIndex]).trim();
		// 1) SRC OPTIONS
		inputClassName=null;
		inputArgs=new String[0];
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
		inputFileName=nextArg;
		argIndex++;
		if(argIndex<arguments.length){
			nextArg=arguments[argIndex];
		}else{
			throw new RuntimeException("Was expecting more arguments.");
		}
		// 3) DEST OPTIONS
		outputClassName=null;
		outputArgs=new String[0];
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
		outputFileName=nextArg;
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
		String message = "input\n";
		message += "\t file: " + inputFileName + "\n";
		message += "\t class: " + inputClassName + "\n";
		message += "\t args: ";
		for (String inputArg : inputArgs) {
			message += inputArg + " ";
		}
		System.out.println(message);
		Results.putMessage(getClass().getSimpleName() + ": " + message);

		//
		// Check output
		//
		if(outputClassName==null){
			outputClassName=IoUtils.getDefaultClass(outputFileName);
			if(outputClassName==null){
				throw new RuntimeException("Could not find code to read output file.");
			}
		}
		message = "output\n";
		message += "\t file: " + outputFileName + "\n";
		message += "\t class: " + outputClassName + "\n";
		message += "\t args: ";
		for (String outputArg : outputArgs) {
			message += outputArg + " ";
		}
		System.out.println(message);
		Results.putMessage(getClass().getSimpleName() + ": " + message);
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

	@Deprecated
	//TODO remove. AK
    public static void copyDataFromDataObjectsToDataObjectWithSelection(Collection<BBStochModelVectorConfig> scalarOutputVectorCollection, IDataObject[] inputDataObjects, IDataObject outputDataObject) {

        for (java.util.Iterator<BBStochModelVectorConfig> it = scalarOutputVectorCollection.iterator(); it.hasNext();) {
            BBStochModelVectorConfig vectorConfig = it.next();
            String id = vectorConfig.getId();
            String sourceExchangeItemId = vectorConfig.getSourceId();
            IDimensionIndex[] selectionIndices = vectorConfig.getSelectionIndices();
            //find outputExchangeItem.
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
