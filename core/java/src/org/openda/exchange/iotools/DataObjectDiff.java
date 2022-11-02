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

import org.openda.blackbox.config.BBUtils;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.*;
import org.openda.utils.Results;

import java.io.File;
import java.util.*;

/**
 * The IDataObject is an important tool for creating file based connections to OpenDA.
 * This DataObjectDiff class can compare the contents of one IDataObject to another IDataObject.
 *
 * @author Werner Kramer
 */
public class DataObjectDiff implements IConfigurable {

    private String referenceFileName = null;
    private String referenceClassName = null;
    private String[] referenceArgs = null;
    private String testFileName = null;
    private String testClassName = null;
    private String[] testArgs = null;
    private IDataObject referenceDataObject = null;
    private IDataObject testDataObject = null;

    private static String DIFF_OUTPUT_HEADER =
        " Difference for file : %1$s\n" +
        "      exchange item          time          rms         max99        max     \n";
    private static String HLINE = "------------------------ ------------ ------------ ------------ ------------\n";
    private static String DIFF_OUTPUT_LINE = "%1$-24s %2$12s %3$12f %4$12f %5$12f";
    public static double EPSILON = 1e-5;

    /**
     * Creates a DataCopier for the given input IDataObject and output IDataObject.

     */
//    public DataObjectDiff(IDataObject reference, IDataObject test) {
//        if (reference == null) {
//            throw new IllegalArgumentException(getClass().getName() + ": reference IDataObject is null.");
//        }
//        if (test == null) {
//            throw new IllegalArgumentException(getClass().getName() + ": test IDataOject is null.");
//        }
//
//        this.referenceDataObject = reference;
//        this.testDataObject = test;
//    }

    public DataObjectDiff() {
    }

    /**
     * This method is only present to be able to run DataObjectDiff as a BBAction. Also see comments in method BBUtils.runJavaClass.
     @param workingDir
     @param
      */
    public void initialize(File workingDir, String[] arguments) {
    	processArguments(arguments);
        initDataObjects();
    }

    private void initDataObjects() {
        //Note: this code uses the parent folder of the input/outputFilePath as the workingDir for the input/outputDataObject. This does NOT work correctly when running DataCopier as a BBAction.
        referenceDataObject = IoUtils.initializeDataObject(referenceFileName, referenceClassName, referenceArgs);
        testDataObject = IoUtils.initializeDataObject(testFileName, testClassName, testArgs);
    }

    /**
     * Copies all data from the inputFile to the outputFile.
     *
     * @param arguments command line arguments: see help text above
     */
    public static void main(String[] arguments) {
		// check for -h (help) option
		if(arguments.length==0 || arguments[0].trim().equalsIgnoreCase("-h")){
			String message = getUsageMessage();
			System.out.println(message);
			return;
		}
		DataObjectDiff differ = new DataObjectDiff();
        differ.initialize(null, arguments);
        boolean result = differ.compare();
        if ( !result) {
            System.exit(1);
        }
    }


    /**
     * Returns all exchange items from the given dataObject.
     */
    private static IExchangeItem[] getAllInputExchangeItems(IDataObject dataObject) {
        String[] ids = dataObject.getExchangeItemIDs();
        IExchangeItem[] items = new IExchangeItem[ids.length];
        for (int n = 0; n < items.length; n++) {
            String id = ids[n];
            IExchangeItem item = dataObject.getDataObjectExchangeItem(id);
            if (item == null) {
                throw new IllegalArgumentException(DataObjectDiff.class.getSimpleName() + ": exchange item with id '" + id + "' not found in input data object.");
            }
            items[n] = item;
        }
        return items;
    }

    /**
     * Returns all ensemble exchange items from the given dataObject.
     */
    private static Map<String, Map<Integer, IExchangeItem>> getAllEnsembleInputExchangeItems(IEnsembleDataObject dataObject) {
        Map<String, Map<Integer, IExchangeItem>> ensembles = new LinkedHashMap<>();

        String[] ids = dataObject.getEnsembleExchangeItemIds();
        int[] indices = dataObject.getEnsembleMemberIndices();
        for (String id : ids) {
            Map<Integer, IExchangeItem> ensemble = new LinkedHashMap<>();
            ensembles.put(id, ensemble);

            for (int index : indices) {
                IExchangeItem item = dataObject.getDataObjectExchangeItem(id, index);
                if (item == null) {
                    throw new IllegalArgumentException(DataObjectDiff.class.getSimpleName() + ": ensemble exchange item with id '" + id + "' and ensemble member index " + index + " not found in input data object.");
                }
                ensemble.put(index, item);
            }
        }

        return ensembles;
    }

    /**
     * Checks that the given dataObject contains a matching output exchangeItem for the given exchangeItem.
     */
    private boolean validateExchangeItem(IExchangeItem exchangeItem, IDataObject dataObject) {
        String id = exchangeItem.getId();
        IExchangeItem testExchangeItem = dataObject.getDataObjectExchangeItem(id);

        // check if exchange item exists
        if (testExchangeItem == null) {
            System.out.println(dataObject.getClass() + " does not contain exchange item id " + id);
            return false;
        }

        double[] times = null;
        if (exchangeItem.getTimeInfo() != null) {
            times = exchangeItem.getTimeInfo().getTimes();
            //System.out.println("id: " + id + " has time info " + times.length);
        }
        // check sizes

        boolean result = true;
        double[] referenceValues = exchangeItem.getValuesAsDoubles();
        double[] testValues = testExchangeItem.getValuesAsDoubles();
        if (referenceValues.length != testValues.length) {
            System.out.println(dataObject.getClass() + " sizes differ for exchange item id " + id + ":" + referenceValues.length + " <> " + testValues.length);
            return false;
        }
        // check root mean square differnce

        List<Double> d1 = new ArrayList(referenceValues.length);
        for (double value : referenceValues)
            d1.add(value);
        List<Double> d2 = new ArrayList(testValues.length);
        for (double value : testValues)
            d2.add(value);

        double norm  = Collections.max(d1) - Collections.min(d1);

        if (times != null && times.length !=0) {
            int n = d1.size() / times.length;
            for (int t = 0; t < times.length; t++) {
                int i1 = t * n;
                int i2 = (t + 1) * n - 1;
                List<Double> slice1 = d1.subList(i1, i2);
                List<Double> slice2 = d2.subList(i1, i2);
                double max = maxDifference(slice1, slice2);
                if (max / norm > EPSILON) {
                    System.out.println(String.format(Locale.UK, DIFF_OUTPUT_LINE, id, TimeUtils.mjdToString(times[t]), rmsDifference(slice1, slice2), p99Difference(slice1, slice2), max));
                    result = false;
                }
            }
        } else {
            double max = maxDifference(d1, d2);
            if (max / norm > EPSILON) {
                System.out.println(String.format(Locale.UK, DIFF_OUTPUT_LINE, id, "NA", rmsDifference(d1, d2), p99Difference(d1, d2), max));
                result = false;
            }
        }
        if (result) {
            System.out.println(String.format(Locale.UK, DIFF_OUTPUT_LINE, id, "", null, null, null));
        }
        System.out.print(HLINE);
        return result;
    }

    public void finish() {
    }
	/**
	 * Help text for the command line
	 */
	private static String getUsageMessage() {
		StringBuffer message = new StringBuffer();
		if (BBUtils.RUNNING_ON_LINUX) {
			message.append("NAME\n"
				+ "\t oda_diff.sh - a tool to compare the contents of one IDataObject to another IDataObject\n");
		} else {
			message.append("NAME\n"
				+ "\t oda_diff.bat - a tool to compare the contents of one IDataObject to another IDataObject\n");
		}
		message.append("SYNOPSIS\n"
			+"\t oda_diff.sh FILE1 FILE2 ...]\n"
			+"\t or on windows: oda_diff.bat FILE1 FILE2...]\n"
			+"\t compares the contents of FILE1 to the contents of FILE2\n");
		message.append("DESCRIPTION\n"
			+"\t The IDataObject is the central interface for connecting files in different format to OpenDA.\n"
			+"\t This tools uses the reading routines of this class to compare data between files/objects\n");
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

        int argIndex=0;
        String nextArg=(arguments[argIndex]).trim();
        // 1) REFERENCE OPTIONS
        referenceClassName=null;
        referenceArgs=new String[0];
        while(nextArg.startsWith("-")){
            String argValue;
            if((argIndex+1)<arguments.length){
                argValue=arguments[argIndex+1];
            }else{
                throw new RuntimeException("Was expecting a value for option: "+nextArg);
            }
            if(nextArg.toLowerCase().startsWith("-c")){
                referenceClassName=argValue;
            }else if(nextArg.toLowerCase().startsWith("-a")){
                referenceArgs=argValue.split(" ");
            }
            argIndex+=2;
            if(argIndex<arguments.length){
                nextArg=arguments[argIndex];
            }else{
                throw new RuntimeException("Was expecting more arguments.");
            }
        }
        // 2) REFERENCE FILE
        referenceFileName=nextArg;
        argIndex++;
        if(argIndex<arguments.length){
            nextArg=arguments[argIndex];
        }else{
            throw new RuntimeException("Was expecting more arguments.");
        }
        // 3) TEST OPTIONS
        testClassName=null;
        testArgs=new String[0];
        while(nextArg.startsWith("-")){
            String argValue;
            if((argIndex+1)<arguments.length){
                argValue=arguments[argIndex+1];
            }else{
                throw new RuntimeException("Was expecting a value for option: "+nextArg);
            }
            if(nextArg.toLowerCase().startsWith("-c")){
                testClassName=argValue;
            }else if(nextArg.toLowerCase().startsWith("-a")){
                testArgs=argValue.split(" ");
            }
            argIndex+=2;
            if(argIndex<arguments.length){
                nextArg=arguments[argIndex];
            }else{
                throw new RuntimeException("Was expecting more arguments.");
            }
        }
        // 4) TEST  FILE
        testFileName=nextArg;
        argIndex++;
        // 5) ITEM OPTIONS
        if(argIndex<arguments.length){
            //TODO more options were given
        }

        //
        // Check reference
        //
        if(referenceClassName==null){
            referenceClassName=IoUtils.getDefaultClass(referenceFileName);
            if(referenceClassName==null){
                throw new RuntimeException("Could not find IDataObject class to read reference file." + referenceFileName);
            }
        }
        String message = "reference\n";
        message += "\t file: " + referenceFileName + "\n";
        message += "\t class: " + referenceClassName + "\n";
        message += "\t args: ";
        for (String inputArg : referenceArgs) {
            message += inputArg + " ";
        }
        System.out.println(message);
        Results.putMessage(getClass().getSimpleName() + ": " + message);

        //
        // Check test
        //
        if(testClassName==null){
            testClassName=IoUtils.getDefaultClass(testFileName);
            if(testClassName==null){
                throw new RuntimeException("Could not find IDataObject class to read test file.");
            }
        }
        message = "test\n";
        message += "\t file: " + testFileName + "\n";
        message += "\t class: " + testClassName + "\n";
        message += "\t args: ";
        for (String outputArg : testArgs) {
            message += outputArg + " ";
        }
        System.out.println(message);
        Results.putMessage(getClass().getSimpleName() + ": " + message);
    }

    /**
     */
    public boolean compare() {

        String[] exchangeItemIds = referenceDataObject.getExchangeItemIDs();
        boolean result= true;
        System.out.print(String.format(DIFF_OUTPUT_HEADER, this.testFileName));
        System.out.print(String.format(HLINE));

        for (String id : exchangeItemIds) {
            if ( ! validateExchangeItem(referenceDataObject.getDataObjectExchangeItem(id), testDataObject) ) {
                result = false;
            }
        }
        return result;
    }


    private static double rmsDifference(List<Double> d1, List<Double> d2) {
        double rms = 0;
        for (int i = 0; i < d1.size(); i++) {
            rms += Math.pow(d1.get(i) - d2.get(i), 2);
        }
        rms = Math.sqrt(rms )/ d1.size();
        return rms;
    }

    private static double p99Difference(List<Double> d1, List<Double> d2) {
        List<Double> d = new ArrayList(d1.size());
        for (int i = 0; i < d1.size(); i++) {
            d.add(i, Math.abs(d1.get(i) - d2.get(i)) );
        }
        Collections.sort(d);
        int index99 =  (int) Math.round( d.size() *0.99);
        return Collections.max(d.subList(0,index99));
    }


    private static double maxDifference(List<Double> d1, List<Double> d2) {
        List<Double> d = new ArrayList(d1.size());
        for (int i = 0; i < d1.size(); i++) {
            d.add(i, Math.abs(d1.get(i) - d2.get(i) ));
        }
        return Collections.max(d);
    }

    public static boolean almostEquals(double d1, double d2) {
        return almostEquals(d1, d2, EPSILON);
    }

    private static boolean almostEquals(double d1, double d2, double epsilon) {
        return Math.abs(d1 - d2) < epsilon;
    }




}
