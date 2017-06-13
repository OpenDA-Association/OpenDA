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
package org.openda.model_swan;

import org.openda.interfaces.*;

import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Locale;

/**
 * Class containing the quantities, locations and value a swan results or observations file
 */
public class SwanResults implements IDataObject {

    ArrayList<SwanResult> swanResultsList;          // ordered list of results
    HashMap<String, SwanResult> swanResultsHashMap; // hash map to be able to find result quickly on id
    private String[] ids;

    private void initialize(File workingDir, String fileName, String[] arguments) {
        // Open the observations file for reading
        File observationsFile = new File(workingDir, fileName);
        if (!observationsFile.exists()) {
            // Specified observation file does not exist. It could be a nested run
            composeObservationsForNestedRun(observationsFile);
        }
        readSwanResultsFile(observationsFile);
    }

//    public IPrevExchangeItem[] getExchangeItems() {
//        IPrevExchangeItem[] exchangeItems = new IPrevExchangeItem[swanResultsList.size()];
//        for (int i = 0; i < exchangeItems.length; i++) {
//            exchangeItems[i] = swanResultsList.get(i);
//        }
//        return exchangeItems;
//    }

    private void readSwanResultsFile(File swanResultsFile) {

        // Open the swanResults file for reading

        FileReader resultsFileReader;
        try {
            resultsFileReader = new FileReader(swanResultsFile);
        } catch (FileNotFoundException e) {
            throw new RuntimeException("File " + swanResultsFile.getAbsolutePath() + " does not exist");
        }
        BufferedReader resultsFileBufferedReader = new BufferedReader(resultsFileReader);

        swanResultsHashMap = new HashMap<String, SwanResult>();
        swanResultsList = new ArrayList<SwanResult>();
        ArrayList<String> quantityIds = new ArrayList<String>();

        String line;
        try {
            line = resultsFileBufferedReader.readLine().trim();
            while (line != null && quantityIds.size() == 0) {
                line = line.trim();
                if (line.startsWith("%") && line.length() > 1) {
                    String quantsLine = line.substring(1).trim();
                    String xPositionKeyWord = "Xp";
                    if (quantsLine.startsWith(xPositionKeyWord)) {
                        String[] quantStrings = quantsLine.split("[ \t]+");
                        String yPositionKeyWord = "Yp";
                        if (!(quantStrings[0].equals(xPositionKeyWord) && quantStrings[1].equals(yPositionKeyWord))) {
                            throw new RuntimeException("Invalid line: " + line);
                        }
                        //noinspection ManualArrayToCollectionCopy
                        for (int i = 2; i < quantStrings.length; i++) {
                            quantityIds.add(quantStrings[i]);
                        }
                    }
                }
                line = resultsFileBufferedReader.readLine();
            }
        } catch (IOException e) {
            throw new RuntimeException("Could not read quantities from swan file " +
                    swanResultsFile.getAbsolutePath() + " (error:" + e.getMessage() + ")");
        }

        int quantityCount = quantityIds.size();
        if (quantityCount == 0) {
            throw new RuntimeException("No quantities available in swan file " +
                    swanResultsFile.getAbsolutePath() + ")");
        }

        try {
            while (line != null) {
                line = line.trim();
                if (line.length() > 0 && !line.startsWith("%")) {
                    String[] valuesAsString = line.split("[ \t]+");
                    // check line length (first 2 positions for x/y coords)
                    if (valuesAsString.length - 2 != quantityCount) {
                        throw new RuntimeException("Unexpected #values (" + (valuesAsString.length - 2) +
                                ") in swan file (#quants.: " + quantityCount + ")");
                    }
                    SwanResult.SwanResultLocation resultLocation = new SwanResult.SwanResultLocation(valuesAsString[0], valuesAsString[1]);
                    for (int i = 2; i < valuesAsString.length; i++) {
                        final double resultValue = Double.valueOf(valuesAsString[i]);
                        if (!(Double.compare(resultValue, SwanResult.MISSING_VALUE) == 0)) {
                            SwanResult result = new SwanResult(resultLocation,
                                    quantityIds.get(i - 2), resultValue);
                            swanResultsHashMap.put(result.getId(), result);
                            swanResultsList.add(result);
                        }
                    }
                }
                line = resultsFileBufferedReader.readLine();
            }
            resultsFileBufferedReader.close();

        } catch (IOException e) {
            throw new RuntimeException("Could not read x/y coords from swan file " +
                    swanResultsFile.getAbsolutePath() + " (error:" + e.getMessage() + ")");
        }

        if (swanResultsHashMap.size() == 0) {
            throw new RuntimeException("No locations and values available in swan file " +
                    swanResultsFile.getAbsolutePath() + ")");
        }

        ids = new String[swanResultsList.size()];
        for (int i = 0; i < ids.length; i++) {
            ids[i] = swanResultsList.get(i).getId();
        }
    }


    /**
     * For the observed values for a series of result/observation ids
     *
     * @param ids The location ids for which to get the values
     * @return The value of re
     */
    public double[] getValues(String[] ids) {
        double[] values = new double[ids.length];
        if (ids == this.ids) {
            // deliver all values
            for (int i = 0; i < values.length; i++) {
                values[i] = (Double) swanResultsList.get(i).getValues();
            }
        } else {
            // provide values ordered according to the incoming ids
            for (int i = 0; i < ids.length; i++) {
                SwanResult swanResult = swanResultsHashMap.get(ids[i]);
                if (swanResult == null) {
                    throw new RuntimeException("Could not find var. " + ids[i] + " in swan values");
                }
                values[i] = (Double) swanResult.getValues();
            }
        }
        return values;
    }

    public String[] getExchangeItemIDs() {
        return ids;
    }

    public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
        return getExchangeItemIDs();
    }

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		int indexExchangeItem = Integer.MAX_VALUE;
		for (int i = 0; i < swanResultsList.size(); i++) {
			if (exchangeItemID.equals(swanResultsList.get(i).getId())) {
				indexExchangeItem = i;
				break;
			}
		}
		if (indexExchangeItem == Integer.MAX_VALUE) {
			throw new RuntimeException("unknown exchange item: " + exchangeItemID);
		} else {
			return swanResultsList.get(indexExchangeItem);
		}
	}

    public void finish() {
        // no action needed;
    }

    public static void composeObservationsForNestedRun(File observationsFile) {

        final String locTabString = "_loc.tab";
        String baseFileName = observationsFile.getName();
        int locTabPos = baseFileName.indexOf(locTabString);
        if (locTabPos < 0) return;
        baseFileName = baseFileName.substring(0, locTabPos);

        File observationsDir = observationsFile.getParentFile();
        Locale locale = new Locale("EN");
        ArrayList<File> numberedFiles = new ArrayList<File>();
        for (int i = 1; i < 10; i++) {
            File numberedFile = new File(observationsDir, baseFileName + "_" +  String.format(locale, "%02d", i) + locTabString);
            if (numberedFile.exists()) {
                numberedFiles.add(numberedFile);
            }
        }
        if (numberedFiles.size() > 0) {
            File currentNumberedFile = null;
            try {
                BufferedWriter writer = new BufferedWriter(new FileWriter(observationsFile));
                boolean writeHeader = true;
                for (File numberedFile : numberedFiles) {

                    currentNumberedFile = numberedFile;
                    BufferedReader reader = new BufferedReader(new FileReader(numberedFile));

                    String lineFromNumberFile = reader.readLine();
                    while (lineFromNumberFile !=null) {
                        if (lineFromNumberFile.trim().startsWith("%")) {
                            if (writeHeader) {
                                writer.write(lineFromNumberFile);
                                writer.newLine();
                            }
                        } else {
                            writer.write(lineFromNumberFile);
                            writer.newLine();
                        }
                        lineFromNumberFile = reader.readLine();
                    }
                    writeHeader = false;
                }
                currentNumberedFile = null;
                writer.close();
            } catch (IOException e) {
                throw new RuntimeException("Error composing nested observation file " +
                        observationsFile.getAbsolutePath() +
                        (currentNumberedFile == null ? "" : " when reading from " + currentNumberedFile.getAbsolutePath()) +
                        ", error: " + e.getMessage());
            }
        }
    }

    public void initialize(File workingDir, String[] arguments) {
        String fileName = arguments[0];
        String[] remainingArguments = new String[arguments.length-1];
        System.arraycopy(arguments, 1, remainingArguments, 0, remainingArguments.length);
        initialize(workingDir, fileName, remainingArguments);
    }

    /**
     * A Swan result value or meaurement (quantity/location/value).
     */
    public static class SwanResult implements IExchangeItem{

        public static final double MISSING_VALUE = -999.0;
        public static final String QUANT_LOC_DELIMITER =" @ ";

        private SwanResultLocation SwanResultLocation;
        private String quantityId;
        private double value;

        public SwanResult(SwanResultLocation SwanResultLocation, String quantityId, double value) {
            this.SwanResultLocation = SwanResultLocation;
            this.quantityId = quantityId;
            this.value = value;
        }

        public SwanResultLocation getLocation() {
            return SwanResultLocation;
        }

        public String getQuantityId() {
            return quantityId;
        }

        public String getId() {
            return quantityId + QUANT_LOC_DELIMITER + SwanResultLocation.getId();
        }

        public String getDescription() {
            return null;  // no description
        }

        public Class getValueType() {
            return Double.class;
        }

        public ValueType getValuesType() {
            return ValueType.doubleType;
        }

        public IPrevExchangeItem.Role getRole() {
            return IPrevExchangeItem.Role.Output;
        }

        public Object getValues() {
            return value;
        }

        public double[] getValuesAsDoubles() {
            return new double[]{value};
        }

		public void setValues(Object values) {
			throw new RuntimeException(this.getClass().getName() +
					"setValues(): neither allowed nor useful for output item");
		}

		public void copyValuesFromItem(IExchangeItem sourceItem) {
			if (sourceItem.getValueType() != getValueType()) {
				throw new RuntimeException("Incompatible value types in copy action from " + sourceItem.getId() +
						" to " + getId() + "(" + sourceItem.getValueType().toString() + "/=" + getValueType().toString());
			}
			setValues(sourceItem.getValues());
		}

		public void setValuesAsDoubles(double[] values) {
			throw new RuntimeException(this.getClass().getName() +
					"setValuesAsDoubles(): neither allowed nor useful for output item");
		}

		public ITimeInfo getTimeInfo() {
			throw new UnsupportedOperationException(this.getClass().getName() + ".getTimeInfo(): Not implemented yet.");
		}

		public IQuantityInfo getQuantityInfo() {
			throw new UnsupportedOperationException(this.getClass().getName() + ".getQuantityInfo(): Not implemented yet.");
		}

		public IGeometryInfo getGeometryInfo() {
			return null;
		}

		public void axpyOnValues(double alpha, double[] axpyValues) {
				throw new RuntimeException(this.getClass().getName() +
						"axpyOnValues(): neither allowed nor useful for output item");
		}

		public void multiplyValues(double[] multiplicationFactors) {
			throw new RuntimeException(this.getClass().getName() +
					"multiplyValues(): neither allowed nor useful for output item");
		}

        public double[] getTimes() {
            return null;
        }

        public void setTimes(double[] times) {
            throw new RuntimeException(this.getClass().getName() + " does not have timeStamps");
        }

        /**
         * Location of a swan result or measurement value.
         */
        public static class SwanResultLocation {

            private Double x;
            private Double y;

            public SwanResultLocation(String xCoord, String yCoord) {
                this.x = Double.valueOf(xCoord);
                this.y = Double.valueOf(yCoord);
            }

            public Double getX() {
                return x;
            }

            public Double getY() {
                return y;
            }

            public String getId() {
                return x + "," + y;
            }
        }
    }
}
