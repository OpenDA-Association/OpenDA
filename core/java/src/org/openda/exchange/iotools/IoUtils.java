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
import java.io.Writer;
import java.util.HashMap;
import java.util.Set;

import org.openda.exchange.ArrayGeometryInfo;
import org.openda.interfaces.IArray;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.ITimeInfo;
import org.openda.interfaces.IExchangeItem.ValueType;
import org.openda.utils.Reflection;

/**
 * This class contains utility methods for IDataObjects and IExchangeItems.
 *
 * @author Arno Kockx
 */
public class IoUtils {
	
	/**
	 * Creates an IDataObject as specified by the given arguments.
	 *
	 * @param filePath full pathname of file.
	 * @param className of IDataObject to use.
	 * @param arguments optional one or more arguments that are passed to the IDataObject initialize method.
	 * @return the created IDataObject.
	 */
	public static IDataObject initializeDataObject(String filePath, String className, String[] arguments) {
		//create data object.
		Object instance;
		try {
			Class<?> dataClass = Class.forName(className);
			instance = dataClass.newInstance();
		} catch (ClassNotFoundException e) {
			throw new RuntimeException("ClassNotFoundException while creating " + className, e);
		} catch (InstantiationException e) {
			throw new RuntimeException("InstantiationException while creating " + className, e);
		} catch (IllegalAccessException e) {
			throw new RuntimeException("IllegalAccessException while creating " + className, e);
		}
		if (!(instance instanceof IDataObject)) {
			throw new RuntimeException(className + " is not implementing the IDataObject interface.");
		}
		IDataObject dataObject = (IDataObject) instance;

		//initialize data object.
		File file = new File(filePath);
		File workingDir = file.getParentFile();
		String fileName = file.getName();
		String[] newArguments = new String[arguments.length + 1];
		newArguments[0] = fileName;
		System.arraycopy(arguments, 0, newArguments, 1, arguments.length);
		dataObject.initialize(workingDir, newArguments);

		return dataObject;
	}

	public static void writeExchangeItem(Writer writer, IExchangeItem exchangeItem) throws IOException {
		//write general info.
		writer.write("id = " + exchangeItem.getId() + "\n");
		writer.write("  description = " + exchangeItem.getDescription() + "\n");
		writer.write("  role = " + exchangeItem.getRole() + "\n");

		//write quantity info.
		writer.write("  quantityInfo:\n");
		writeQuantityInfo(writer, exchangeItem.getQuantityInfo(), "	");

		//write geometry info.
		writer.write("  geometryInfo:\n");
		IGeometryInfo geometryInfo = exchangeItem.getGeometryInfo();
		if (geometryInfo != null) {
			if (geometryInfo instanceof ArrayGeometryInfo) {
				ArrayGeometryInfo arrayGeometryInfo = (ArrayGeometryInfo) geometryInfo;

				writer.write("    latitude:\n");
				writeQuantityInfo(writer, arrayGeometryInfo.getLatitudeQuantityInfo(), "      ");
				int[] latitudeValueIndices = arrayGeometryInfo.getLatitudeValueIndices();
				if (latitudeValueIndices != null) {
					writer.write("      valueIndices =");
					for (int n = 0; n < latitudeValueIndices.length; n++) {
						writer.write(" " + latitudeValueIndices[n]);
					}
					writer.write("\n");
				}
				writer.write("      coordinateValues:\n");
				writeValues(writer, ValueType.IArrayType, arrayGeometryInfo.getLatitudeArray(), "        ");

				writer.write("    longitude:\n");
				writeQuantityInfo(writer, arrayGeometryInfo.getLongitudeQuantityInfo(), "      ");
				int[] longitudeValueIndices = arrayGeometryInfo.getLongitudeValueIndices();
				if (longitudeValueIndices != null) {
					writer.write("      valueIndices =");
					for (int n = 0; n < longitudeValueIndices.length; n++) {
						writer.write(" " + longitudeValueIndices[n]);
					}
					writer.write("\n");
				}
				writer.write("      coordinateValues:\n");
				writeValues(writer, ValueType.IArrayType, arrayGeometryInfo.getLongitudeArray(), "        ");
			}
		}

		//write time info.
		writer.write("  timeInfo:\n");
		ITimeInfo timeInfo = exchangeItem.getTimeInfo();
		if (timeInfo != null) {
			writer.write("    times:\n");
			writeValues(writer, ValueType.doublesType, timeInfo.getTimes(), "      ");
		}

		//write values.
		writer.write("  values:\n");
		writeValues(writer, exchangeItem.getValuesType(), exchangeItem.getValues(), "    ");

		writer.write("\n");
	}
	
	/**
	 * Return the name of the class that most likey can read or write this file.
	 * @param fileName
	 * @return classname of IDataObject
	 */
	public static String getDefaultClass(String fileName){
		String result=null;
		HashMap<String,String> classNames = new HashMap<String,String>();
		classNames.put("noos","org.openda.exchange.dataobjects.NoosDataObject");
		classNames.put("tab","org.openda.model_swan.SwanResultsTimeDependent");
		classNames.put("xml","nl.deltares.openda.fews.io.PiTimeSeriesDataObject");
		classNames.put("nc", "org.openda.exchange.dataobjects.NetcdfDataObject");
		classNames.put("txt", "org.openda.exchange.dataobjects.TestDataObject");

		String extension=fileName.replaceFirst(".*\\.", "");
		if(classNames.containsKey(extension.toLowerCase())){
			result=classNames.get(extension.toLowerCase());
		}
		return result;
	}

	private static void writeValues(Writer writer, ValueType valueType, Object valueObject, String indentation) throws IOException {
		if (valueObject == null) {
			//do nothing.
			return;
		}

		double[] values;
		switch (valueType) {
			case IArrayType:
				values = ((IArray) valueObject).getValuesAsDoubles();
				break;
			case doublesType:
				values = (double[]) valueObject;
				break;
			default:
				throw new UnsupportedOperationException(IoUtils.class.getSimpleName() + ": writing values of type "
						+ valueType + " not implemented yet.");
		}

		if (values != null) {
			for (int n = 0; n < values.length; n++) {
				writer.write(indentation + values[n] + "\n");
			}
		}
	}

	private static void writeQuantityInfo(Writer writer, IQuantityInfo quantityInfo, String indentation) throws IOException {
		if (quantityInfo != null) {
			writer.write(indentation + "quantityId = " + quantityInfo.getQuantity() + "\n");
			writer.write(indentation + "unitId = " + quantityInfo.getUnit() + "\n");
		}
	}
}
