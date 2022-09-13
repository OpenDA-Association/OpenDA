/* MOD_V2.0
* Copyright (c) 2013 OpenDA Association
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
package org.openda.model_dflowfm;

import org.openda.exchange.ExchangeItem;
import org.openda.exchange.dataobjects.NetcdfUtils;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.utils.Results;
import org.openda.utils.Vector;
import org.openda.utils.generalJavaUtils.StringUtilities;
import ucar.ma2.Array;
import ucar.ma2.DataType;
import ucar.ma2.InvalidRangeException;
import ucar.nc2.*;

import java.io.File;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

/**
 *  Reading and writing the D-Flow FM restart file <testcase>_map.nc
 */
public class DFlowFMRestartFileWrapper implements IDataObject {

	public static final String EXCHANGE_ITEM_ID_POST_FIX = "exchangeItemIdPostFix";
	private String exchangeItemIdPostFix;

	// create a MetaExchangeItem
	private class DFlowFMMetaExchangeItem{
		public ExchangeItem exchangeItem;
		public boolean fromRestartFile;
		String shortName;
		public int[] nDims=null;

		DFlowFMMetaExchangeItem(ExchangeItem exchangeItem, boolean fromRestartFile, String fullName){
			this.exchangeItem=exchangeItem;
			this.fromRestartFile=fromRestartFile;
			if (fromRestartFile){
				nDims=fullNameToDims(fullName);
				shortName=fullNameToShortName(fullName);
			}
		}
	}

	File workingDir;
	String fileName = null;
	Double Reftime = -1.0;
	int time_index = 0;
	String netcdffileName = null;
	HashMap<String,DFlowFMMetaExchangeItem> ExchangeItems = new LinkedHashMap<String,DFlowFMMetaExchangeItem>();


	/**
	 * Initialize.
	 * The first argument is expected to be the working directory, extra arguments are ignored
	*/
	public void initialize(File workingDir, String[] arguments) {
		this.workingDir = workingDir;
		if (arguments != null) {
			this.fileName = arguments[0];
			try {
				Reftime = Double.parseDouble(arguments[1]);
			} catch (Exception e){
				Results.putMessage("DFlowFMRestartFileWrapper: no time specified, reading values for the last time index");
			}
			for (int i = 1; i < arguments.length; i++) {
				String argument = arguments[i];
				String[] keyValue = StringUtilities.getKeyValuePair(argument);
				if (keyValue == null) continue;
				String key = keyValue[0];
				String value = keyValue[1];
				switch (key) {
					case EXCHANGE_ITEM_ID_POST_FIX:
						exchangeItemIdPostFix = value;
						continue;
					default:
						throw new RuntimeException("Unknown key " + key + ". Please specify only " + EXCHANGE_ITEM_ID_POST_FIX + " as key=value pair");
				}
			}
		}
		NetcdfFile inputFile;
		// check file
		try{
			File fileNameFull = getNetcdfFile(workingDir);
			netcdffileName = fileNameFull.getAbsolutePath();
			inputFile = NetcdfFile.open(netcdffileName, null);
		} catch (Exception e) {
			throw new RuntimeException("DFlowFMRestartFileWrapper: problem opening file "+ this.fileName + " due to " + e.getMessage(), e);
		}

		// get list of all variables and construct a smaller list of exchange variables from this list:
		// all variables that contain geometry information should never be exchanged.
		List<Variable> allVariables = inputFile.getVariables();
		List<Variable> exchangeVariables = new ArrayList<Variable>(allVariables);
		Collections.copy(exchangeVariables,allVariables);
		for (Variable var : allVariables)  {
			if (var.getShortName().contains("time") || var.findDimensionIndex("time") == -1 )  {
				// System.out.println("Excluded: " + var.getShortName());
				exchangeVariables.remove(var);
			}
		}

		// read time information
		double[] times = null;
		for (Variable var : allVariables) {
			if (var.getShortName().contentEquals("time")) {
				try {
					Array timevals = inputFile.readSection(var.getShortName());
					if (timevals.getElementType()==double.class){
						times = (double []) timevals.copyTo1DJavaArray();
					} else if (timevals.getElementType()==float.class){
						float[] sValues= (float []) timevals.copyTo1DJavaArray();
						times = new double[sValues.length];
						for (int j=0; j<sValues.length; j++){
							times[j]=sValues[j];
						}
					}
				} catch (IOException e) {
					throw new RuntimeException("Error reading array 'time' from NetCDF file "+ netcdffileName + " due to " + e.getMessage(), e);
				} catch (InvalidRangeException e) {
					throw new RuntimeException("Error reading from NetCDF file "+ netcdffileName + " due to " + e.getMessage(), e);
				}
			}
		}

		// determine time index, if no time was specified take the last index value.
		double epsilon = 1.E-5;
		if (times != null) {
			time_index = times.length - 1;
			for (int i=0; i < times.length ; i++) {
				if (Math.abs(times[i]-Reftime) < epsilon) {
					time_index = i;
				}
			}
		}
		IGeometryInfo geometryInfo = getGeometryInfo(inputFile);

		// read all variables not containing geometry information
		// full arrays contain information for all times, take a slice for the time index determined above.
		Array thisValue = null;
		for (Variable var : exchangeVariables) {
    		try {
				List<Dimension> dimensions = var.getDimensions();
				int dimensionSize = dimensions.size();
				int[] origin = new int[dimensionSize];
				origin[0] = time_index;
				int[] shape = var.getShape();
				shape[0] = 1;
			    thisValue = var.read(origin, shape);
		    } catch (IOException | InvalidRangeException e) {
				throw new RuntimeException("Error reading from NetCDF file " + netcdffileName + " due to " + e.getMessage(), e);
		    } catch (ArrayIndexOutOfBoundsException e) {
				System.out.println("Error processing "+var.getShortName());
				continue;
				//e.printStackTrace();
			}


		    // Create exchange items
			double[] dValues;
			assert thisValue != null;
			if (thisValue.getElementType()==double.class){
				dValues= (double []) thisValue.copyTo1DJavaArray();
			} else if (thisValue.getElementType()==float.class){
				float[] sValues= (float []) thisValue.copyTo1DJavaArray();
				dValues = new double[sValues.length];
				for (int j=0; j<sValues.length; j++){
					dValues[j]=sValues[j];
				}
			} else {
				// for the moment only doubles and floats are needed
				dValues=null;
//				System.out.println("Array skipped: "+exchangeVariables.get(i).getNameAndDimensions());
			}
			if (dValues!=null){
				String fullName = var.getNameAndDimensions();
				String unitID = null;
				List<Attribute> atts = var.getAttributes();
				for (Attribute temp : atts) {
					if (temp.getShortName().contains("units")) {
						unitID = temp.getStringValue();
					}
				}
				//System.out.println("DEBUG: "+ fullName + " unitID: " + unitID  );
				if (unitID != null) {
					String id = getId(fullName);
					ExchangeItem exchange = new DFlowFMExchangeItem(id, unitID, geometryInfo);
					exchange.setValues(new Vector(dValues));
					double[] timeinfo = { Reftime };
					exchange.setTimes(timeinfo);
					Attribute att = var.findAttribute("standard_name");
					if (att != null) {
						exchange.setQuantityId(att.getStringValue());
					}
					this.ExchangeItems.put(id, new DFlowFMMetaExchangeItem(exchange, true, fullName));
				} else {
					throw new RuntimeException("Error: no unit specified for variable "  + var.getShortName() + " in netcdf file " + netcdffileName);
				}
			}
			inputFile.finish();
		}
		try {
			inputFile.close();
		} catch (IOException e) {
			throw new RuntimeException("Error closing NetCDF file " + netcdffileName + " due to " + e.getMessage(), e);
		}
	}

	private String getId(String fullName) {
		String id = fullNameToShortName(fullName);
		if (exchangeItemIdPostFix == null) return id;
		return id + exchangeItemIdPostFix;
	}

	private File getNetcdfFile(File workingDir) {
		File fileNameFull = new File(workingDir, fileName);
		if (!fileNameFull.exists()) throw new RuntimeException("DFlowFMRestartFileWrapper: restart Input file " + fileNameFull.getAbsolutePath() + " does not exist");
		return fileNameFull;
	}

	private IGeometryInfo getGeometryInfo(NetcdfFile inputFile) {
		Variable variableX = inputFile.findVariable("FlowElem_xcc");
		if (variableX == null) return null;
		String unitsString = variableX.getUnitsString();
		boolean unitInMeters = "m".equals(unitsString);

		Variable variableY = inputFile.findVariable("FlowElem_ycc");
		int[] originXY = createOrigin(variableX);
		if (variableY == null) return null;
		int[] sizeArrayXY = variableX.getShape();
		// For geometryInfo
		double[] xCoords = NetcdfUtils.readSelectedData(variableX, originXY, sizeArrayXY, -1);
		double[] yCoords = NetcdfUtils.readSelectedData(variableY, originXY, sizeArrayXY, -1);

		double[] zCoords = getZCoords(inputFile);

		assert xCoords != null;
		assert yCoords != null;
		return new DFlowFMMapExchangeItemGeometryInfo(xCoords, yCoords, zCoords, unitInMeters);
	}

	private double[] getZCoords(NetcdfFile inputFile) {
		Variable variableZ = inputFile.findVariable("FlowElem_zcc");
		if (variableZ == null) return null;
		int[] originZ = createOrigin(variableZ);
		int[] sizeArrayZ = variableZ.getShape();
		return NetcdfUtils.readSelectedData(variableZ, originZ, sizeArrayZ, -1);
	}

	public String [] getExchangeItemIDs() {
		String [] result = new String[this.ExchangeItems.size()];
		Set<String> keys = this.ExchangeItems.keySet();
		int idx=0;
		for (String key: keys) {
			result[idx]=key;
			idx++;
		}
		return result;
	}

	public String [] getExchangeItemIDs(IExchangeItem.Role role) {
		return getExchangeItemIDs();
	}

	public IExchangeItem getDataObjectExchangeItem(String ExchangeItemID) {
		Set<String> keys = this.ExchangeItems.keySet();
		for (String key: keys) {
			if (ExchangeItemID.equals(key)){
				return this.ExchangeItems.get(key).exchangeItem;
			}
		}
		return null;
	}

	// Write the NetCDF restart file
	public void finish() {
		try {
			NetcdfFileWriter netcdfFileWriter= NetcdfFileWriter.openExisting(netcdffileName);
			netcdfFileWriter.setFill(true);

			//Loop over all exchangeItems
			Set<String> keys = this.ExchangeItems.keySet();
			for(String key : keys){
				// Only consider those exchangeItems that are related to the restart file
				if (this.ExchangeItems.get(key).fromRestartFile){
					IExchangeItem exchangeItem=this.ExchangeItems.get(key).exchangeItem;
					int[] nDims=this.ExchangeItems.get(key).nDims;
					// Get the dimension of the exchangeItem.
					if (nDims!=null){
						Array array=Array.factory(DataType.FLOAT,nDims);
						double[] vals = exchangeItem.getValuesAsDoubles();
						//Check Dimensions
						if (nDims[1] != vals.length) {
							throw new RuntimeException("Array "+this.ExchangeItems.get(key).shortName+", incorrect length");
						}
						int offset = nDims[1]*time_index;
						for (int i=0; i<vals.length; i++){
							array.setFloat(i+offset, (float) vals[i]);
						}
						Variable myVar =  netcdfFileWriter.findVariable(this.ExchangeItems.get(key).shortName);
						try {
							netcdfFileWriter.write(myVar,array);
						} catch (InvalidRangeException e) {
							throw new RuntimeException("Error writing to NetCDF file " + netcdffileName + " due to " + e.getMessage(), e);
						}
					}
					else {
						System.out.println("WARNING: writing variable : "+key+" was skipped.");
					}

				}

			}
			netcdfFileWriter.close();

		} catch (IOException e) {
			throw new RuntimeException("Error writing to NetCDF file " + netcdffileName + " due to " + e.getMessage(), e);
		}
	}

   // find dimensions of a variable
   private int [] fullNameToDims(String fullName){
		// format fullName:
		// u0(time=38, nFlowLink=148)
		int [] nDims=null;
		int istart=fullName.indexOf("(");
		int istop =fullName.indexOf(")");
		if (istart>0 && istop>istart){
			String allDims=fullName.substring(istart+1,istop);
			String[] aDim = allDims.split(",");
			nDims = new int[aDim.length];
			for (int i=0; i<aDim.length; i++){
				String[] str=aDim[i].split("=");
				nDims[i]= Integer.parseInt(str[1]);
			}
		}
	   assert nDims != null;
	   if (nDims.length ==0){
			System.out.println("fullName="+fullName+" NO-DIMENSIONS");
		}
//		else {
//			System.out.println("fullName="+fullName+" nDims"+ Arrays.toString(nDims));
//		}
		return nDims;
	}

	// find shortname of variable
	private String fullNameToShortName(String fullName){
		// format fullName:
		// s1(time=4, nFlowElem=87)
		String[] subStr = fullName.split("\\(");
//		System.out.println("Short name is "+subStr[0]);
		return subStr[0];
	}

	private static int[] createOrigin(Variable var) {
		int dimensionCount = var.getDimensions().size();
		int[] origin = new int[dimensionCount];
		Arrays.fill(origin, 0);
		return origin;
	}
}
