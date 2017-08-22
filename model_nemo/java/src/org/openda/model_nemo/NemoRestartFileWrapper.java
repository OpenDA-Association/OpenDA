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
package org.openda.model_nemo;

import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.exchange.nemo_exchangeItem;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.*;
import org.openda.utils.Vector;
import ucar.ma2.Array;
import ucar.ma2.DataType;
import ucar.ma2.InvalidRangeException;
import ucar.nc2.NetcdfFile;
import ucar.nc2.NetcdfFileWriter;
import ucar.nc2.Variable;

import java.io.File;
import java.io.IOException;
import java.util.*;


/**
 *
 * Read from ascii file in following format
 *
 */


public class NemoRestartFileWrapper implements IoObjectInterface{

	boolean debug=false;
	boolean debug_writer = false;

    private class NemoMetaExchangeItem{
		public IExchangeItem exchangeItem;
		public boolean fromRestartFile;
		String shortName;
		public int[] nDims=null;

		NemoMetaExchangeItem(IExchangeItem exchangeItem, boolean fromRestartFile, String fullName){
			this.exchangeItem=exchangeItem;
			this.fromRestartFile=fromRestartFile;
			if (fromRestartFile){
				nDims=fullNameToDims(fullName);
				shortName=fullNameToShortName(fullName);
			}
		}
	}



	File workingDir;
	String configString;
	String fileName = null;
	//HashMap<String,IExchangeItem> items = new LinkedHashMap<String,IExchangeItem>();
	HashMap<String,NemoMetaExchangeItem> items = new LinkedHashMap<String,NemoMetaExchangeItem>();
	List<String> namelistContent= new ArrayList<String>();

	//cache these values
	double refdate;
	double tstart;
	double dt=1.0;
	double tstop;
	double unit=1.0;
	String sourceLabels[];
	String boundLabels[];
	String outputLabels[];
	String netcdfFile=null;

	public void initialize(File workingDir, String fileName, String[] arguments) {
		this.workingDir = workingDir;
		this.fileName = fileName;
		if (debug){System.out.println("ioObject : filename = "+fileName);}

		if (arguments != null && arguments.length > 0) {
			if (debug){
				for(int i=0;i<arguments.length;i++){
					System.out.println("ioObject : arg = "+fileName);
				}

				Results.putMessage("ioObject for file="+fileName+" arguments ignored");
			}
			//throw new RuntimeException("IoObject myWrapper does not expect any additional arguments");
		}
		NetcdfFile inputFile = null;
		// check file
		try{
			File fileNameFull= new File(workingDir,fileName);
			if (!fileNameFull.exists()){
				throw new RuntimeException("NemoWrapper: restart Input file "+ fileNameFull.getAbsolutePath()+" does not exist");
			}
			netcdfFile = fileNameFull.getAbsolutePath();
			inputFile = NetcdfFile.open(fileNameFull.getAbsolutePath(), null);
		}catch (Exception e) {
			throw new RuntimeException("NemoWrapper: trouble opening file "+this.fileName+" workdir="+this.workingDir);
		}

		//Get list of all variables
		List<Variable> allVariables = inputFile.getVariables();

		//read all variables
		List<Array> allValues = null;
		try {
			allValues = inputFile.readArrays(allVariables);
		} catch (IOException e) {
			throw new RuntimeException("Error when reading all Arrays from NetCDF file");
		}

        //Create an exchange items for all values
		for (int i=0; i<allValues.size(); i++){
			Array thisValue=allValues.get(i);
			double[] dValues=null;
			if (thisValue.getElementType()==double.class){
				dValues= (double []) thisValue.copyTo1DJavaArray();
			} else if (thisValue.getElementType()==float.class){
				float[] sValues= (float []) thisValue.copyTo1DJavaArray();
				dValues = new double[sValues.length];
				for (int j=0; j<sValues.length; j++){
					dValues[j]=sValues[j];
				}
			} else {
				dValues=null;
				if (debug) System.out.println("Skip Array"+allVariables.get(i).getNameAndDimensions());
			}
			if (dValues!=null){
				String fullName = allVariables.get(i).getNameAndDimensions();
				IExchangeItem exchange = new nemo_exchangeItem(fullNameToShortName(fullName));
				exchange.setValues(new Vector(dValues));
				double times[] ={1.0};
				exchange.setTimes(times);
			   	//IExchangeItem exchange = new DoublesExchangeItem(fullNameToShortName(fullName), IExchangeItem.Role.InOut, dValues);
				this.items.put(allVariables.get(i).getNameAndDimensions(),new NemoMetaExchangeItem(exchange,true,fullName));
			}
		}

		inputFile.finish();
		try {
			inputFile.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		if(debug){
			for(String key: this.items.keySet()){
				System.out.println("key="+key);
				System.out.println(this.items.get(key).toString());
			}
		}
	}

	public IExchangeItem[] getExchangeItems() {
		//TODO for now return some dummy timeSeries
		int n = this.items.size();
		Set<String> keys = this.items.keySet();
		IExchangeItem[] result=new IExchangeItem[n];
		int i=0;
		for(String key : keys){
			result[i]=this.items.get(key).exchangeItem;
			i++;
		}
		return result;
	}

	public void finish() {


		//Write the NETCDF restart file
		try {
			NetcdfFileWriter netcdfFileWriter= NetcdfFileWriter.openExisting(netcdfFile);
			netcdfFileWriter.setFill(true);

			//Loop over all exchangeItems
			int n = this.items.size();
			Set<String> keys = this.items.keySet();
			//IExchangeItem[] result=new IExchangeItem[n];
			for(String key : keys){
				// Only consider those exchangeItems that are related to the restart file
				if (this.items.get(key).fromRestartFile){
					IExchangeItem exchangeItem=this.items.get(key).exchangeItem;
					int[] nDims=this.items.get(key).nDims;
					// Get the dimension of the exchangeItem.
					if (nDims!=null){
						Array array=Array.factory(DataType.FLOAT,nDims);
						double[] vals = exchangeItem.getValuesAsDoubles();
						//Check Dimensions
						//TODO !!!!
						if (debug_writer) System.out.println("Writing array "+this.items.get(key).shortName+" length="+vals.length);
						for (int i=0; i<vals.length; i++){
							array.setFloat(i, (float) vals[i]);
						}
						try {
							Variable myVar = netcdfFileWriter.findVariable(this.items.get(key).shortName);
							netcdfFileWriter.write(myVar,array);
						} catch (InvalidRangeException e) {
							e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
						}
					}
					else {
						if (debug_writer) System.out.println("WARNING: SKIPPING VARIABLE: "+key);
					}

				}

			}
			netcdfFileWriter.close();
			if (debug) System.out.println("Klaar met schrijven");

		} catch (IOException e) {
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}




	}


	private int [] fullNameToDims(String fullName){
		// The full name looks similar to
		// hdivn(t=1, z=11, y=81, x=121)
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
		if (debug){
			if (nDims!=null){
				System.out.println("fullName="+fullName+" nDims"+nDims.toString());
			}
			else {
				System.out.println("fullName="+fullName+" NO-DIMENSIONS");
			}
		}
		return nDims;
	}

	private String fullNameToShortName(String fullName){
		// The full name looks similar to
		// hdivn(t=1, z=11, y=81, x=121)
		String[] subStr = fullName.split("\\(");
		if (debug) System.out.println("Short name is "+subStr[0]);
		return subStr[0];
	}
}
