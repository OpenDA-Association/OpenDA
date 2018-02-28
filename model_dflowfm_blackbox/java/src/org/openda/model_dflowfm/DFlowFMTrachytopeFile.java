/* OpenDA v2.4.3 
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

package org.openda.model_dflowfm;

import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem.Role;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;

/**
 * Started with a copy from model_delft3d
 * Adjusted to syntax differences for delft3d-FM alias DFLOW or D-Hydro
 */
public class DFlowFMTrachytopeFile implements IDataObject {

	private File workingDir = null;
	private String configString = null;
	private DFlowFMRougnessFileExchangeItem[] exchangeItems =null;
	private HashMap<String,Integer> index = new HashMap<String,Integer>(); 
	private String[] sContent=null;
	private File roughFile=null;

	public void initialize(File workingDir, String[] arguments) {
		/* Just save the initialization input */
		this.workingDir    = workingDir;
		this.configString = arguments[0];

		// Create an arraylist for storing all exchange items that are found in the input file
		ArrayList<DFlowFMRougnessFileExchangeItem> listOfExchangeItems= new ArrayList<DFlowFMRougnessFileExchangeItem>();

		/* read the parameters file */
		roughFile=new File(this.workingDir, this.configString);
		this.sContent = DFlowFMRoughnessUtils.readWholeFile(roughFile);
		String[] labels = {"A","B","C","D","E"};

		/* Iterate over all lines in the intput file 
		 * and parse the input
		 */
		int lastCode=-1;
		String lastType=""; // "" or "DISCHARGE" or "WATERLEVEL" where "" means no dependency
		for (int iLine=0; iLine < sContent.length; iLine++){
			String line=sContent[iLine];
			if(line.contains("#")){
				int hashIndex=line.indexOf("#");
				if(hashIndex>0){
					line=line.substring(0, hashIndex-1);
				}else if(hashIndex==0){
					line="";
				}
			}
			if(line.trim().length()==0){
				continue; //empty line
			} 
			int thisCode=-1;
			String [] lineParts=line.split("\\s+");
			try{thisCode=Integer.parseInt(lineParts[0]);}catch(NumberFormatException e){
				throw new RuntimeException("Expecting first columns to contain the parameter code for line:"+line);
			}
			if(lineParts[1].equalsIgnoreCase("WATERLEVEL")){
				// 674 WATERLEVEL H-PannkopNijmegen <==HERE
				// 674 8.50 101 0.0759 2.5
				// 674 10.00 101 0.0806 2.5
				// 674 12.50 101 0.0971 2.5
				lastType="WATERLEVEL";
				lastCode=thisCode;
			}else if(lineParts[1].equalsIgnoreCase("DISCHARGE")){
				// 673 DISCHARGE Q-PannkopNijmegen <==HERE
				// 673 2000 101 0.0759 2.5
				// 673 5000 101 0.0806 2.5
				// 673 8000 101 0.0971 2.5
				lastType="DISCHARGE";
				lastCode=thisCode;
			}else if(thisCode==lastCode){
				// 673 DISCHARGE Q-PannkopNijmegen 
				// 673 2000 101 0.0759 2.5
				// 673 5000 101 0.0806 2.5 <==HERE
				// 673 8000 101 0.0971 2.5
				double depValue=Double.NaN;
				try{depValue=Double.parseDouble(lineParts[1]);}catch(NumberFormatException e){
					throw new RuntimeException("Expecting second column to contain the discharge value for line:"+line);
				}
				int FormulaNumber=-1;
				try{FormulaNumber=Integer.parseInt(lineParts[2]);}catch(NumberFormatException e){
					throw new RuntimeException("Expecting third column to contain the formula number for line:"+line);
				}
				int firstValueColumn=3; //java-counting style
				String id="RoughNr_"+thisCode+"_"+lastType+lineParts[1]+"_FormulaNr"+FormulaNumber+"_"; //still to add A,B,C or D
				for(int iColumn=firstValueColumn;iColumn<lineParts.length;iColumn++){
					double value=Double.NaN;
					try{value=Double.parseDouble(lineParts[iColumn]);}catch(NumberFormatException e){
						throw new RuntimeException("Expecting column "+(iColumn+1)+" to contain a parameter value for line:"+line);
					}
					DFlowFMRougnessFileExchangeItem tempItem=new DFlowFMRougnessFileExchangeItem(id+labels[iColumn-firstValueColumn],iLine,iColumn,value);
					listOfExchangeItems.add(tempItem);
				}
			}else{
				lastCode=thisCode;
				lastType="";
				//#nieuwe ruwheidscode voor de vaste laag bij St. Andries
				//1001  52 35.0 <==HERE
				//#410 is missing; using the same values as 411
				//410   101  0.0800   2.5 <==OR HERE
				int FormulaNumber=-1;
				try{FormulaNumber=Integer.parseInt(lineParts[1]);}catch(NumberFormatException e){
					throw new RuntimeException("Expecting second column to contain the formula number for line:"+line);
				}
				int firstValueColumn=2; //java-counting style
				String id="RoughNr_"+thisCode+"_FormulaNr"+FormulaNumber+"_"; //still to add A,B,C or D
				for(int iColumn=firstValueColumn;iColumn<lineParts.length;iColumn++){
					double value=Double.NaN;
					try{value=Double.parseDouble(lineParts[iColumn]);}catch(NumberFormatException e){
						throw new RuntimeException("Expecting column "+(iColumn+1)+" to contain a parameter value for line:"+line);
					}
					DFlowFMRougnessFileExchangeItem tempItem=new DFlowFMRougnessFileExchangeItem(id+labels[iColumn-firstValueColumn],iLine,iColumn,value);
					listOfExchangeItems.add(tempItem);
				}
			}
		}

		this.exchangeItems = new DFlowFMRougnessFileExchangeItem[listOfExchangeItems.size()];
		listOfExchangeItems.toArray(this.exchangeItems);
		createIndex(this.exchangeItems,this.index);
	}

	public void finish() {
		DFlowFMRoughnessUtils.updateContent(this.sContent, this.exchangeItems);
		DFlowFMRoughnessUtils.writeWholeFile(this.roughFile, this.sContent);
	}

	public String toString(){
		String result="DFlowFM Roughness parameters dataObject (org.openda.model_dflowfm.DFlowFMTrachytopeFile)\n";
		for(DFlowFMRougnessFileExchangeItem item: this.exchangeItems){
			result+=item.toString()+"\n";
		}
		return result;
	}

	@Override
	public String[] getExchangeItemIDs() {
		int n=this.exchangeItems.length;
		String[] result=new String[n];
		for(int i=0;i<n;i++){
			result[i]=this.exchangeItems[i].getId();
		}
		return result;
	}

	@Override
	public String[] getExchangeItemIDs(Role role) {
		if(role==Role.InOut){
			return getExchangeItemIDs();
		}else{
			return new String[0];
		}
	}

	@Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		if(this.index.containsKey(exchangeItemID)){
			int i=this.index.get(exchangeItemID);
			return this.exchangeItems[i];
		}else{
			return null;
		}
	}

	private void createIndex(DFlowFMRougnessFileExchangeItem[] items, HashMap<String,Integer> index){
		int i=0;
		for(DFlowFMRougnessFileExchangeItem item: items){
			String id=item.getId();
			this.index.put(id, i);
			i++;
		}
	}
}
