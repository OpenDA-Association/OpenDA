/* MOD_V2.0
 * Copyright (c) 2016 OpenDA Association
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

package org.openda.model_delft3d;

import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.interfaces.IExchangeItem;
import java.io.File;
import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA.
 * User: nils
 * Date: May 8, 2009
 * Time: 2:49:06 PM
 */
public class D3dRoughParamsFile implements IoObjectInterface {

	private File workingDir = null;
	private String configString = null;
	private IExchangeItem[] exchangeItems =null;

	public void initialize(File workingDir, String configString, String[] arguments) {
		/* Just save the initialization input */
		this.workingDir    = workingDir;
		this.configString = configString;

		/*Not sure what will be the content of  fileName
         This can be a whole XML-file specifying what parameters we will use but for now
         we assume it is only a filename later we can make it more luxurious
		 */

		// Create an arraylist for storing all exchange items that are found in the input file
		ArrayList<IExchangeItem> listOfExchangeItems= new ArrayList();

		/* read the parameters file */
		File roughFile  =new File(workingDir, configString);
		String[] sContent = D3dRoughParamsUtils.readWholeFile(roughFile);

		/* Iterate over all lines in the intput file */
		for (int iLine=0; iLine < sContent.length; iLine++){

			// Prepare the line for processing
			String line=D3dRoughParamsUtils.prepareRoughLine(sContent[iLine]);

			if(!line.trim().startsWith("#")){
				// Chop line in parts
				// format=
				// 411 101 0.1000 2.5
				// R_CODE PARAM A B
				String [] lineParts=line.split(" ");

				// Check whether the line starts with R_CODE
				if (lineParts.length>2){
					boolean hasA=false;
					boolean hasB=false;
					boolean hasC=false;
					boolean hasD=false;
					boolean hasE=false;

					String sCode=lineParts[0];
					String sFormulaNumber=lineParts[1];

					if(lineParts.length>=3){ // first parameter exists
						hasA=true;

					}
					if(lineParts.length>=4){ // second parameter exists
						hasB=true;

					}
					if(lineParts.length>=5){ // third parameter exists
						hasC=true;

					}
					if(lineParts.length>=6){ // fourth parameter exists
						hasD=true;

					}
					if(lineParts.length>=7){ // fifth parameter exists
						hasE=true;

					}

					// Construct the ID
					String id="RoughNr_"+sCode;
					if (!sFormulaNumber.equals("")){
						id=id+"_FormulaNr"+sFormulaNumber;
					}

					// Add exchange items for all parameters (if they exist)
					if (hasA){
						String idA=id+"_A";
						IExchangeItem newExchangeItem=new D3dRoughParamsFileExchangeItem(idA, roughFile, iLine, "A");
						listOfExchangeItems.add(newExchangeItem);
					}
					if (hasB){
						String idB=id+"_B";
						IExchangeItem newExchangeItem=new D3dRoughParamsFileExchangeItem(idB, roughFile, iLine, "B");
						listOfExchangeItems.add(newExchangeItem);
					}
					if (hasC){
						String idC=id+"_C";
						IExchangeItem newExchangeItem=new D3dRoughParamsFileExchangeItem(idC, roughFile, iLine, "C");
						listOfExchangeItems.add(newExchangeItem);
					}
					if (hasD){
						String idD=id+"_D";
						IExchangeItem newExchangeItem=new D3dRoughParamsFileExchangeItem(idD, roughFile, iLine, "D");
						listOfExchangeItems.add(newExchangeItem);
					}
					if (hasE){
						String idD=id+"_E";
						IExchangeItem newExchangeItem=new D3dRoughParamsFileExchangeItem(idD, roughFile, iLine, "E");
						listOfExchangeItems.add(newExchangeItem);
					}
				}
			}
		}
		this.exchangeItems = new IExchangeItem[listOfExchangeItems.size()];
		listOfExchangeItems.toArray(this.exchangeItems);
	}

	public IExchangeItem[] getExchangeItems() {
		return this.exchangeItems;
	}

	public void finish() {
		// no action needed
	}
}
