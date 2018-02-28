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

import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem.Role;

import java.io.File;
import java.util.ArrayList;

/**
 * @deprecated
 * This class is replaced by the generic DFlowFMXynFile class
 */
public class DflowfmFrictionCoefficientFile implements IDataObject {

	private ArrayList<IExchangeItem> exchangeItems =null;

	public void initialize(File workingDir, String[] arguments) {
		/* Just save the initialization input */
		String configString = arguments[0];

		// Create an arraylist for storing values already found in the input file
		ArrayList<String> listOfFactors = new ArrayList<String>();
		// Create an arraylist for storing unique exchange items that are found in the input file
		ArrayList<IExchangeItem> listOfExchangeItems= new ArrayList<IExchangeItem>();
		// counter
		int count = 0;

		/* read the file */
		File frictioncoefFile  =new File(workingDir, configString);
		String[] sContent = DflowFMFrictionCoefficientUtils.readWholeFile(frictioncoefFile);

		/* Iterate over all lines in the input file */
		for (int iLine=0; iLine < sContent.length; iLine++){

			// Prepare the line for processing
			String line= DflowFMFrictionCoefficientUtils.prepareLine(sContent[iLine]);

			if(!line.trim().startsWith("#")){
				// Chop line in parts
				// format= x_coor  y_coor  factor
				String [] lineParts=line.split(" ");

				// Check whether the line has the expected format
				if (lineParts.length==3){

					String factor=lineParts[2];

					// Check whether this factor is already on the list of factors found
					boolean found = false;
					for (String fac : listOfFactors) {
						if (fac.equalsIgnoreCase(factor)) {
							found = true;
							// add line number to existing ExchangeItem for this factor
							for (IExchangeItem item : listOfExchangeItems) {
								if (item instanceof DFlowFMFrictionCoefExchangeItem) {
									String id = item.getId();
									int tmp = 1 + listOfFactors.indexOf(factor);
									if (id.contentEquals("friction_"+tmp)) {
										((DFlowFMFrictionCoefExchangeItem) item).addlineNum(iLine);
									}
								}
							}
						}
					}
					if (!found) {
						count++;
						listOfFactors.add(factor);
						// Construct the ID
						String id="friction_"+count;

						// Add exchange item
					 	IExchangeItem newExchangeItem=new DFlowFMFrictionCoefExchangeItem(id, frictioncoefFile,iLine);
						listOfExchangeItems.add(newExchangeItem);
					}

				}
			}
		}
		this.exchangeItems = listOfExchangeItems;
	}

	
	public String[] getExchangeItemIDs() {
		ArrayList<String> exchangeItemIDs = new ArrayList<String>();
		for (IExchangeItem item : this.exchangeItems) {
    		exchangeItemIDs.add(item.getId());
		}
		return exchangeItemIDs.toArray(new String[exchangeItemIDs.size()]);
	}

	
	public String[] getExchangeItemIDs(Role role) {
		ArrayList<String> exchangeItemIDs = new ArrayList<String>();
		for (IExchangeItem item : this.exchangeItems) {
			if (item.getRole().equals(role)) {
			   exchangeItemIDs.add(item.getId());
			}
		}
		return exchangeItemIDs.toArray(new String[exchangeItemIDs.size()]);
	}

	
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		for (IExchangeItem exchangeItem : this.exchangeItems) {
			if (exchangeItem.getId().equals(exchangeItemID)) {
				return exchangeItem;
			}
		}
		return null;
	}

	public void finish() {
		// no action needed
	}

}
