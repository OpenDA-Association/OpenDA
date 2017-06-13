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
package org.openda.model_dflowfm;

import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem.Role;

import java.io.File;
import java.util.ArrayList;
import java.util.Scanner;

/**
 */
public class DFlowFMXyzFile implements IDataObject {
	
	private ArrayList<IExchangeItem> exchangeItems =null;

	public void initialize(File workingDir, String[] arguments) {
		/* Just save the initialization input */
		String fileName     = arguments[0];
		File frictioncoefFile  =  new File(workingDir, fileName);
		String[] fileNameParts =  frictioncoefFile.getName().split("\\.(?=[^\\.]+$)");

		if (arguments.length < 2) {
            throw new RuntimeException("ioObject DFlowFMXyzFile requires one of the following config arguments 'idsFromTemplateFile=', 'idsFromValuesInFile' or 'idsFromKeywordsInFile'.");
		}

		// Get settings from arguments
		String mode = null;		
		String idPrefix = fileNameParts[0] + "_";		
		String templateFileName = null;

		
		for (int i=1 ; i<arguments.length ; i++ ) {
	        //System.out.println(arguments[i]);
			Scanner s = new Scanner(arguments[i]);
			s.useDelimiter("[=]");
		    // read key
			String key = s.next();
			
			//System.out.println("key:" + key);
		    if ( key.equalsIgnoreCase("idsFromTemplateFile") ) {
		    	mode = key;
		    	templateFileName=s.next();
		    }
			else if ( key.startsWith("ids") ) {
		    	mode = key;	    	
		    } else if ( key.equalsIgnoreCase("idPrefix")) {
		    	idPrefix=s.next();
		    } else {
		    }
		    s.close();
		}
		
						
		// Create an arraylist for storing values already found in the input file
		ArrayList<String> foundExchangeItemIds = new ArrayList<String>();
		// Create an arraylist for storing unique exchange items that are found in the input file
		ArrayList<IExchangeItem> listOfExchangeItems= new ArrayList<IExchangeItem>();
		// counter
		//int count = 0;

		/* read the file */
		File templateFile      =  null;		
		if (templateFileName == null) {
			templateFile = frictioncoefFile;
		} else {
			templateFile = new File(workingDir, templateFileName);
		}
		
		String[] sContent = DFlowFMXyzUtils.readWholeFile(templateFile);

		/* Iterate over all lines in the input file */
		for (int iLine=0; iLine < sContent.length; iLine++){

			// Prepare the line for processing
			String line= DFlowFMXyzUtils.prepareLine(sContent[iLine]);

			if(!line.trim().startsWith("#")){
				String exchangeItemId;
				double value;
				if ( mode.equalsIgnoreCase("idsFromTemplateFile") || mode.equalsIgnoreCase("idsFromValuesInFile") ) {
					// Chop line in parts
					// format= x_coor  y_coor  factor
					String [] lineParts=line.split("[^\\s]\\s+", 3);
					// Create exchangeItemId from prefix and value
					int number = (int) Math.round(Double.parseDouble(lineParts[2]));
					exchangeItemId=idPrefix + number;
					value = DFlowFMXyzUtils.readValueFromFile(frictioncoefFile , iLine);
					// Check whether this exchangeItemId is already found
				}
				else if (  mode.equalsIgnoreCase("idsFromKeywordsInFile") ) {
					// Chop line in parts
					// format= x_coor  y_coor  factor #comment
					String [] lineParts=line.split("[^\\s]\\s+", 4);
					// Get exchangeItemId from comment
					exchangeItemId=lineParts[3].substring(1);
					exchangeItemId=exchangeItemId.replaceFirst("^\\s+","").replaceFirst("\\s+$","");
					value = Double.parseDouble( lineParts[2] );
					
				} else {
                    throw new RuntimeException("Mode is not correct: " + mode + " for ioObject DFlowFMXyzFile");
				}
					
				// Search if exchange item already exists
				boolean found = false;
				for (String foundId : foundExchangeItemIds) {
					if (foundId.equalsIgnoreCase(exchangeItemId)) {
						found = true;
						// add line number to existing ExchangeItem for this factor
						for (IExchangeItem item : listOfExchangeItems) {
							if (item instanceof DFlowFMXyzExchangeItem) {
								if (item.getId().contentEquals(exchangeItemId)) {
									((DFlowFMXyzExchangeItem) item).addValue(iLine, value);
								}	
							}
						}
					}
				}
				// If not found create new ExchangeItem
				if (!found) {
					foundExchangeItemIds.add(exchangeItemId);
					// This is a new exchange item
				 	IExchangeItem newExchangeItem=new DFlowFMXyzExchangeItem(exchangeItemId, frictioncoefFile,  value, iLine);
					listOfExchangeItems.add(newExchangeItem);
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
