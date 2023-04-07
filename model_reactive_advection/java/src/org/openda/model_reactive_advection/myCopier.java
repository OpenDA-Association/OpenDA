/*
* Copyright (c) 2023 OpenDA Association 
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
package org.openda.model_reactive_advection;
import java.io.File;
import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.interfaces.IExchangeItem;

import static junit.framework.Assert.assertEquals;

/**
 * Copy values for 'c' concentration on grid from one file to another.
 * Arguments [sourceFile] [destFile]
 * Assumes destination exists.
 *
 * @author verlaanm
 *
 */
public class myCopier {

	/**
	 * arg[0] is source file
	 * arg[1] is destination
	 *
	 * @param args
	 */
	public static void main(String args[]){
		if(args.length<2){
			throw new RuntimeException("SimpleBbAsciiCopier.main requires 2 arguments");
		}
		File inputFile = new File(args[0]);
		if(!inputFile.exists()){
			throw new RuntimeException("SimpleBbAsciiCopier.main source file not found:"+args[0]);
		}
		File outputFile = new File(args[1]);
		if(!outputFile.exists()){
			throw new RuntimeException("SimpleBbAsciiCopier.main destination file not found:"+args[1]);
		}


		myWrapper output = new myWrapper();
		String[] ioArgs = {outputFile.getName()};
		output.initialize(outputFile.getParentFile(), ioArgs);
		IExchangeItem outputItem1=null;
		IExchangeItem outputItem2=null;
		String[] ids = output.getExchangeItemIDs();
		for(String id: ids){
			IExchangeItem exchangeItem = output.getDataObjectExchangeItem(id);
			String itemId = exchangeItem.getId();
			assertEquals(id, itemId);
			//System.out.println("looking at item: "+itemId);
			if(itemId.equalsIgnoreCase("concentration1.grid")){
				outputItem1 = exchangeItem;
			}
			if(itemId.equalsIgnoreCase("concentration2.grid")){
				outputItem2 = exchangeItem;
			}
		}

		myWrapper input = new myWrapper();
		ioArgs = new String[]{inputFile.getName()};
		input.initialize(inputFile.getParentFile(), ioArgs);
		ids = input.getExchangeItemIDs();
		for(String id:ids){
			IExchangeItem exchangeItem = input.getDataObjectExchangeItem(id);
			String itemId = exchangeItem.getId();
			assertEquals(id, itemId);
			//System.out.println("looking at item: "+itemId);
			if(itemId.equalsIgnoreCase("concentration1.grid")){
				//System.out.println("changing item: "+itemId);
				double[] values = exchangeItem.getValuesAsDoubles();
				assert outputItem1 != null;
				outputItem1.setValuesAsDoubles(values);
			}
			if(itemId.equalsIgnoreCase("concentration2.grid")){
				//System.out.println("changing item: "+itemId);
				double[] values = exchangeItem.getValuesAsDoubles();
				assert outputItem2 != null;
				outputItem2.setValuesAsDoubles(values);
			}
		}

		input.finish();
		output.finish();
	}

}
