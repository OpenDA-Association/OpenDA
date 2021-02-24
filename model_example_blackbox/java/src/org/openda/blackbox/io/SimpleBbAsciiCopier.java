/*
* Copyright (c) 2021 OpenDA Association 
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
package org.openda.blackbox.io;

import org.openda.exchange.AbstractDataObject;
import org.openda.interfaces.IExchangeItem;

import java.io.File;

/**
 * Copy values for 'c' concentration on a grid from one file to another.
 * Arguments [sourceFile] [destFile]
 * Assumes destination exists.
 *
 * @author verlaanm
 *
 */
public class SimpleBbAsciiCopier {

	/**
	 * arg[0] is source file
	 * arg[1] is destination
	 *
	 * @param args corresponding arguments
	 */
	public static void main(String[] args){
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


		AbstractDataObject output = new SimpleBbAsciiFile();
		String[] ioArgs = {outputFile.getName()};
		output.initialize(outputFile.getParentFile(), ioArgs);
		IExchangeItem outputItem=null;
		for(String id:output.getExchangeItemIDs()){
			System.out.println("looking at item: "+id);
			if(id.equalsIgnoreCase("concentration.grid")){
				outputItem = output.getDataObjectExchangeItem(id);
				break;
			}
		}

		AbstractDataObject input = new SimpleBbAsciiFile();
		ioArgs = new String[]{inputFile.getName()};
		input.initialize(inputFile.getParentFile(), ioArgs);
		for(String id:input.getExchangeItemIDs()){
			System.out.println("looking at item: "+id);
			if(id.equalsIgnoreCase("concentration.grid")){
				System.out.println("changing item: "+id);
				IExchangeItem inputItem = input.getDataObjectExchangeItem(id);
				double[] values = inputItem.getValuesAsDoubles();
				outputItem.setValuesAsDoubles(values);
				break;
			}
		}

		input.finish();
		output.finish();
	}

}
