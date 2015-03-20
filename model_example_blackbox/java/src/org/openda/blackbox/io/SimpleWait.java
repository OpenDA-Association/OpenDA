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
package org.openda.blackbox.io;
import org.openda.utils.Results;


/**
 * org.openda.blackbox.io.SimpleWait can be used to delay execution in a blackboxwrapper.
 * The main function has one argument; the delay in seconds.
 * @author verlaanm
 *
 */
public class SimpleWait {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		if(args.length<1){
			throw new RuntimeException("SimpleWait.main requires 1 argument: delay in seconds, eg. 3.0");
		}
		double delay=1.0;
		try {
			delay = Double.parseDouble(args[0]);			
		} catch (Exception e) {
			throw new RuntimeException("SimpleWait.main: argument could not be parsed as double: '"+args[0]+"'");
		}
		Results.putMessage("starting wait for "+delay+"seconds.");
		try {
			Thread.sleep((int)(delay*1000));
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		Results.putMessage("wait finished.");
	}

}
