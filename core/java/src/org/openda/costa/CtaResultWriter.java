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
package org.openda.costa;

import org.openda.interfaces.IInstance;
import org.openda.interfaces.IResultWriter;
import org.openda.interfaces.IVector;

/**
 * Created with IntelliJ IDEA.
 * User: nils
 * Date: 7/25/13
 * Time: 10:05 AM
 * To change this template use File | Settings | File Templates.
 */
public class CtaResultWriter implements IResultWriter {

	/**
	 * Put a message.
	 *
	 * @param source  the producer of the message (algorithm, model or observer)
	 * @param message message to be written to output
	 */
	public void putMessage(Source source, String message) {
		//To change body of implemented methods use File | Settings | File Templates.
	}

	/**
	 * Put a message.
	 *
	 * @param source  the producer of the message (algorithm, model or observer)
	 * @param message message to be written to output
	 */
	public void putMessage(IInstance source, String message) {
		//To change body of implemented methods use File | Settings | File Templates.
	}

	/**
	 * Put a vector, indicating the algorithm's current iteration.
	 *
	 * @param source      the producer of the message (algorithm, model or observer)
	 * @param id          the output vector's identifier
	 * @param result      the output item
	 * @param outputLevel the level of output group
	 * @param context     context where the value is written
	 * @param iteration   current iteration
	 */
	public void putValue(Source source, String id, Object result, OutputLevel outputLevel, String context, int iteration) {
		//To change body of implemented methods use File | Settings | File Templates.
	}

	/**
	 * Put a report on the iteration.
	 *
	 * @param source     the producer of the message (algorithm, model or observer)
	 * @param iteration  iteration index
	 * @param cost       cost function
	 * @param parameters parameters used in the computation
	 */
	public void putIterationReport(IInstance source, int iteration, double cost, IVector parameters) {
		//To change body of implemented methods use File | Settings | File Templates.
	}

	/**
	 * Give the default maximum size of result item that can be printed to the result file by this result writer.
	 *
	 * @return int defaultMaxSize
	 */
	public int getDefaultMaxSize() {
		return 0;  //To change body of implemented methods use File | Settings | File Templates.
	}

	/**
	 * Free the ResultWriter instance.
	 */
	public void free() {
		//To change body of implemented methods use File | Settings | File Templates.
	}
}
