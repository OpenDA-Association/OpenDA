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

package org.openda.resultwriters;

import org.openda.costa.CtaInitialize;
import org.openda.costa.CtaObject;
import org.openda.costa.CtaVector;
import org.openda.interfaces.IInstance;
import org.openda.interfaces.IResultWriter;
import org.openda.interfaces.IVector;

import java.io.File;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Created with IntelliJ IDEA.
 * User: nils
 * Date: 7/3/13
 * Time: 9:29 PM
 */
public class NativeResultWriter implements IResultWriter {

static {
	CtaInitialize.initialize();
}


	static private AtomicInteger resultWriterID = new AtomicInteger(0);
	private int myID=0;
	private String configString;
	private String workingDir;

	public NativeResultWriter(File workingDir, String configString){
		this.myID         = resultWriterID.incrementAndGet();
		this.configString = configString;
		this.workingDir   = workingDir.getAbsolutePath();
	}

	/**
	 * Put a message.
	 *
	 * @param source  the producer of the message (algorithm, model or observer)
	 * @param message message to be written to output
	 */
	public void putMessage(Source source, String message) {

		this.putMessage(this.myID, this.configString, this.workingDir, message);
	}

	/**
	 * Put a message.
	 *
	 * @param source  the producer of the message (algorithm, model or observer)
	 * @param message message to be written to output
	 */
	public void putMessage(IInstance source, String message) {
		this.putMessage(this.myID, this.configString, this.workingDir, message);
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
        int handle=0;
		//Translate the output level to a string
		int outLevel=0;
		if (outputLevel == OutputLevel.All){
			outLevel=4;
		}else if (outputLevel == OutputLevel.Verbose){
			outLevel=3;
		}else if (outputLevel == OutputLevel.Essential){
			outLevel=2;
		}else if (outputLevel == OutputLevel.Normal){
			outLevel=1;
		}else if (outputLevel == OutputLevel.None){
			outLevel=0;
		}
        if (result instanceof CtaObject){
		    handle= ((CtaObject) result).getOpenDANativeHandle();
			this.putValue(this.myID, this.configString, this.workingDir, id, handle, outLevel, context, iteration);
		}
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

		int handle=0;
		if (parameters instanceof CtaObject){
			this.putIterationReport(this.myID, this.configString, this.workingDir, iteration, cost, handle);
		} else {
			CtaVector ctaParameters=new CtaVector(parameters);
			this.putIterationReport(this.myID, this.configString, this.workingDir, iteration, cost, handle);
			ctaParameters.free();
		}
	}

	/**
	 * Give the default maximum size of result item that can be printed to the result file by this result writer.
	 *
	 * @return int defaultMaxSize
	 */
	public int getDefaultMaxSize() {
		return Integer.MAX_VALUE;
	}

	/**
	 * Free the ResultWriter instance.
	 */
	public void free() {
		this.free(this.myID);
		
	}

	private native void putMessage(int IDWriter, String config, String workingDir, String message);
	private native void putValue(int IDWriter, String config, String workingDir, String id, int handle, int outputLevel, String context, int iteration);
	private native void putIterationReport(int IDWriter, String config, String workingDir, int iteration, double cost, int handle);
    private native void free(int IDWriter);
	
}
