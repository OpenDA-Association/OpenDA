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

package org.openda.interfaces;

/**
 * Interface that facilitates algorithms to generate their output. An algorithm can put vector results,
 * textual results, etcetera. The implementation of the ResultWriter will write these results to
 * a text file, a Matlab file, a NetCDF file etcetera.
 */
public interface IResultWriter {

	/**
     * Enumeration for sources of result messages.
     */
    public static enum Source {
        Model,
        Algorithm,
        Observer
    }

    /**
     * Enumeration for result types that can be put by an Algorithm.
     */
    public static enum MessageType {
        Step,
        InnerIteration,
        OuterIteration,
        Instance
    }

	/**
	 * Enumeration for output level used for results selection.
	 */
	public static enum OutputLevel {
		None,
		Essential,
		Normal,
		Verbose,
		All
	}

    /**
     * Default configuration of results selection.
     */
    public static int defaultMinSize = 1;
    public static int defaultMaxSize = Integer.MAX_VALUE;
    public static String defaultId = "any";
    public static String defaultContext = "any";
    public static OutputLevel defaultOutputLevel = OutputLevel.Normal;

    /**
     * Put a message.
     *
     * @param source  the producer of the message (algorithm, model or observer)
     * @param message message to be written to output
     */
    public void putMessage(Source source, String message);

    /**
     * Put a message.
     *
     * @param source  the producer of the message (algorithm, model or observer)
     * @param message message to be written to output
     */
    public void putMessage(IInstance source, String message);

//    /**
//     * Put a vector.
//     *
//     * @param source the producer of the message (algorithm, model or observer)
//     * @param id     the output vector's identifier
//     * @param result the output item
//     */
//    public void putValue(Source source, String id, Object result);
//
//
//    /**
//     * Put a vector.
//     *
//     * @param source the producer of the message (algorithm, model or observer)
//     * @param id     the output vector's identifier
//     * @param result the output item
//     */
//    public void putValue(IInstance source, String id, Object result);
//
//    /**
//     * Put a vector, indicating the algorithm's current iteration.
//     *
//     * @param source    the producer of the message (algorithm, model or observer)
//     * @param id        the output vector's identifier
//     * @param result    the output item
//     * @param iteration current iteration
//     */
//    public void putValue(Source source, String id, Object result, int iteration);

//    /**
//     * Put a vector, indicating the algorithm's current iteration.
//     *
//     * @param source    the producer of the message (algorithm, model or observer)
//     * @param id        the output vector's identifier
//     * @param result    the output item
//     * @param iteration current iteration
//     */
//    public void putValue(IInstance source, String id, Object result, int iteration);

	/**
	 * Put a vector, indicating the algorithm's current iteration.
	 *
	 * @param source    the producer of the message (algorithm, model or observer)
	 * @param id        the output vector's identifier
	 * @param result    the output item
	 * @param outputLevel the level of output group
	 * @param context	context where the value is written
	 * @param iteration current iteration
	 */
	public void putValue(Source source, String id, Object result, OutputLevel outputLevel, String context, int iteration);

    /**
     * Put a report on the iteration.
     *
     * @param source    the producer of the message (algorithm, model or observer)
     * @param iteration iteration index
     * @param cost      cost function
     * @param parameters  parameters used in the computation
     */
    public void putIterationReport(IInstance source, int iteration, double cost, IVector parameters);

    /**
     * Give the default maximum size of result item that can be printed to the result file by this result writer.
     * @return int defaultMaxSize
     */
    public int getDefaultMaxSize();

    /**
     * Free the ResultWriter instance.
     */
    public void free();
}
