/* MOD_V1.0
* Copyright (c) 2010 OpenDA Association
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
package org.openda.models.threadModel;

import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.ITime;

/**
 * Thread that implements the Compute method of a stochastic Model
 *
 * @author Nils van Velzen (VORtech)
 *
 */
public class ThreadStochModelCompute extends Thread{

    /* We need the model instance and the end time of compute */
    IStochModelInstance threadModel=null;
    ITime targetTime=null;

    public ThreadStochModelCompute(IStochModelInstance threadModel, ITime targetTime){
        this.threadModel=threadModel;
        this.targetTime=targetTime;
    }

    public void run() {
		threadModel.compute(targetTime);
    }
}
