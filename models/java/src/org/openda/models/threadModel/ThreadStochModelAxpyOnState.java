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
package org.openda.models.threadModel;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.IVector;

/**
 * Created with IntelliJ IDEA.
 * User: nils
 * Date: 11/2/12
 * Time: 4:48 PM
 * To change this template use File | Settings | File Templates.
 */
public class ThreadStochModelAxpyOnState extends Thread {

	IStochModelInstance threadModel=null;
	IVector x=null;
	double alpha;

	    public ThreadStochModelAxpyOnState(IStochModelInstance threadModel, double alpha, IVector x){
			this.threadModel=threadModel;
			this.alpha=alpha;
			this.x=x.clone();
	    }

	    public void run() {
			this.threadModel.axpyOnState(this.alpha, this.x);
            x.free();
			x=null;
	    }

}
