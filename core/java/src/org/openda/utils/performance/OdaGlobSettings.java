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
package org.openda.utils.performance;
public class OdaGlobSettings {
	private static boolean productionRun=false;
	private static double timePrecision =(1.0/24.0/60.0/60.0); //1 Second
	private static boolean vectorPrecisionIsFloat = false;
	private static boolean vectorIsNative =false;

	static public void setProductionRun(boolean val){
		productionRun=val;
	}

	static public boolean getProductionRun(){
		return productionRun;
	}

    static public void setTimePrecision(double eps){
		timePrecision=eps;
	}
	static public double  getTimePrecision(){
		return timePrecision;
	}

	static public void setVectorPrecisionFloat(boolean val){
		vectorPrecisionIsFloat=val;
	}
	static public boolean getVectorPrecisionFloat(){
		return vectorPrecisionIsFloat;
	}

	static public void setVectorIsNative(boolean val){
		vectorIsNative=val;
	}
	static public boolean getVectorIsNative(){
		return vectorIsNative;
	}



}
