package org.openda.utils.performance;

/**
 * Created by IntelliJ IDEA.
 * User: nils
 * Date: 4/27/12
 * Time: 3:29 PM
 * To change this template use File | Settings | File Templates.
 */
public class OdaGlobSettings {
	private static boolean productionRun=false;
	private static double timePrecision =(1.0/24.0/60.0/60.0); //1 Second
	private static boolean vectorPrecisionIsFloat = false;

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
}
