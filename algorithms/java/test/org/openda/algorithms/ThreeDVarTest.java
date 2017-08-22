/* OpenDA v2.4.1 
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
package org.openda.algorithms;
import junit.framework.TestCase;

import org.openda.algorithms.kalmanFilter.*;
import org.openda.interfaces.*;
import org.openda.models.lorenz.LorenzStochModelFactory;
import org.openda.models.lorenz96.Lorenz96StochModelFactory;
import org.openda.utils.CsvStochObserver;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.StochVector;
import org.openda.utils.Vector;

import java.io.*;

public class ThreeDVarTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;
	private long initSeed=1234567890;
	
	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(ThreeDVarTest.class,"algorithms");
		testRunDataDir = testData.getTestRunDataDir();
	}
	
	public void test3DVar() {
		System.out.println("========================================================");
		System.out.println(" Test 3D-Var");
		System.out.println(" Lorenz with linear observations x , y , z.");
		System.out.println("========================================================");
		StochVector.setSeed(initSeed);
		//generate observations
		IStochModelFactory factory = new LorenzStochModelFactory();
		factory.initialize(null, new String[]{"<lorenzConfig><simulationTimespan>[0.0,0.05,0.05]</simulationTimespan><systemNoise>{[0.0,0.0,0.0],[0.01,0.01,0.01]}</systemNoise><initialState>[1.508870, -1.531271, 25.46091]</initialState><initialStateUncertainty>[0.5,0.5,0.5]</initialStateUncertainty></lorenzConfig>"});
		String content 	= "time,index,value,std,transform\n"
						+ "0.05,0.0, 1.368,0.5,1.0\n"
						+ "0.05,1.0,-0.293,0.5,1.0\n"
						+ "0.05,2.0,23.225,0.5,1.0";
		IStochObserver obsGenerated = new CsvStochObserver(content);

		// create and run
		IAlgorithm var3d = new ThreeDVar();
		var3d.initialize(testRunDataDir,new String[]{""});
		var3d.setStochComponents(obsGenerated,factory);		
		var3d.prepare();
		var3d.run();
		
		// final time
		// The residuals are initially about [1 1 1] with equal weights for obs and model,
		// thus the optimal solution is halfway.
		IVector x = ((ThreeDVar) var3d).getCurrentState();
		System.out.println("                   x = "+x);
		IVector xop = new Vector("[0.86,-0.79,22.72]");
		System.out.println("Should be close to x = "+xop);
		assertEquals("First variable:  ",x.getValue(0),xop.getValue(0),0.01);
		assertEquals("Second variable: ",x.getValue(1),xop.getValue(1),0.01);
		assertEquals("Third variable:  ",x.getValue(2),xop.getValue(2),0.01);
		
		var3d.finish();
	}
	
	public void test3DVar_spatial() {
		System.out.println("========================================================");
		System.out.println(" Test 3D-Var");
		System.out.println(" Lorenz96 with linear observations x , y , z.");
		System.out.println("========================================================");
		StochVector.setSeed(initSeed);
		//generate observations
		IStochModelFactory factory = new Lorenz96StochModelFactory();
		factory.initialize(null, new String[]{"<lorenz96Config><simulationTimespan>[0.0,0.05,0.05]</simulationTimespan></lorenz96Config>"});
		String content 	= "time,index,value,std,transform\n"
						+ "0.05,0.0, 2.572,0.5,1.0\n"
						+ "0.05,10.0,3.750,0.5,1.0\n"
						+ "0.05,30.0,5.288,0.5,1.0";
		IStochObserver obsGenerated = new CsvStochObserver(content);

		// create and run
		IAlgorithm var3d = new ThreeDVar();
		var3d.initialize(testRunDataDir,new String[]{""});
		var3d.setStochComponents(obsGenerated,factory);		
		var3d.prepare();
		var3d.run();
		
		// final time
		// The residuals are initialy about [1 1 1] with equal weightsfor obs and model,
		// thus the optimal solution is halfway.
		IVector x = ((ThreeDVar) var3d).getCurrentState();
		((Vector)x).maxFullExpandLength=50;
		System.out.println("                   x = "+x);
		IVector xop = new Vector("[2.580071130860116,9.231851158819309,4.518063687480813,-1.4787333723694438,-2.702404444686147,-0.583832418710757,-1.9745544925260057,8.130735502208678,2.0697664807480947,-7.281108215194047,3.7377327106248446,0.41323941285483423,1.0140655172439188,7.452237509867457,9.255650640065578,3.8200723893049613,3.816197885499214,3.962579879948762,6.080714580817431,3.1248557399595454,-2.3558166976968926,1.4071750156795912,9.522279463345484,0.17787399337305276,-3.272560177070968,0.16855363237521837,13.155315899274338,2.874854965851796,-0.700831145385807,3.236703922558145,5.28806644547596,2.237983335060493,-5.0400291136025155,-0.9580720117574275,-2.1400896943017425,2.8754923358800806,7.3838677871211615,2.4099566617840353,-0.5268356902843909,-1.2823796752909966]");
		((Vector)x).maxFullExpandLength=50;
		System.out.println("Should be close to x = "+xop);
		assertEquals("First variable:  ",x.getValue(0),xop.getValue(0),0.01);
		assertEquals("Second variable: ",x.getValue(1),xop.getValue(1),0.01);
		assertEquals("Third variable:  ",x.getValue(2),xop.getValue(2),0.01);
		
		var3d.finish();
	}

}
