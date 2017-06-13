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


package org.openda.models.biasAwareObservations;

import junit.framework.TestCase;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Test Model for modelling bias in the observed values
 *
 * @author Nils van Velzen (TU-Delft/VORtech)
 *
 */
public class BiasAwareModelTest extends TestCase{

	    private File testRunDataDir;
	    private OpenDaTestSupport testData;

	    protected void setUp() throws IOException {
	    	testData = new OpenDaTestSupport(BiasAwareModelTest.class,"models");
	        testRunDataDir = testData.getTestRunDataDir();
	    }

	    public void testBiasAwareModel_1() {

	        System.out.println("=========================================================");
	        System.out.println("Test using ID's");
	        System.out.println("=========================================================");
	        // Constructors
	        IStochModelFactory fact1 = new BiasAwareObservationsModelFactory();
	        fact1.initialize(testRunDataDir, new String[]{"BiasAwareStochModel1.xml"});
	        IStochModelInstance mod1 = fact1.getInstance(IStochModelFactory.OutputLevel.Suppress);

			assertEquals(8,mod1.getState().getSize());
	    }

		public void testBiasAwareModel_2() {

     		System.out.println("=========================================================");
     		System.out.println("Test using fixed size");
     		System.out.println("=========================================================");
     		// Constructors
     		IStochModelFactory fact1 = new BiasAwareObservationsModelFactory();
     		fact1.initialize(testRunDataDir, new String[]{"BiasAwareStochModel2.xml"});
     		IStochModelInstance mod1 = fact1.getInstance(IStochModelFactory.OutputLevel.Suppress);

			assertEquals(mod1.getState().getSize(),13);
 }


}
