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
package org.openda.uncertaintyModels;

import junit.framework.TestCase;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.IVector;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

public class ParameterDrawerTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
       	testData = new OpenDaTestSupport(ParameterDrawerTest.class,"core");
        testRunDataDir = testData.getTestRunDataDir();
   }

    public void testGLUEParameterDrawer() {
        System.out.println("========================================================");
        System.out.println(" Test ParameterDrawer.");
        System.out.println("========================================================");

		ParameterDrawerFactory parameterDrawerFactory = new ParameterDrawerFactory();
		parameterDrawerFactory.initialize(testRunDataDir, new String[]{"paramsHBV.csv"});
		IStochModelInstance parameterDrawer = parameterDrawerFactory.getInstance(
				IStochModelFactory.OutputLevel.ModelDefault);
        IVector selectedParameters;
        int nTrial = 10;
        for (int i=0; i<nTrial; i++){
            selectedParameters = parameterDrawer.getParameterUncertainty().createRealization();
            System.out.println("selectedParameters: "+selectedParameters.toString());
            assertEquals("Check TreeVectorId: ", selectedParameters.toString().contains("parameterSet"),true);
        }
    }
}
