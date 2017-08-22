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
package org.openda.model_dflowfm;
import java.io.File;
import java.io.IOException;

import junit.framework.TestCase;

import org.openda.utils.OpenDaTestSupport;

public class DFlowFMCmpInputFileTest extends TestCase{

	private File testRunDataDir;
	OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(DFlowFMCmpInputFile.class,"model_dflowfm_blackbox");
		testRunDataDir = new File(testData.getTestRunDataDir(),"Timeseries");
	}

	public void testDflowfmAstrocompontentFile() {
		DFlowFMCmpInputFile cmpFile = new DFlowFMCmpInputFile(testRunDataDir , "astro.cmp");
		String[] AC = cmpFile.getACname();
		for (String var : AC) {
			if (var.contentEquals("M2")) {
				assertEquals(cmpFile.getAmplitude(var),0.6);
				assertEquals(cmpFile.getPhase(var),0.0);
			}
			if (var.contentEquals("S2")) {
				assertEquals(cmpFile.getAmplitude(var),0.1);
				assertEquals(cmpFile.getPhase(var),0.0);
			}
		}
		
		cmpFile.setAmplitude("M2", 0.7);
		cmpFile.setPhase("M2", 0.1);
		cmpFile.setAmplitude("S2", 0.3);
		cmpFile.setPhase("S2", 0.2);
		
		cmpFile.WriteInputFile();
		
		assertTrue(testData.FilesAreIdentical(new File(testRunDataDir, "astro.cmp"),new File(testRunDataDir,"astro.cmp.check")));
	}

	public void testDflowfmParsePeriodFile() {
		DFlowFMCmpInputFile cmpFile = new DFlowFMCmpInputFile(testRunDataDir , "period.cmp" );
		String[] AC = cmpFile.getACname();
		for (String var : AC) {
			if (var.contentEquals("Period")) {
				assertEquals(cmpFile.getPeriod(),0.0);
				assertEquals(cmpFile.getAmplitude(var),1.0);
				assertEquals(cmpFile.getPhase(var),0.0);
			}
		}
	}

}
