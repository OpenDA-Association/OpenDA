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
package org.openda.model_swan;

import junit.framework.TestCase;

import org.openda.exchange.dataobjects.NoosDataObject;
import org.openda.exchange.iotools.DataCopier;
import org.openda.exchange.iotools.DataDumper;
import org.openda.interfaces.IDataObject;
import org.openda.model_swan.SwanResultsTimeDependent;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Tests for Swan Results IoObject
 */
public class SwanFormatConversionTest extends TestCase {

    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(SwanFormatConversionTest.class,"model_swan");
    }

    public void testObservations_1() throws Exception {
		System.out.println("==============================================================================");
		System.out.println(" Basic test for dumping SWAN time-dependent series.");
		System.out.println("==============================================================================");

        File inputDir = new File(testData.getTestRunDataDir(), "FormatConversion");
        String fileName="northsea_1d_nonstationary_points_true.tab";
		IDataObject SwanDataObject = new SwanResultsTimeDependent();
        SwanDataObject.initialize(inputDir, new String[]{fileName});
		DataDumper dumper = new DataDumper(SwanDataObject);
		dumper.setOutputDir(inputDir);
		dumper.dump();
		// nothing can be tested as the output goes to System.out
		// the DataCopier can be tested more thoroughly below. 
    }
    
    public void testDataCopier_1() throws Exception {
		System.out.println("==============================================================================");
		System.out.println(" Conversion test for dumping SWAN time-dependent series.");
		System.out.println("==============================================================================");

        File inputDir = new File(testData.getTestRunDataDir(), "FormatConversion");
        String fileName="northsea_1d_nonstationary_points_true.tab";
		IDataObject swanDataObject = new SwanResultsTimeDependent();
        swanDataObject.initialize(inputDir, new String[]{fileName});
		
        File outputDir = new File(inputDir, "noos_output");
        if(!outputDir.exists()){
        	outputDir.mkdir();
        }else{
        	throw new RuntimeException("Output directory already exists. This should not be possible");
        }
        IDataObject noosDataObject = new NoosDataObject();
        noosDataObject.initialize(outputDir, new String[]{""});

        DataCopier copier = new DataCopier(swanDataObject, noosDataObject);
        copier.copyAll();
        copier.finish();
		File outputFile = new File(outputDir,"Hsig_0._0..noos");
		File referenceFile = new File(inputDir,"Hsig.noos.ref");
		boolean test=testData.FilesAreIdentical(outputFile, referenceFile);
		assertTrue(test);
    }
    

//    public void testObservations_1() throws Exception {
//        File test_1_dir = new File(testData.getTestRunDataDir(), "SwanFormatConversion");
//        IDataObject ioObject = BBUtils.createDataObject(test_1_dir,
//                SwanResultsTimeDependent.class.getName(), "out_P1.TAB", new String[]{});
//        checkValues(ioObject);
//        ioObject = BBUtils.createDataObject(test_1_dir,
//                SwanResultsTimeDependent.class.getName(), "out_P2.TAB", new String[]{});
//        checkValues2(ioObject);
//        ioObject = BBUtils.createDataObject(test_1_dir,
//                SwanResultsTimeDependent.class.getName(), "out_P3.TAB", new String[]{});
//        checkValues3(ioObject);
//    }
 
}
