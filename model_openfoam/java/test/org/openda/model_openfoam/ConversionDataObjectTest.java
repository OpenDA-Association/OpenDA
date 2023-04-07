/*
* Copyright (c) 2023 OpenDA Association 
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
package org.openda.model_openfoam;
import junit.framework.TestCase;
import org.openda.exchange.iotools.DataCopier;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;

/**
 * Created by werner on 25/03/16.
 */
public class ConversionDataObjectTest extends TestCase {

    private File testRunDataDir;

    protected void setUp() throws IOException {
		OpenDaTestSupport testData = new OpenDaTestSupport(ConversionDataObjectTest.class,"model_openfoam");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testCopyProbeToTextMain() {
        File inputFile = new File(testRunDataDir, "probes/0/p");
        File outputFile = new File(testRunDataDir, "conversion.txt");
        DataCopier.main(new String[]{"-c", "org.openda.model_openfoam.ProbeDataObject", "-a", "2015-12-01T00:00:00Z", inputFile.getAbsolutePath(),
            "-c", "org.openda.exchange.dataobjects.TestDataObject", outputFile.getAbsolutePath()});

    }

//    public void testCopyCsvTimeSeriesToProbe() {
//        File inputFile = new File(testRunDataDir, "CsvTimeSeries/SENSORID_QUANTITY.csv");
//        File outputFile = new File(testRunDataDir, "convertedToProbeFormat");
//        DataCopier.main(new String[]{"-c", "org.openda.model_openfoam.CsvTimeSeriesDataObject",  inputFile.getAbsolutePath(),
//            "-c", "org.openda.model_openfoam.ProbeDataObject", "-a", "2015-12-01T00:00:00Z", outputFile.getAbsolutePath()});
//
//    }



}
