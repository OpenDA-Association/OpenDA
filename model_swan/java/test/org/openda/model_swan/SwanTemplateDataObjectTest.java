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

package org.openda.model_swan;

import junit.framework.Assert;
import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Time;

import java.io.File;
import java.io.IOException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

/**
 * Test class for testing class SwanTemplateDataObject
 */
public class SwanTemplateDataObjectTest extends TestCase {
    private OpenDaTestSupport testData;
    private File testRunDataDir;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(SwanTemplateDataObjectTest.class, "model_swan");
        testRunDataDir = testData.getTestRunDataDir();
        Locale.setDefault(Locale.UK);
    }

	public void testTemplateInputFile() {
		SwanTemplateDataObject dataObject = new SwanTemplateDataObject();
		String[] arguments = new String[]{"TSTART","TSTOP","-r","swan-restart"};
		String swanSWNOutputFileName = "input.swn";
		String swanSWNOutputFilePath = "SwanInput/input/" + swanSWNOutputFileName;
		String swanConfigFile = "templateConfig.xml";
		File workDir = new File(testRunDataDir, "SwanInput/input");
		dataObject.initialize(workDir, swanConfigFile, arguments);
		double startDate = formatDate("2011-06-04 07:00:00");
		double endDate = formatDate("2011-06-04 21:00:00");
		dataObject.setPeriod(startDate, endDate);
		dataObject.finish();

		//Get all exchangeItems items
		String[] exchangeItemsIDs = dataObject.getExchangeItemIDs();
		//Loop over all exchangeItems items and request the ID, name and value
		for (String id: exchangeItemsIDs) {
			IExchangeItem exchangeItem = dataObject.getDataObjectExchangeItem(id);
			assertTrue("Exchange item with id " + id + " is null",exchangeItem!=null);
			String out =  "id= " + id + " ; value= " + exchangeItem.getValues();
			System.out.println(out);
		}
		File outputRefFile=new File(testRunDataDir,"SwanInput/expected/" + swanSWNOutputFileName+ "_expected");
		File outputFile = new File(testRunDataDir,swanSWNOutputFilePath);
		Assert.assertTrue("Actual output file '" + outputFile + "' does not equal expected output file '"
				+ outputRefFile + "'.", testData.FilesAreIdentical(outputFile, outputRefFile));

	}

	public void testTemplateStationaryInputFile() {
		SwanTemplateDataObject dataObject = new SwanTemplateDataObject();
		String[] arguments = new String[]{"TSTART","TSTOP","60","-r","swan-restart"};
		String swanSWNOutputFileName = "inputStationary.swn";
		String swanSWNOutputFilePath = "SwanInput/input/" + swanSWNOutputFileName;
		String swanConfigFile = "templateStationaryConfig.xml";
		File workDir = new File(testRunDataDir, "SwanInput/input");
		dataObject.initialize(workDir, swanConfigFile, arguments);
		double startDate = formatDate("2011-06-04 07:00:00");
		double endDate = formatDate("2011-06-04 21:00:00");
		dataObject.setPeriod(startDate, endDate);
		dataObject.finish();

		//Get all exchangeItems items
		String[] exchangeItemsIDs = dataObject.getExchangeItemIDs();
		//Loop over all exchangeItems items and request the ID, name and value
		for (String id: exchangeItemsIDs) {
			IExchangeItem exchangeItem = dataObject.getDataObjectExchangeItem(id);
			assertTrue("Exchange item with id " + id + " is null",exchangeItem!=null);
			String out =  "id= " + id + " ; value= " + exchangeItem.getValues();
			System.out.println(out);
		}
		File outputRefFile=new File(testRunDataDir,"SwanInput/expected/" + swanSWNOutputFileName+ "_expected");
		File outputFile = new File(testRunDataDir,swanSWNOutputFilePath);
		Assert.assertTrue("Actual output file '" + outputFile + "' does not equal expected output file '"
				+ outputRefFile + "'.", testData.FilesAreIdentical(outputFile, outputRefFile));

	}


    private double formatDate(String dateString) {
        DateFormat formatter = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        formatter.setTimeZone(TimeZone.getTimeZone("UTC"));
        Date startDate;

        try {
            startDate = formatter.parse(dateString);
        } catch (ParseException e) {
            throw new RuntimeException(this.getClass().getName() +
                    ": could not parse time from argument \"" + dateString + "\"");
        }
        return new Time(startDate).getMJD();
    }
}
