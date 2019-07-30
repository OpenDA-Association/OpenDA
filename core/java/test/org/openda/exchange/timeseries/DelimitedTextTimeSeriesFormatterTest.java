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
package org.openda.exchange.timeseries;

	import junit.framework.TestCase;
	import org.openda.utils.ConfigTree;
	import org.openda.utils.OpenDaTestSupport;

	import java.io.*;
	import java.nio.charset.Charset;
	import java.text.ParseException;
	import java.util.Locale;

public class DelimitedTextTimeSeriesFormatterTest extends TestCase {
	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(DelimitedTextTimeSeriesFormatterTest.class,"core");
		testRunDataDir = testData.getTestRunDataDir();
		Locale currentLocale = Locale.getDefault();
		System.out.println(currentLocale.getDisplayLanguage());
		System.out.println(currentLocale.getDisplayCountry());
	}

	public void testNoosFormatted_skip() {
		double delta=0.0001;

		ConfigTree config = new ConfigTree("<config><dateTimePattern>yyyyMMddHHmm</dateTimePattern><timeZone>GMT</timeZone><delimiter>\\s+</delimiter><skipLines>12</skipLines></config>");

		TimeSeriesFormatter formatter = new DelimitedTextTimeSeriesFormatter(config);
		File denHelderNoos = new File(testRunDataDir, "den_helder_waterlevel_astro.noos");
		TimeSeries series1 = formatter.readFile(denHelderNoos.getAbsolutePath());
		double times[] = series1.getTimesRef();
		assertEquals("times[0]", 54466.0,times[0], delta);

	}

	public void testNoosFormatted_comment() {
		double delta=0.0001;

		ConfigTree config = new ConfigTree("<config><dateTimePattern>yyyyMMddHHmm</dateTimePattern><timeZone>GMT</timeZone><delimiter>\\s+</delimiter><commentMarker>#</commentMarker></config>");

		TimeSeriesFormatter formatter = new DelimitedTextTimeSeriesFormatter(config);
		File denHelderNoos = new File(testRunDataDir, "den_helder_waterlevel_astro.noos");
		TimeSeries series1 = formatter.readFile(denHelderNoos.getAbsolutePath());
		double times[] = series1.getTimesRef();
		assertEquals("times[0]", 54466.0,times[0], delta);

	}



	public void testCsvFormatted() {
		ConfigTree config = new ConfigTree("<root><delimiter>,</delimiter><skipLines>1</skipLines><decimal>.</decimal></root>");

		TimeSeriesFormatter formatter = new DelimitedTextTimeSeriesFormatter(config);
		File csvFile = new File(testRunDataDir, "output.c1_locA.concentration.csv");
		TimeSeries series1 = formatter.readFile(csvFile.getAbsolutePath());
		assertNotNull(series1);
		double times[] = series1.getTimesRef();
		assertEquals("times[0]", 60.0,times[0], Math.ulp(60.0));
		assertEquals("times length", 300,times.length);
	}

	public void testCsvFormattedWithSelectors() {
		double delta=0.0001;
		ConfigTree config = new ConfigTree("<root><dateTimeSelector>2</dateTimeSelector><valueSelector>0</valueSelector><delimiter>,</delimiter><skipLines>1</skipLines><decimal>.</decimal></root>");
		TimeSeriesFormatter formatter = new DelimitedTextTimeSeriesFormatter(config);
		File csvFile = new File(testRunDataDir, "columnselector.csv");
		TimeSeries series1 = formatter.readFile(csvFile.getAbsolutePath());
		assertNotNull(series1);
		double times[] = series1.getTimesRef();
		assertEquals("times[0]", 60.0,times[0], Math.ulp(60.0));
		assertEquals("times length", 300,times.length);
	}

	public void testCsvFormattedWithExponenents() {
//		time,value
//		60,-1.4683382E+00
//		120,-6.1627460E-02
//		180,4.6469403E-01
//		240,8.5277511E-01
//		300,-7.4561748E-01
//		360,5.4569893E+01

		double delta=0.0001;
		ConfigTree config = new ConfigTree("<root><dateTimeSelector>0</dateTimeSelector><valueSelector>1</valueSelector><delimiter>,</delimiter><skipLines>1</skipLines></root>");
		TimeSeriesFormatter formatter = new DelimitedTextTimeSeriesFormatter(config);
		File csvFile = new File(testRunDataDir, "c1_locA.csv");
		TimeSeries series1 = formatter.readFile(csvFile.getAbsolutePath());
		assertNotNull(series1);
		double times[] = series1.getTimesRef();
		double values[] = series1.getValuesRef();
		assertEquals("times[0]", 60.0,times[0], Math.ulp(60.0));
		assertEquals("times length", 250,times.length);
		assertEquals("values[0]", -1.4683382E+00,values[0], Math.ulp(-1.4683382E+00));
		assertEquals("values[1]", -6.1627460E-02,values[1], Math.ulp(-6.1627460E-02));
		assertEquals("values[2]", 4.6469403E-01,values[2], Math.ulp(4.6469403E-01));
		assertEquals("values[5]", 5.4569893E+01,values[5], Math.ulp(5.4569893E+01));
	}

	public void testSemicolon() throws ParseException, IOException{
		double delta=0.0001;
		ConfigTree config = new ConfigTree("<root><delimiter>;</delimiter><decimalSeparator>,</decimalSeparator></root>");
		TimeSeriesFormatter formatter = new DelimitedTextTimeSeriesFormatter(config);
		InputStream testInputStream = new ByteArrayInputStream("60,0;300,0\n".getBytes(Charset.forName("UTF-8")));
		TimeSeries series1 = formatter.read(testInputStream);
		assertNotNull(series1);
		double times[] = series1.getTimesRef();
		double values[] = series1.getValuesRef();
		assertEquals("times[0]", 60.0,times[0], Math.ulp(60.0));
		assertEquals("values[0]", 300.0,values[0], Math.ulp(300.0));
	}



}
