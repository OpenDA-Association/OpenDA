/* MOD_V2.0
* Copyright (c) 2010 OpenDA Association
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
package org.openda.model_delft3d;

import junit.framework.TestCase;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Created by IntelliJ IDEA.
 * User: nils
 * Date: May 15, 2009
 * Time: 11:59:44 AM
 * To change this template use File | Settings | File Templates.
 */
public class D3dRoughParamsTest  extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(D3dRoughParamsTest.class,"model_delft3d");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testExistingfile(){

        // First write a test file
    	File dataDir=testRunDataDir;
        D3dRoughParamsFile roughFile=new D3dRoughParamsFile();
        roughFile.initialize(testRunDataDir, "ruw.karak",new String[]{});

        IPrevExchangeItem[] items = roughFile.getExchangeItems();

        int n=items.length;
        double delta=0.00001;
        assertEquals(10, n);
        for(int i=0;i<n;i++){
        	String id=items[i].getId();
        	double values[] =items[i].getValuesAsDoubles();
        	assertEquals(1, values.length);
        	System.out.println("Id="+id+" value="+values[0]);

        	String code=id.substring(id.length()-1);
        	if(code.matches("A")){
        		assertEquals(0.1, values[0], delta);
        	}else{
        		assertEquals(2.5, values[0], delta);
        	}
        }

        // change some parameter
        items[0].setValuesAsDoubles(new double[]{100.});
        items[1].setValuesAsDoubles(new double[]{4.5});
        // flush to file
        roughFile.finish();

        // check output
        File outputFile=new File(dataDir,"ruw.karak");
        File referenceFile=new File(dataDir,"ruw.karak.ref");
        assertTrue(outputFile.exists());
        assertTrue(referenceFile.exists());

        assertTrue(testData.FilesAreIdentical(outputFile, referenceFile));

    }

    public void testExistingfile_2(){

        // First write a test file
    	File dataDir=testRunDataDir;
        D3dRoughParamsFile roughFile=new D3dRoughParamsFile();
        roughFile.initialize(testRunDataDir, "ruwQ7020.karak",new String[]{});

        IPrevExchangeItem[] items = roughFile.getExchangeItems();

        int n=items.length;
        double delta=0.00001;
        assertEquals(137, n);
        for(int i=0;i<n;i++){
        	String id=items[i].getId();
        	double values[] =items[i].getValuesAsDoubles();
        	assertEquals(1, values.length);
        	System.out.println("Id="+id+" value="+values[0]);
        }

        // change some parameter
        items[13].setValuesAsDoubles(new double[]{100.});
        // flush to file
        roughFile.finish();

        // check output
        File outputFile=new File(dataDir,"ruwQ7020.karak");
        File referenceFile=new File(dataDir,"ruwQ7020.karak.ref");
        assertTrue(outputFile.exists());
        assertTrue(referenceFile.exists());

        testData.FilesAreIdentical(outputFile, referenceFile);

    }

    public void test1(){

        // First write a test file
        String[] sContent=testFile();
        File roughFile=new File(testRunDataDir, "ruw.karak1");
        D3dRoughParamsUtils.writeWholeFile(roughFile, sContent);

        // Now Create some Exchange Items
        D3dRoughParamsFileExchangeItem ex1= new D3dRoughParamsFileExchangeItem("Param1", roughFile,  2, "A");
        D3dRoughParamsFileExchangeItem ex2= new D3dRoughParamsFileExchangeItem("Param2", roughFile,  2, "B");
        D3dRoughParamsFileExchangeItem ex3= new D3dRoughParamsFileExchangeItem("Param3", roughFile,  3, "A");
        D3dRoughParamsFileExchangeItem ex4= new D3dRoughParamsFileExchangeItem("Param4", roughFile,  4, "A");
        D3dRoughParamsFileExchangeItem ex5= new D3dRoughParamsFileExchangeItem("Param5", roughFile,  4, "B");

        // Get the values
        Double val1= (Double) ex1.getValues();
        Double val2= (Double) ex2.getValues();
        Double val3= (Double) ex3.getValues();
        Double val4= (Double) ex4.getValues();
        Double val5= (Double) ex5.getValues();

        assertEquals("ex1.getValues()", "0.1", Double.toString(val1));
        assertEquals("ex2.getValues()", "2.5",  Double.toString(val2));
        assertEquals("ex3.getValues()", "0.1",  Double.toString(val3));
        assertEquals("ex4.getValues()", "0.1",  Double.toString(val4));
        assertEquals("ex5.getValues()", "2.5",  Double.toString(val5));

        //Set the values
        ex1.setValues(1.0);
        ex2.setValues(2.0);
        ex3.setValues(3.0);
        ex4.setValues(4.0);
        ex5.setValues(5.0);

        // Get the values
        val1= (Double) ex1.getValues();
        val2= (Double) ex2.getValues();
        val3= (Double) ex3.getValues();
        val4= (Double) ex4.getValues();
        val5= (Double) ex5.getValues();

        assertEquals("ex1.getValues()", "1.0",  Double.toString(val1));
        assertEquals("ex2.getValues()", "2.0",  Double.toString(val2));
        assertEquals("ex3.getValues()", "3.0",  Double.toString(val3));
        assertEquals("ex4.getValues()", "4.0",  Double.toString(val4));
        assertEquals("ex4.getValues()", "4.0",  Double.toString(val4));
        assertEquals("ex5.getValues()", "5.0",  Double.toString(val5));


        //Set the values
        ex1.setValues(5.0);
        ex2.setValues(6.0);
        ex3.setValues(7.0);
        ex4.setValues(8.0);
        ex5.setValues(9.0);

        // Get the values
        val1= (Double) ex1.getValues();
        val2= (Double) ex2.getValues();
        val3= (Double) ex3.getValues();
        val4= (Double) ex4.getValues();
        val5= (Double) ex5.getValues();

        assertEquals("ex1.getValues()", "5.0",  Double.toString(val1));
        assertEquals("ex2.getValues()", "6.0",  Double.toString(val2));
        assertEquals("ex3.getValues()", "7.0",  Double.toString(val3));
        assertEquals("ex4.getValues()", "8.0",  Double.toString(val4));
        assertEquals("ex5.getValues()", "9.0",  Double.toString(val5));

    }


    public void test2(){

        // First write a test file
        String[] sContent=testFile();
        String fName="ruw.karak2";
        File roughFile=new File(testRunDataDir, fName);

        D3dRoughParamsUtils.writeWholeFile(roughFile, sContent);

        D3dRoughParamsFile paramsFile=new D3dRoughParamsFile();
        String[] noArguments = new String[]{};
        paramsFile.initialize(testRunDataDir, fName, noArguments);

        //Get all exchangeItems items
        IPrevExchangeItem[] exchangeItems =paramsFile.getExchangeItems();

        //Loop over all exchangeItems items and request the ID, name and value
        for (IPrevExchangeItem exchangeItem : exchangeItems) {
            String id = exchangeItem.getId();
            Double value = (Double) exchangeItem.getValues();
            String out = "Parameter id=" + id + " value=" + Double.toString(value);
            System.out.println(out);
        }
    }





    private String[] testFile(){
        String[] sContent = new String[6];

        // Create some content for the file
        sContent[ 0]="# This is an example roughness include file for Delft3D";
        sContent[ 1]="411   101  0.1000   2.5";
        sContent[ 2]="412   101  0.1000   2.5";
        sContent[ 3]="413   101  0.1000   2.5";
        sContent[ 4]="413   101  0.1000   2.5";
        sContent[ 5]="415   101  0.1000   2.5";


        return sContent;
    }




}
