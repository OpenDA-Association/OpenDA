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


package org.openda.utils.io;

import junit.framework.TestCase;
import org.openda.interfaces.ITreeVector;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.TreeVector;

import java.io.File;
import java.io.IOException;

/**
 * Test for writing TreeVector files
 */
public class TreeVectorWriterTest extends TestCase {
	
    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(TreeVectorWriterTest.class,"core");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testReadTreeVector_1() {
        File treeVectorFile = new File(testRunDataDir, "example_1_very_simple.xml");
        File treeVectorOutFile = new File(testRunDataDir, "example_1_very_simple-out.xml");
        ITreeVector treeVector = readTreeVector(treeVectorFile);
        writeTreeVector(treeVectorOutFile, treeVector);
    }

    public void testReadTreeVector_2() {
        File treeVectorFile = new File(testRunDataDir, "example_2_fields_without_metadata.xml");
        File treeVectorOutFile = new File(testRunDataDir, "example_2_fields_without_metadata-out.xml");
        ITreeVector treeVector = readTreeVector(treeVectorFile);
        writeTreeVector(treeVectorOutFile, treeVector);
    }

    public void testReadTreeVector_3() {
        File treeVectorFile = new File(testRunDataDir, "example_3_fields_with_metadata.xml");
        File treeVectorOutFile = new File(testRunDataDir, "example_3_fields_with_metadata-out.xml");
        ITreeVector treeVector = readTreeVector(treeVectorFile);
        writeTreeVector(treeVectorOutFile, treeVector);
    }

    public void testReadTreeVector_4() {
        File treeVectorFile = new File(testRunDataDir, "example_4_timeseries.xml");
        File treeVectorOutFile = new File(testRunDataDir, "example_4_timeseries-out.xml");
        ITreeVector treeVector = readTreeVector(treeVectorFile);
        writeTreeVector(treeVectorOutFile, treeVector);
    }

    private ITreeVector readTreeVector(File treeVectorFile) {
        TreeVectorReader treeVectorReader = new TreeVectorReader(treeVectorFile);
        ITreeVector treeVector = treeVectorReader.readTreeVector();
        assertTrue(treeVector instanceof TreeVector);
        return treeVector;
    }

    private void writeTreeVector(File treeVectorFile, ITreeVector treeVector) {
        TreeVectorWriter treeVectorWriter = new TreeVectorWriter(treeVectorFile);
        treeVectorWriter.writeTreeVector(treeVector);
    }
}
