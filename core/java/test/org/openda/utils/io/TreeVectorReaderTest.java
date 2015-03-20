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
 * Test for reading TreeVector files
 */
public class TreeVectorReaderTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(TreeVectorReaderTest.class,"core");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testReadTreeVector_1() throws IOException {
        File treeVectorFile = new File(testRunDataDir, "example_1_very_simple.xml");
        readTreeVector(treeVectorFile);
    }

    public void testReadTreeVector_2() {
        File treeVectorFile = new File(testRunDataDir, "example_2_fields_without_metadata.xml");
        readTreeVector(treeVectorFile);
    }

    public void testReadTreeVector_3() {
        File treeVectorFile = new File(testRunDataDir, "example_3_fields_with_metadata.xml");
        readTreeVector(treeVectorFile);
    }

    public void testReadTreeVector_4() {
        // TODO: test fails on reg. exp.
        File treeVectorFile = new File(testRunDataDir, "example_4_timeseries.xml");
        readTreeVector(treeVectorFile);
    }

    private void readTreeVector(File treeVectorFile) {
        TreeVectorReader treeVectorReader = new TreeVectorReader(treeVectorFile);
        ITreeVector treeVector = treeVectorReader.readTreeVector();
        assertTrue(treeVector instanceof TreeVector);
        TreeVector simpleTreeVector = (TreeVector)treeVector;
        System.out.println(simpleTreeVector.toString());
    }
}
