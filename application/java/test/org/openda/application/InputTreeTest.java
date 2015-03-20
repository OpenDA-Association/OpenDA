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
package org.openda.application;
import junit.framework.TestCase;

import org.openda.application.gui.InputTree;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

public class InputTreeTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(InputTreeTest.class,"application");
        testRunDataDir = testData.getTestRunDataDir();
    }

	public void testTreeAnalysis() {
		System.out.println("==============================================================================");
		System.out.println("Build tree from input");
		System.out.println("==============================================================================");
		InputTree appConf = new InputTree(null,testRunDataDir.getAbsolutePath(),"oscillatorDudOpenDaConfig.xml","openDaApplication","org.openda.application.OpenDaApplication");
		//InputTree(String parentDir, String workingDir, String fileName, String label, String className)
		String appConfTree = appConf.toLongString();
		System.out.println(appConfTree);
		System.out.println("label = "+appConf);

		String fileName   = appConf.getFileName();
		System.out.println("fileName ="+fileName);
		System.out.println("Should be fileName = oscillatorDudOpenDaConfig.xml");
		assertEquals("fileName",fileName,"oscillatorDudOpenDaConfig.xml");

		InputTree[] children = appConf.getChildren();
		int n = children.length;
		System.out.println("children.length ="+n);
		System.out.println("Should be children.length = 4");
		assertEquals("children.length",n,4);
	}

	public void testTreeAnalysis2() {
		System.out.println("==============================================================================");
		System.out.println("updating and saving a tree");
		System.out.println("==============================================================================");
        InputTree dummy = new InputTree(null,".","","","");
        String buffer = dummy.readFile(testRunDataDir.getAbsolutePath(),"oscillatorDudOpenDaConfig.xml");
        dummy.writeFile(testRunDataDir.getAbsolutePath(),"oscillatorDudOpenDaConfig.xml",buffer);
        // also for algorithm
        File inDir = new File(testRunDataDir,"algorithm");
        File outDir = new File(testRunDataDir,"algorithm");
        outDir.mkdir();
        buffer = dummy.readFile(inDir.getAbsolutePath(),"dudAlgorithm.xml");
        dummy.writeFile(outDir.getAbsolutePath(),"dudAlgorithm.xml",buffer);
		// load the copy
        InputTree appConf = new InputTree(null,
        		testRunDataDir.getAbsolutePath(),
				"oscillatorDudOpenDaConfig.xml","openDaApplication","org.openda.application.OpenDaApplication");

		String appConfTree = appConf.toLongString();
		System.out.println(appConfTree);
		System.out.println("label = "+appConf);

		//modify the algorithm
		InputTree[] children = appConf.getChildren();
		buffer = children[2].getCurrentBuffer();
		int startKey = buffer.indexOf("<outerLoop maxIterations=");
		int start    = buffer.indexOf("\"", startKey);
		String newBuffer = buffer.substring(0,start+1) + "30" + buffer.substring(start+3,buffer.length()-1);
		System.out.println(newBuffer);
		children[2].setCurrentBuffer(newBuffer);
		// save the tree
		appConf.saveTree(true);

		//now reload the file and check the change
		buffer = dummy.readFile(outDir.getAbsolutePath(),"dudAlgorithm.xml");
		int foundChange = buffer.indexOf("<outerLoop maxIterations=\"30\"");
		System.out.println("file changed at pos ="+foundChange);
		System.out.println("Should be file changed at pos =181");
		assertEquals("dudAlgorithm.xml change",foundChange,181);

		// now there are no further changes
		boolean changed = appConf.treeHasChanged();
		System.out.println("appConf.treeHasChanged() = "+changed);
		System.out.println("Should be appConf.treeHasChanged() = false");
		assertEquals("appConf.treeHasChanged()",changed,false);
			}

}
