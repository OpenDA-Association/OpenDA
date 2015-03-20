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

import org.openda.interfaces.ITreeVector;
import org.openda.interfaces.IVector;
import org.openda.core.io.castorgenerated.*;
import org.openda.utils.TreeVector;

import java.io.File;
import java.util.ArrayList;


/**
 * Tree Vector Writer
 */
public class TreeVectorWriter {

    private File treeVectorFile;

    /**
     * Create a reader for a tree vector xml file
     * @param treeVectorFile The file to be read.
     */
    public TreeVectorWriter(File treeVectorFile) {

        if (treeVectorFile == null) {
            throw new IllegalArgumentException("TreeVectorWriter: treeVectorFile == null");
        }
        File targetDirectory = treeVectorFile.getParentFile();
        if (targetDirectory != null && !targetDirectory.exists()) {
            throw new IllegalArgumentException("TreeVectorWriter: directory " +
                    targetDirectory.getAbsolutePath() + "does not exist");
        }
        this.treeVectorFile = treeVectorFile;
    }

    public void writeTreeVector(ITreeVector treeVector) {
        TreeVectorFileXML treeVectorFileXML = new TreeVectorFileXML();
        treeVectorFileXML.setTreeVector(createTreeVectorXML(treeVector, true));
        CastorUtils.write(treeVectorFileXML, treeVectorFile, "treeVectorFile", null, null);
    }

    private static TreeVectorXMLChoice createSubTreeVectorOrLeaf(ITreeVector treeVector, boolean storeClassNames) {

        TreeVectorXMLChoice treeVectorXMLChoice = new TreeVectorXMLChoice();
        TreeVectorXMLChoiceItem leafOrSubTree = new TreeVectorXMLChoiceItem();
        treeVectorXMLChoice.setTreeVectorXMLChoiceItem(leafOrSubTree);

        ArrayList<String> subTreeVectorIds = treeVector.getSubTreeVectorIds();
        if (subTreeVectorIds.size() == 0 ) {
            // tree vector leaf
            leafOrSubTree.setTreeVectorLeaf(createTreeVectorLeaveXML(treeVector, storeClassNames));
        } else {
            // tree vector
            leafOrSubTree.setSubTreeVector(createTreeVectorXML(treeVector, storeClassNames));
        }
        return treeVectorXMLChoice;
    }

    private static TreeVectorLeafXML createTreeVectorLeaveXML(ITreeVector treeVector, boolean storeClassName) {
        TreeVectorLeafXML treeVectorLeave = new TreeVectorLeafXML();
        String id = treeVector.getId();
        String caption = treeVector.getCaption();
        treeVectorLeave.setId(id);
        treeVectorLeave.setCaption(caption);
		if (storeClassName) {
			treeVectorLeave.setClassName(determineTreeVectorClassName(treeVector));
		}
		if (treeVector.excludeFromVector()) {
            treeVectorLeave.setExcludeFromVector(true);
        }
        String valuesString = createVectorXML(treeVector);
        treeVectorLeave.setVector(valuesString);
        return treeVectorLeave;
    }

	public static TreeVectorXML createTreeVectorXML(ITreeVector treeVector) {
		return createTreeVectorXML(treeVector, false);
}

	public static TreeVectorXML createTreeVectorXML(ITreeVector treeVector, boolean storeClassNames) {
		TreeVectorXML treeVectorXML = new TreeVectorXML();
		treeVectorXML.setId(treeVector.getId());
		treeVectorXML.setCaption(treeVector.getCaption());
		treeVectorXML.setDescription(treeVector.getDescription());
		if (storeClassNames) {
			treeVectorXML.setClassName(determineTreeVectorClassName(treeVector));
		}
		if (treeVector.excludeFromVector()) {
			treeVectorXML.setExcludeFromVector(true);
		}
		for (String subTreeVectorId : treeVector.getSubTreeVectorIds()) {
			treeVectorXML.addTreeVectorXMLChoice(createSubTreeVectorOrLeaf(treeVector.getSubTreeVector(subTreeVectorId), storeClassNames));
		}
		return treeVectorXML;
	}

	public static String createVectorXML(IVector vector) {
        double[] values = vector.getValues();
        String valuesString = "";
        boolean printSpace = false;
        for (double value : values) {
            if (printSpace) {
                valuesString += (",");
            } else {
                printSpace = true;
            }
            valuesString += value;
        }
        return valuesString;
    }

    private static String determineTreeVectorClassName(ITreeVector treeVector) {
        return (treeVector instanceof TreeVector) ? null : treeVector.getClass().getName();
    }
}
