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
import org.openda.core.io.castorgenerated.*;
import org.openda.utils.TreeVector;
import org.openda.utils.Vector;

import java.io.File;


/**
 * Tree Vector Reader
 */
public class TreeVectorReader {

    private File treeVectorFile;

    /**
     * Create a reader for a tree vector xml file
     *
     * @param treeVectorFile The file to be read.
     */
    public TreeVectorReader(File treeVectorFile) {

        if (treeVectorFile == null) {
            throw new IllegalArgumentException("TreeVectorReader: treeVectorFile == null");
        }
        if (!treeVectorFile.exists()) {
            throw new IllegalArgumentException("TreeVectorReader: file does not exist: " +
                    treeVectorFile.getAbsolutePath());
        }
        this.treeVectorFile = treeVectorFile;

    }

    /**
     * Read the tree vector from file
     *
     * @return The read tree vector.
     */
    public ITreeVector readTreeVector() {
        TreeVectorFileXML treeVectorFileXML = (TreeVectorFileXML) CastorUtils.parse(treeVectorFile, TreeVectorFileXML.class);
        TreeVectorXML treeVectorXML = treeVectorFileXML.getTreeVector();
        try {
            return parseTreeVector(null, treeVectorXML);
        } catch (Exception e) {
            throw new RuntimeException("Error creating TreeVector from file " + treeVectorFile.getAbsolutePath() +
                    ": " + e.getMessage());
        }
    }

    public static ITreeVector parseTreeVector(ITreeVector parent, TreeVectorXML treeVectorXML) {

        String treeVectorClassName = "org.utils.simple.TreeVector";
        if (treeVectorXML.getClassName() != null) {
            treeVectorClassName = treeVectorXML.getClassName();
        }
        ITreeVector treeVector;
        if (treeVectorClassName.equals("org.utils.simple.TreeVector")) {
            treeVector = new TreeVector(treeVectorXML.getId(), treeVectorXML.getCaption());
            ((TreeVector)treeVector).setDescription(treeVectorXML.getDescription());
            if (treeVectorXML.hasExcludeFromVector()) {
                ((TreeVector)treeVector).setExcludeFromVector(treeVectorXML.getExcludeFromVector());
            }
        } else if (treeVectorClassName.equals("org.openda.costa.CtaTreeVector")) {
            // TODO: create CtaTreeVector
            System.err.println("CtaTreeVector not yet supported, creating TreeVector)");
            treeVector = new TreeVector(treeVectorXML.getId(), treeVectorXML.getCaption());
        } else {
            throw new RuntimeException("class " + treeVectorClassName + " not (yet) supported");
        }

        TreeVectorXMLChoice[] treeVectorXMLChoices = treeVectorXML.getTreeVectorXMLChoice();
        for (TreeVectorXMLChoice treeVectorXMLChoice : treeVectorXMLChoices) {

            TreeVectorXMLChoiceItem xmlChoiceItem = treeVectorXMLChoice.getTreeVectorXMLChoiceItem();

            TreeVectorLeafXML vectorXML = xmlChoiceItem.getTreeVectorLeaf();
            TreeVectorXML subTreeVectorXML = xmlChoiceItem.getSubTreeVector();

            if (vectorXML != null) {

                double[] values = parseValuesFromSpaceSeparatedString(vectorXML.getVector());

                String vectorClassName = "org.utils.simple.Vector";
                if (vectorXML.getClassName() != null) {
                    vectorClassName = vectorXML.getClassName();
                }
                Vector vector;
                if (vectorClassName.equals("org.utils.simple.Vector")) {
                    vector = new Vector(values);
//                } else if (parent instanceof CtaTreeVector) {
//                    // TODO: create CtaVector
//                    System.err.println("CtaVector not yet supported, creating Vector");
//                    vector = new Vector(values);
                } else {
                    throw new RuntimeException("class " + vectorClassName + " not (yet) supported");
                }
                TreeVector treeVectorLeaf = new TreeVector(vectorXML.getId(), vectorXML.getCaption(), vector);
                ((TreeVector) treeVector).addChild(treeVectorLeaf);
            } else if (subTreeVectorXML != null) {

                ITreeVector subTreeVector = parseTreeVector(treeVector, subTreeVectorXML);
                if (parent != null) {
                    if (parent instanceof TreeVector) {
                        ((TreeVector) parent).addChild(subTreeVector);
                    } else {
                        // TODO if ( parent instanceof CtaTreeVector )
                        throw new RuntimeException("SubTreeVector \"" + subTreeVector.getId() +
                                "\" has unsupported parent type (" + parent.getClass().getName() + ")");
                    }
                }
            }
        }
        return treeVector;
    }

    public static double[] parseValuesFromSpaceSeparatedString(String valuesLine) {
        valuesLine = valuesLine.replace('\n', ' ').replace('\t', ' ').trim();
        String valuesSeparators = "[ ,']+";
        String[] valuesAsString = valuesLine.split(valuesSeparators);
        double[] values = new double[valuesAsString.length];
        for (int i = 0; i < valuesAsString.length; i++) {
            values[i] = Double.valueOf(valuesAsString[i]);
        }
        return values;
    }

}
