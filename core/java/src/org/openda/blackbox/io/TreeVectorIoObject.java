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
package org.openda.blackbox.io;
import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.ITreeVector;
import org.openda.utils.Vector;
import org.openda.utils.io.TreeVectorReader;
import org.openda.utils.io.TreeVectorWriter;

import java.io.File;
import java.util.ArrayList;

/**
 * IoObject based on treevector xml-file
 */
public class TreeVectorIoObject implements IoObjectInterface {

    private File treeVectorFile = null;
    private ITreeVector treeVector = null;
    private boolean exportAsOneExchangeItem = false;
    private final String exportAsOneExchangeItemString = "OneExchangeItem";
	IPrevExchangeItem[] exchangeItems = null;

    public void initialize(File workingDir, String fileName, String[] arguments) {
        treeVectorFile = new File(workingDir, fileName);
        if (arguments.length > 0) {
            if (arguments.length != 1) {
                throw new RuntimeException(this.getClass().getName() +
                        ".initialize()expects only one (optional) argument. TreeVector file: " +
                treeVectorFile.getAbsolutePath());
            }
            if(arguments[0].equalsIgnoreCase(exportAsOneExchangeItemString)) {
                exportAsOneExchangeItem = true;
            }
        }
        TreeVectorReader treeVectorReader = new TreeVectorReader(treeVectorFile);
        treeVector = treeVectorReader.readTreeVector();
    }

    public IPrevExchangeItem[] getExchangeItems() {
        if (treeVectorFile == null) {
            throw new RuntimeException(this.getClass().getName() + ": IoObject has not been initialized");
        }
		if (exchangeItems == null) {
			if (exportAsOneExchangeItem) {
				exchangeItems = new IPrevExchangeItem[] {new TreeVectorIoObjectExchangeItem(treeVector)};
			} else {
				ArrayList<String> subTreeVectorIds = treeVector.getSubTreeVectorIds();
				exchangeItems = new IPrevExchangeItem[subTreeVectorIds.size()];
				for (int i = 0, subTreeVectorIdsSize = subTreeVectorIds.size(); i < subTreeVectorIdsSize; i++) {
					String childId = subTreeVectorIds.get(i);
					exchangeItems[i] = new TreeVectorIoObjectExchangeItem(treeVector.getSubTreeVector(childId));
				}
			}
		}
		return exchangeItems;
    }

    public void finish() {
        if (treeVectorFile == null) {
            throw new RuntimeException(this.getClass().getName() + ": IoObject has not been initialized");
        }
		TreeVectorWriter treeVectorWriter = new TreeVectorWriter(treeVectorFile);
		treeVectorWriter.writeTreeVector(treeVector);
    }

	@Override
	public String toString() {
		return treeVectorFile.toString();
	}

    private class TreeVectorIoObjectExchangeItem implements IPrevExchangeItem {

        private ITreeVector treeVector;

        public TreeVectorIoObjectExchangeItem(ITreeVector treeVector) {
            this.treeVector = treeVector;
        }

        public String getId() {
            return treeVector.getId();
        }

        public String getDescription() {
            return treeVector.getDescription();
        }

        public Class getValueType() {
            return double[].class;
        }

        public Role getRole() {
            return IPrevExchangeItem.Role.InOut;
        }

        public Object getValues() {
            return getValuesAsDoubles();
        }

        public double[] getValuesAsDoubles() {
            return treeVector.getValues();
        }

        public void axpyOnValues(double alpha, double[] axpyValues) {
            treeVector.axpy(alpha, new Vector(axpyValues));
        }

        public void multiplyValues(double[] multiplicationFactors) {
            treeVector.pointwiseMultiply(new Vector(multiplicationFactors));
        }

        public void setValues(Object values) {
            if (!(values instanceof double[])) {
                throw new RuntimeException(this.getClass().getName() + ": unexpected values type: "
                        + values.getClass().getName());
            }
            setValuesAsDoubles((double[]) values);
        }

        public void setValuesAsDoubles(double[] values) {
            treeVector.setValues(values);
        }

        public double[] getTimes() {
            return null;
        }

        public void setTimes(double[] times) {
            throw new RuntimeException(this.getClass().getName() + " has no time info");
        }
    }
}
