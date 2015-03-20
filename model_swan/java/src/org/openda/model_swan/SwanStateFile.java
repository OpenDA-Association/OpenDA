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

package org.openda.model_swan;

import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IPrevExchangeItem;

import java.io.File;
import java.io.IOException;

/**
 * Reading and writing SWAN state file.
 */
public class SwanStateFile implements IDataObject {

    private IExchangeItem[] exchangeItems = new IExchangeItem[1];
    private SwanSpectralFile swanState;
    private String swanStateId = "swanstate";

    protected double[] getAllStateValues() throws IOException {
        double[] dblValues;
        dblValues = swanState.getSpectralValuesAsDouble(0);
        return dblValues;
    }

    protected void setAllStateValues(double[] state){
        swanState.setSpectralValuesAsDouble(0, state);
    }

    protected void axpyOnAllState(double alpha, double[] axpyValues) throws IOException {
        swanState.axpyOnSpectralValues(0, alpha, axpyValues);
    }

    protected void multiplyAllState(double[] alpha) throws IOException {
        swanState.multiplySpectralValues(0,alpha);
    }

    private void initialize(File workingDir, String fileName, String[] arguments) {
        File swnFile = new File(workingDir,arguments[0]);
        File stateFile = new File(workingDir,fileName);
        if (!stateFile.exists()) {
            throw new RuntimeException("State file does not exist: " + stateFile.getAbsolutePath());
        }
        File stateFile1;
        stateFile1 = stateFile;
        if (!swnFile.exists()) {
            throw new RuntimeException("Input file does not exist: " + stateFile.getAbsolutePath());
        }
        File swnFile1 = swnFile;

        this.swanState = new SwanSpectralFile(stateFile1);

        exchangeItems[0]=new SwanStateFileExchangeItem(swanStateId,this);
    }

    public String[] getExchangeItemIDs() {
        return new String[] {exchangeItems[0].getId()};
    }

    public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
        return getExchangeItemIDs();
    }

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		if (!exchangeItemID.equals(swanStateId)) {
			throw new RuntimeException("unknown exchange item: " + exchangeItemID);
		}
		return exchangeItems[0];
	}

    public void finish() {
        swanState.writeSpectralFile();
        // no action (setValues already writes file)
    }

    public void initialize(File workingDir, String[] arguments) {
        String fileName = arguments[0];
        String[] remainingArguments = new String[arguments.length-1];
        System.arraycopy(arguments, 1, remainingArguments, 0, remainingArguments.length);
        initialize(workingDir, fileName, remainingArguments);
    }
}
