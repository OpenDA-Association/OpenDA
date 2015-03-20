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
import org.openda.interfaces.ITimeInfo;

import java.io.File;
import java.io.IOException;

/**
 * Reading and writing SWAN open boundary file, in spectral format.
 */
public class SwanOpenBoundarySpectralFile implements IDataObject, ITimeInfo {

    private IExchangeItem[] exchangeItems;
    private SwanSpectralFile swanOB;
    private String swanOpenBoundarySpectralFileID = "openboundary";

    protected double[] getAllStateValues(int iTime) throws IOException {
        double[] dblValues;
        dblValues = swanOB.getSpectralValuesAsDouble(iTime);
        return dblValues;
    }

    protected void setAllStateValues(int iTime, double[] state){
        swanOB.setSpectralValuesAsDouble(iTime,state);
    }

    protected void axpyOnAllState(int iTime, double alpha, double[] axpyValues) throws IOException {
        swanOB.axpyOnSpectralValues(iTime,alpha,axpyValues);
    }

    protected void multiplyAllState(int iTime, double[] alpha) throws IOException {
        swanOB.multiplySpectralValues(iTime,alpha);
    }

    public double[] getTimes(){
        return swanOB.getTimesDbl();
    }

	protected Integer getNTimes(){
        return swanOB.getNTimes();
    }

    private void initialize(File workingDir, String fileName, String[] arguments) {
        File obFile = new File(workingDir,fileName);
        if (!obFile.exists()) {
            throw new RuntimeException("Open-boundary file does not exist: " + obFile.getAbsolutePath());
        }

        this.swanOB = new SwanSpectralFile(obFile);

        exchangeItems = new IExchangeItem[1];
        exchangeItems[0]=new SwanOpenBoundarySpectralFileExchangeItem(swanOpenBoundarySpectralFileID,this);
    }

    public String[] getExchangeItemIDs() {
        return new String[] {exchangeItems[0].getId()};
    }

    public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
        return getExchangeItemIDs();
    }

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		if (!exchangeItemID.equals(swanOpenBoundarySpectralFileID)) {
			throw new RuntimeException("unknown exchange item: " + exchangeItemID);
		}
		return exchangeItems[0];
	}

    public void finish() {
        swanOB.writeSpectralFile();
        // no action (setValues already writes file)
    }

    public void initialize(File workingDir, String[] arguments) {
        String fileName = arguments[0];
        String[] remainingArguments = new String[arguments.length-1];
        System.arraycopy(arguments, 1, remainingArguments, 0, remainingArguments.length);
        initialize(workingDir, fileName, remainingArguments);
    }
}
