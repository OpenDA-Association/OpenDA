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

package org.openda.costa;

import org.openda.interfaces.IModelState;

import java.io.File;
import java.net.FileNameMap;

public class CtaModelState implements IModelState {
	private String ID;
	private int modelHandle;

	public  CtaModelState(String ID, int modelHandle){
		this.ID=ID;
		this.modelHandle=modelHandle;
	}

	public void savePersistentState(File savedStateFile) {
		String AbsFileName = savedStateFile.getAbsolutePath();
		// native code expects a NetCDF file
		String FileName;
		if (AbsFileName.endsWith(".zip")) {
			FileName = AbsFileName.replace(".zip",".nc");
		} else {
			FileName = AbsFileName+".nc";
		}
		nativeSavePersistentState(FileName, this.ID, this.modelHandle);
	}

	private native void nativeSavePersistentState(String FileName, String ID, int modelHandle);
	
	public String getID(){
		return this.ID;
	}

	public int getmodelHandle(){
		return this.modelHandle;
	}
}
