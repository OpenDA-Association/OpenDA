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


package org.openda.dotnet;

import org.openda.interfaces.IModelState;

import java.io.File;

/**
 * Java wrapper around .net class for a Model State
 */
public class ModelStateN2J implements IModelState {

	private cli.OpenDA.DotNet.Interfaces.IModelState dotNetModelState;

	public ModelStateN2J(cli.OpenDA.DotNet.Interfaces.IModelState dotNetModelState) {
		this.dotNetModelState = dotNetModelState;
	}

	public void savePersistentState(File savedStateFile) {
		dotNetModelState.SavePersistentState(savedStateFile.getAbsolutePath());
	}

	public cli.OpenDA.DotNet.Interfaces.IModelState getDotNetModelState() {
		return dotNetModelState;
	}
}
