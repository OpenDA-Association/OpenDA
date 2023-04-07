/*
* Copyright (c) 2023 OpenDA Association 
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
package org.openda.externalfile;
import org.openda.model_external_file.io.castorgenerated.ExternalFileModelFactoryConfigXML;
import org.openda.utils.io.CastorUtils;

import java.io.File;

public class ExternalFileModelFactoryConfigReader {

	private final String modelParametersFile;
	private final String modelResultsFile;

	public ExternalFileModelFactoryConfigReader(File configFile) {
		ExternalFileModelFactoryConfigXML castor = (ExternalFileModelFactoryConfigXML) CastorUtils.parse(configFile, ExternalFileModelFactoryConfigXML.class);
		modelParametersFile = castor.getModelParametersFile();
		modelResultsFile = castor.getModelResultsFile();
	}

	public String getModelParametersFile() {
		return modelParametersFile;
	}

	public String getModelResultsFile() {
		return modelResultsFile;
	}
}
