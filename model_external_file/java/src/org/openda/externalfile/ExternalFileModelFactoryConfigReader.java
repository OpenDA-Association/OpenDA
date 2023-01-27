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
