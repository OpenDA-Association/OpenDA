package org.openda.fewsworkflow;

import org.openda.model_fews_workflow.io.castorgenerated.FewsWorkflowModelFactoryConfigXML;
import org.openda.utils.io.CastorUtils;

import java.io.File;

public class FewsWorkflowModelFactoryConfigReader {

	private final String modelParametersFile;
	private final String modelResultsFile;

	public FewsWorkflowModelFactoryConfigReader(File configFile) {
		FewsWorkflowModelFactoryConfigXML castor = (FewsWorkflowModelFactoryConfigXML) CastorUtils.parse(configFile, FewsWorkflowModelFactoryConfigXML.class);
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
