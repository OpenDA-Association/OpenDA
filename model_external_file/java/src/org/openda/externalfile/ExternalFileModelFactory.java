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
import org.openda.blackbox.interfaces.IModelFactory;
import org.openda.blackbox.interfaces.ITimeHorizonConsumer;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.IStochModelPostProcessor;
import org.openda.interfaces.ITime;

import java.io.File;

public class ExternalFileModelFactory implements IModelFactory, ITimeHorizonConsumer, IStochModelFactory {

	private ExternalFileModelFactoryConfigReader configReader;
	private File exchangeDir;


	@Override
	public ExternalFileModelInstance getInstance(String[] arguments, IStochModelFactory.OutputLevel outputLevel) {
		return new ExternalFileModelInstance(configReader.getModelParametersFile(), configReader.getModelResultsFile(), exchangeDir);
	}

	@Override
	public IStochModelInstance getInstance(OutputLevel outputLevel) {
		return new ExternalFileModelInstance(configReader.getModelParametersFile(), configReader.getModelResultsFile(), exchangeDir);
	}

	@Override
	public IStochModelPostProcessor getPostprocessorInstance(File instanceDir) {
		throw new RuntimeException("org.openda.fewsworkflow.FewsWorkflowModelFactory.getPostprocessorInstance() not implemented yet");

	}

	@Override
	public void finish() {
		// not needed
	}

	@Override
	public void setTimeHorizon(ITime timeHorizon) {
		throw new RuntimeException("org.openda.externalsocket.ExternalModelStochModelFactory.setTimeHorizon() not implemented yet");

	}

	@Override
	public void initialize(File workingDir, String[] arguments) {
		if (arguments == null || arguments.length < 1) {
			throw new IllegalArgumentException(getClass().getSimpleName()
				+ ": First argument should be: relative bmiModelFactoryConfig file path.");
		}
		exchangeDir = workingDir;
		File externalFileModelFactoryConfigFile = new File(workingDir, arguments[0]);
		if (!externalFileModelFactoryConfigFile.exists()) {
			throw new IllegalArgumentException(getClass().getSimpleName() + ": Cannot find externalFileModelFactoryConfig file "
				+ externalFileModelFactoryConfigFile.getAbsolutePath());
		}

		configReader = new ExternalFileModelFactoryConfigReader(externalFileModelFactoryConfigFile);
	}
}
